# This file applies each forecasting method for H3 to the data from each city

if (!isNamespaceLoaded("tidyverse")) {
	source(here::here("scripts/00-initialise.R"))
}



# LOAD DATA --------------------------------------------------------------------

if (!exists("crimes")) {
	
	crimes <- read_csv("data_output/crime_data.csv.gz") %>% 
		filter(!is.na(date_single)) %>% 
		# District boundaries are not available for KCMO
		filter(city_name != "Kansas City")
	
}



# PREPARE DATA -----------------------------------------------------------------

# Get holiday dates
holiday_dates <- tibble(
	date = as_date(timeDate::holidayNYSE(2010:2019)),
	holiday = TRUE
)

# Report how many crimes could not be matched to a district
crimes %>% 
	mutate(has_district = !is.na(district)) %>% 
	count(city_name, has_district) %>% 
	spread(has_district, n) %>% 
	mutate(prop_without_district = scales::percent(`FALSE` / (`TRUE` + `FALSE`)))

# Count crimes per year in each district
crimes_by_month <- crimes %>% 
	filter(!is.na(district)) %>% 
	# Create a combined variable for city and district, and extract crime 
	# categories of interest. We will only keep aggravated assaults to keep
	# computational time reasonable. Agg assaults have been chosen because there
	# are reasonable numbers (i.e. not too few) of offences in all cities.
	mutate(
		date = as_date(date_single),
		offense = case_when(
			offense_type  == "aggravated assault" ~ "aggravated assault",
			# offense_group == "arson" ~ "arson",
			# offense_group == "burglary/breaking & entering" ~ "burglary",
			# offense_group == "homicide offenses" ~ "homicide",
			# offense_group == "larceny/theft offenses" ~ "theft",
			# offense_type  == "rape (except statutory rape)" ~ "rape",
			# offense_group == "robbery" ~ "robbery",
			# offense_group == "motor vehicle theft" ~ "vehicle theft",
			TRUE ~ "other"
		)
	) %>% 
	# Choose one district from each city except NYC. This was done randomly using
	# `round(runif(1, 1, n))` where `n` is the number of districts in each city,
	# excluding special districts with very little crime (such as airports). The
	# chosen district in each case is the nth district alphabetically, rather than
	# the district with that number in cities that have numbered rather than named
	# districts. Since we don't have district boundaries for Kansas City, two
	# districts from NYC are chosen to keep the overall number of forecasts the
	# same across Scenarios.
	filter(
		(
			(city_name == "Austin" & district == "BAKER") |
				(city_name == "Chicago" & district == "19") |
				(city_name == "Detroit" & district == "08") |
				(city_name == "Los Angeles" & district == "DEVONSHIRE") |
				(city_name == "Louisville" & district == "5") |
				(city_name == "Memphis" & district == "Tillman") |
				(city_name == "New York" & district %in% c("034", "067")) |
				(city_name == "San Francisco" & district == "NORTHERN") |
				(city_name == "Seattle" & district == "SW") |
				(city_name == "St Louis" & district == "6") |
				(city_name == "Tucson" & district == "Operations Division South")
		),
		offense != "other"
	) %>% 
	count(city_name, district, offense, date, name = "crimes") %>% 
	{
		# This code is needed to fill gaps in the daily data for dates with no
		# offences in a particular district for a particular type. 
		# tsibble::fill_gaps() can't be used for this because it doesn't add missing
		# dates at the start or end of the series
		temp <- .
		expand_grid(
			group = unique(
				paste(temp$city_name, temp$district, temp$offense, sep = " | ")
			),
			date = seq.Date(min(temp$date), max(temp$date), by = "day")
		) %>% 
			separate(
				group, 
				into = c("city_name", "district", "offense"), 
				sep = " \\| "
			) %>% 
			left_join(temp, by = c("city_name", "district", "offense", "date"))
	} %>% 
	# The empty cases will have crimes == NA, so replace this with crimes == 0
	replace_na(list(crimes = 0)) %>% 
	mutate(
		month = yearmonth(date),
		weekday = !wday(date, label = TRUE) %in% c("Sat", "Sun")
	) %>%
	left_join(holiday_dates, by = "date") %>%
	group_by(city_name, district, offense, month) %>%
	summarise(
		crimes = as.integer(sum(crimes)), 
		count_weekdays = sum(weekday),
		count_holidays = sum(holiday, na.rm = TRUE),
		.groups = "drop"
	) %>%
	as_tsibble(index = month, key = c(city_name, district, offense))
	
# Create tsibble to hold model results and separate data into training/test sets
models_by_month <- tsibble(
	forecast_date = yearmonth(
		seq(ymd("2013-01-01"), by = "months", length.out = 48)
	), 
	index = forecast_date
) %>% 
	mutate(
		training_data = map(
			as_date(forecast_date), 
			~ filter(crimes_by_month, between(as_date(month), . - months(36), . - months(1)))
		),
		test_data = map(
			as_date(forecast_date),
			~ filter(crimes_by_month, between(as_date(month), ., . + months(11)))
		)
	)
	


# SET UP PARALLEL PROCESSING

future::plan("multisession")

# remove crimes object because it is too big to copy to each parallel instance
rm(crimes, crimes_by_month)



# RUN MODELS -------------------------------------------------------------------

system.time(
	models_by_month$models <- furrr::future_map2(
		models_by_month$training_data,
		models_by_month$forecast_date,
		function (x, y) {
			model(
				x, 
				naive = NAIVE(crimes ~ lag()),
				snaive = SNAIVE(crimes ~ lag("year")),
				common = RW(crimes ~ lag(12) + lag(24) + lag(36)),
				tslm = TSLM(crimes ~ trend() + season() + count_weekdays + count_holidays),
				stl = decomposition_model(STL(crimes ~ trend() + season()), ETS(season_adjust)),
				ets = ETS(crimes ~ trend() + season() + error()),
				arima = ARIMA(crimes ~ trend() + season() + count_weekdays + count_holidays),
				neural = NNETAR(crimes ~ trend() + season() + AR() + count_weekdays + count_holidays),
				fasster = FASSTER(crimes ~ trend() + season() + ARMA() + count_weekdays + count_holidays),
				prophet = prophet(crimes ~ growth() + season("year") + count_weekdays + count_holidays)
			) %>% 
				mutate(combo = (arima + ets + fasster + stl) / 4)
		},
		.options = furrr::furrr_options(seed = TRUE),
		.progress = TRUE
	)
)



# CALCULATE FORECASTS ----------------------------------------------------------

system.time(
	models_by_month$forecasts <- furrr::future_map2(
		models_by_month$models,
		models_by_month$test_data,
		~ forecast(.x, new_data = select(.y, -crimes)),
		.options = furrr::furrr_options(seed = TRUE),
		.progress = TRUE
	)
)



# CALCULATE ACCURACY MEASURES --------------------------------------------------

system.time(
	models_by_month$accuracy <- furrr::future_pmap(
		list(
			models_by_month$forecasts[1], 
			models_by_month$training_data[1],
			models_by_month$test_data[1]
		),
		~ accuracy(
			object = ..1,
			data = bind_rows(..2, ..3),
			measures = list(point_accuracy_measures, distribution_accuracy_measures)
		),
		.progress = TRUE
	)
)



# SAVE MODELS ------------------------------------------------------------------

models_by_month %>% 
	# select(-models) %>%
	# mutate(forecasts = map(forecasts, ~select(as_tibble(.), -.distribution))) %>%
	write_rds("data_output/models_h3.Rds", compress = "gz")



# SAVE FORECASTS AND ERRORS ----------------------------------------------------

forecasts_by_date <- models_by_date |> 
	pluck("forecasts") |> 
	set_names(nm = models_by_date$forecast_date) |> 
	map_dfr(as_tibble, .id = "forecast_date")

models_by_date %>% 
	pluck("test_data") %>% 
	set_names(nm = models_by_date$forecast_date) %>% 
	map_dfr(as_tibble, .id = "forecast_date") %>% 
	select(city_name, forecast_date, month, actual = crimes) %>% 
	right_join(
		forecasts_by_date, 
		by = c("forecast_date", "city_name", "month")
	) %>% 
	mutate(forecast_date = ym(forecast_date)) %>% 
	select(
		city_name, 
		forecast_date, 
		model = .model, 
		month, 
		actual, 
		forecast = .mean
	) %>% 
	write_rds(here::here("data_output/models_h3_forecasts.Rds"), compress = "gz")

