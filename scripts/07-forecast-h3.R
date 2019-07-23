# This file applies each forecasting method for H3 to the data from each city


if (!isNamespaceLoaded("tidyverse")) {
	source(here::here("scripts/00-initialise.R"))
}


# LOAD DATA

if (!exists("crimes")) {
	
	crimes <- read_csv("data_output/crime_data.csv.gz") %>% 
		filter(!is.na(date_single))
	
}

if (!exists("event_dates")) {
	event_dates <- read_csv("data_output/event_data.csv.gz")
}

if (!exists("weather_dates")) {
	weather_dates <- read_csv("data_output/weather_data.csv.gz")
}



# PREPARE DATA

# get range of dates for crime data
date_range <- lubridate::interval(
	as_datetime(min(crimes$date_single)), 
	as_datetime(max(crimes$date_single))
)

# get holiday dates
holiday_dates <- tibble(
	date = as_date(timeDate::holidayNYSE(2010:2018)),
	holiday = TRUE
)

# get Black Friday dates
black_fridays <- holiday_dates %>% 
	filter(month(date) == 11) %>% 
	select(date) %>% 
	mutate(
		date = date + days(1), 
		black_friday = TRUE
	)

# record how many crimes occur at exactly midnight
crimes %>% 
	filter(city_name != "Fort Worth") %>% 
	filter(hour(date_single) == 0 & minute(date_single) == 0) %>% 
	nrow() %>% 
  format(big.mark = ",")
	
# count crimes per shift in each city
crimes_by_shift <- crimes %>% 
	# Fort Worth data only have dates, not times
	filter(city_name != "Fort Worth") %>% 
	# filter out crimes that occur exactly at midnight
	filter(!hour(date_single) == 0 & !minute(date_single) == 0) %>% 
	# round timestamps to eight-hour periods starting at midnight (midnight starts
	# are common in US police departments)
	mutate(date = floor_date(date_single, "8 hours")) %>% 
	count(city_name, date, name = "crimes") %>% 
	# filter out any crimes occurring outside the dates of interest
	filter(date %within% date_range) %>%
	# convert to tsibble, which is needed to run fill_gaps()
	as_tsibble(index = date, key = city_name) %>% 
	# fill any gaps in the time series, which can cause problems with some models
	fill_gaps() %>% 
	# convert back to tibble, since tsibble causes issues with e.g. mean()
	as_tibble() %>% 
	# create an extra date-only variable for joining
	mutate(date_round = as_date(date)) %>% 
	# join event dates
	left_join(holiday_dates, by = c("date_round" = "date")) %>% 
	left_join(black_fridays, by = c("date_round" = "date")) %>% 
	left_join(event_dates, by = c("date_round" = "date", "city_name")) %>% 
	left_join(weather_dates, 
						by = c("date_round" = "date", "city_name" = "city")) %>% 
	mutate(
		# replace NAs resulting from joins with FALSE
		holiday = ifelse(is.na(holiday), FALSE, holiday),
		black_friday = ifelse(is.na(black_friday), FALSE, black_friday),
		# add notable dates that aren't holidays
		new_years_eve = ifelse(month(date) == 12 & mday(date) == 31, TRUE, FALSE),
		halloween = ifelse(month(date) == 10 & mday(date) == 31, TRUE, FALSE),
		# add first dates dummies
		month_first = mday(date_round) == 1,
		year_first = yday(date_round) == 1,
		# impute crime count with mean of counts for the same shift for a seven-day
		# period centred on the missing day
		crimes = as.integer(ifelse(
			is.na(crimes),
			round(mean(c(lag(crimes, 3), lag(crimes, 6), lag(crimes, 9), 
									 lead(crimes, 3), lead(crimes, 6), lead(crimes, 9)), 
								 na.rm = TRUE)),
			crimes
		))
	) %>% 
	select(-date_round) %>% 
	# convert back to tsibble again
	as_tsibble(index = date, key = city_name)

# check for gaps in the time series, which can cause problems with the models,
# although by this point in the code there shouldn't be any
scan_gaps(crimes_by_shift)

# split data for each model
models_by_shift <- tibble(
	forecast_date = seq.POSIXt(
		as_datetime(min(crimes$date_single) + years(3)), 
		as_datetime(max(crimes$date_single) - days(2)), 
		by = "days"
	)
) %>% 
	sample_n(100, replace = FALSE) %>% 
	arrange(forecast_date) %>% 
	mutate(
		training_data = map(
			forecast_date, 
			~ filter(crimes_by_shift, between(date, . - years(3), . - hours(1)))
		),
		test_data = map(
			forecast_date,
			~ filter(crimes_by_shift, between(date, ., . + hours(23)))
			# ~ filter(crimes_by_shift, between(date, . + hours(24), . + hours(47)))
		)
	)

# check if models_by_shift has the correct format
# models_by_shift %>%
# 	mutate(
# 		training_period = map_chr(training_data, function (x) {
# 			paste(first(x$date), "to", last(x$date))
# 		}),
# 		test_period = map_chr(test_data, function (x) {
# 			paste(first(x$date), "to", last(x$date))
# 		})
# 	) %>%
# 	select(training_period, forecast_date, test_period) %>%
# 	View()



# RUN MODELS

system.time(
	models_by_shift$models <- map(
		models_by_shift$training_data[1], function (x) {
			map(as.character(unique(x$city_name)), function (y) {
				
				training_data <- x %>% 
					filter(city_name == y) %>% 
					# group by city_name because it is constant so would otherwise be
					# unselected by !is_constant()
					group_by(city_name) %>% 
					select_if(~ !is_constant(.)) %>% 
					ungroup()
					# select_if(function (x) {
					# 	ifelse(
					# 		is.logical(x),
					# 		!isTRUE(all.equal(x, rep(FALSE, length(x)))),
					# 		TRUE
					# 	)
					# }) %>% 
					# remove_missing(na.rm = TRUE) %>% 
					# as_tsibble(key = city_name, index = date)
				
				xreg_vars <- training_data %>%
					as_tibble() %>%
					select(-city_name, -date, -crimes) %>%
					names()

				message("\nRetaining variables ", paste(xreg_vars, collapse = ", "),
								" and key ", key(training_data), " for ", 
								first(training_data$city_name), appendLF = TRUE)

				model(
					training_data,
					# naive = NAIVE(crimes ~ lag()),
					snaive = SNAIVE(crimes ~ lag()),
					tslm = TSLM(as.formula(paste(c("crimes ~ trend() + season()", xreg_vars), collapse = " + "))),
					stl = decomposition_model(STL, crimes ~ trend() + season(), ETS(season_adjust), ETS(season_year), dcmp_args = list(robust = TRUE)),
					ets = ETS(crimes ~ trend() + season() + error()),
					arima = ARIMA(as.formula(paste(c("crimes ~ trend() + season()", xreg_vars), collapse = " + "))),
					# neural = NNETAR(as.formula(paste(c("crimes ~ trend() + season() + AR()", xreg_vars), collapse = " + ")), MaxNWts = 2000, scale_inputs = FALSE),
					fasster = FASSTER(as.formula(paste(c("crimes ~ poly(1) + trig(7) + ARMA()", xreg_vars), collapse = " + "))),
					prophet = prophet(as.formula(paste(c("crimes ~ growth() + season('year') + season('week')", xreg_vars), collapse = " + ")))
				) %>%
					mutate(combo = (arima + tslm) / 2)
				
			})
		})
)



# CALCULATE FORECASTS

system.time(
	models_by_shift$forecasts <- map2(
		# models_by_shift$models[1], as_datetime(models_by_shift$forecast_date[1]),
		models_by_shift$models[1], models_by_shift$test_data[1],
		function (x, y) {
			
			# new_data <- expand.grid(
			# 	city_name = map_chr(x, ~ .$city_name),
			# 	date = seq.POSIXt(y, y + hours(16), by = "8 hours")
			# ) %>% 
			# 	arrange(city_name, date) %>% 
			# 	mutate(date_round = as_date(date)) %>% 
			# 	left_join(holiday_dates, by = c("date_round" = "date")) %>% 
			# 	left_join(black_fridays, by = c("date_round" = "date")) %>% 
			# 	left_join(event_dates, by = c("date_round" = "date", "city_name")) %>% 
			# 	left_join(weather_dates, 
			# 						by = c("date_round" = "date", "city_name" = "city")) %>% 
			# 	mutate(
			# 		holiday = ifelse(is.na(holiday), FALSE, holiday),
			# 		black_friday = ifelse(is.na(black_friday), FALSE, black_friday),
			# 		new_years_eve = ifelse(month(date) == 12 & mday(date) == 31, TRUE, FALSE),
			# 		halloween = ifelse(month(date) == 10 & mday(date) == 31, TRUE, FALSE),
			# 		month_first = mday(date_round) == 1,
			# 		year_first = yday(date_round) == 1
			# 	) %>% 
			# 	select(-date_round) %>% 
			# 	as_tsibble(index = date, key = city_name)
			
			map(x, function (z) {
				
				# if (!z$city_name %in% c("Los Angeles", "San Francisco")) {
				
				message(z$city_name)
				
				# this_new_data <- new_data %>%
				# 	filter(city_name == z$city_name) %>%
				# 	select_at(vars(c("city_name", "date", training_vars(z))))
				
				this_new_data <- y %>% 
					filter(city_name == z$city_name) %>%
					select_at(vars(c("city_name", "date", training_vars(z))))
				
				# forecast based on new data
				# This is very slow for NNETAR() models because prediction intervals are
				# calculated by simulation. Set times = 0 to suppress simulations.	
				forecast(z, new_data = this_new_data, bias_adjust = FALSE) %>%
					mutate(coef_variation = map_dbl(.distribution, coef_var))

			})
			
		})

)



# CALCULATE THRESHOLD VALUES
models_by_shift$thresholds <- map(
	models_by_shift$training_data[1], function (x) {
		
		x %>% 
			as_tibble() %>% 
			mutate(shift = case_when(hour(date) < 8 ~ "midnight", 
															 hour(date) < 16 ~ "daytime", 
															 TRUE ~ "evening")) %>% 
			group_by(city_name, shift) %>% 
			summarise(threshold = quantile(crimes, 1 - (12/365.25))) %>% 
			ungroup()
		
	})



# CALCULATE PROBABILTY OF SHIFT BEING EXTREME
models_by_shift$forecasts <- map2(
	models_by_shift$forecasts[1], models_by_shift$thresholds[1],
	function (x, y) {
		
		map(x, function (z) {
			
			z <- z %>% 
				mutate(shift = case_when(hour(date) < 8 ~ "midnight", 
																 hour(date) < 16 ~ "daytime", 
																 TRUE ~ "evening")) %>% 
				left_join(y, by = c("city_name", "shift")) %>% 
				mutate(prob_extreme = prob_extreme(.distribution, threshold))
			
		})
		
	})



# SAVE MODELS
models_by_shift %>% 
	select(-models) %>% 
	mutate(forecasts = map_depth(forecasts, 2, 
															 ~select(as_tibble(.), -.distribution))) %>% 
	write_rds("data_output/models_h3.Rds", compress = "gz")

