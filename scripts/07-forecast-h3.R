# This file applies each forecasting method for H3 to the data from each city


if (!isNamespaceLoaded("tidyverse")) {
	source(here::here("scripts/00-initialise.R"))
}



# LOAD DATA

if (!exists("crimes")) {
	crimes <- read_csv("data_output/crime_data.csv.gz") %>% 
		filter(!is.na(date_single)) %>% 
		# district boundaries are not available for KCMO
		filter(city_name != "Kansas City")
}



# PREPARE DATA

# get holiday dates
holiday_dates <- tibble(
	date = as_date(timeDate::holidayNYSE(2010:2018)),
	holiday = TRUE
)

# report how many crimes could not be matched to a district
crimes %>% 
	mutate(has_district = !is.na(district)) %>% 
	count(city_name, has_district) %>% 
	spread(has_district, n) %>% 
	mutate(prop_without_district = scales::percent(`FALSE` / (`TRUE` + `FALSE`)))

# count crimes per year in each district
crimes_by_month <- crimes %>% 
	filter(!is.na(district)) %>% 
	# create a combined variable for city and district
	mutate(
		city_district = paste(city_name, str_to_title(district)),
		date = as_date(date_single)
	) %>% 
	# remove crimes at Austin airport, which are rare
	# remove crimes occurring in exclaves of Chicago, represented by Chicago 
	# Police 31st District, which doesn't appear to otherwise exist
	# remove Louisville 7th District, which has almost no crimes
	filter(!city_district %in% c("Austin APT", "Chicago 31", "Louisville 7")) %>% 
	count(city_name, city_district, date, name = "crimes") %>%
	mutate(
		month = yearmonth(date),
		weekday = ifelse(!wday(date, TRUE) %in% c("Sat", "Sun"), TRUE, FALSE)
	) %>% 
	left_join(holiday_dates, by = "date") %>% 
	group_by(city_name, city_district, month) %>% 
	summarise(crimes = sum(crimes), count_weekdays = sum(weekday), 
						count_holidays = sum(holiday, na.rm = TRUE)) %>% 
	ungroup() %>% 
	as_tsibble(index = month, key = city_district) %>% 
	fill_gaps(crimes = 0)

# create tsibble to hold model results and separate data into training/test sets
models_by_month <- tsibble(
	forecast_date = yearmonth(seq.Date(ymd("2015-01-01"), ymd("2017-12-31"), 
																		 by = "months")),
	index = forecast_date
) %>% 
	mutate(
		training_data = map(
			as_date(forecast_date), 
			~ filter(crimes_by_month, 
							 between(as_date(month), . - months(12 * 5), .))
		),
		test_data = map(
			as_date(forecast_date),
			~ filter(crimes_by_month,
							 between(as_date(month), . + months(1), . + years(1)))
		)
	)



# RUN MODELS

system.time({
	models_by_month$models <- map(
		models_by_month$training_data[1], model, 
		naive = NAIVE(crimes ~ lag()),
		snaive = SNAIVE(crimes ~ lag("year")),
		tslm = TSLM(crimes ~ trend() + season() + count_weekdays + count_holidays),
		stl = decomposition_model(STL, crimes ~ trend() + season(), 
															ETS(season_adjust), 
															dcmp_args = list(robust = TRUE)),
		ets = ETS(crimes ~ trend() + season() + error()),
		arima = ARIMA(crimes ~ trend() + season() + count_weekdays + 
										count_holidays),
		neural = NNETAR(crimes ~ trend() + season() + AR() + count_weekdays + 
											count_holidays),
		fasster = FASSTER(crimes ~ poly(1) + trig(12) + ARMA() + count_weekdays + 
												count_holidays),
		prophet = prophet(crimes ~ growth() + season("year") + count_weekdays + 
												count_holidays)
	) %>% 
		map(mutate, combo = (arima + ets + fasster + stl) / 4)

	slackr_bot("Finished estimating models for H3")
})



# CALCULATE FORECASTS

system.time({
	models_by_month$forecasts <- map2(
		models_by_month$models[1], models_by_month$forecast_date[1],
		function (x, y) {
			
			# generate tsibble of new data for 90 days starting on the forecast date
			new_data <- expand.grid(
				city_district = as.character(unique(x$city_district)),
				date = seq.Date(as_date(y), as_date(y) + years(3) - months(1),
												by = "days")
			) %>%
			mutate(
				month = yearmonth(date),
				weekday = !wday(date, TRUE) %in% c("Sat", "Sun")
			) %>%
			left_join(holiday_dates, by = "date") %>%
			group_by(city_district, month) %>%
			summarise(count_weekdays = sum(weekday),
								count_holidays = sum(holiday, na.rm = TRUE)) %>%
			ungroup() %>%
			as_tsibble(index = month, key = city_district)

			# forecast based on new data
			# This is very slow for NNETAR() models because prediction intervals are
			# calculated by simulation. Set times = 0 to suppress simulations.
			forecast(x, new_data = new_data, bias_adjust = FALSE) %>%
				mutate(coef_variation = map_dbl(.distribution, coef_var))
			
		}
	)
	
	slackr_bot("Finished estimating forecasts for H3")
})



# GENERATE DRAWS

system.time({
	test_generate <- generate(
		select(models_by_month$models[[1]], city_district, naive, snaive, tslm, ets, 
					 neural),
		select(models_by_month$test_data[[1]], -crimes)
	)
	
	slackr_bot("Finished generating draws for H3")
})



# SAVE MODELS
models_by_shift %>% 
	select(-models) %>% 
	mutate(forecasts = map_depth(forecasts, 2, 
															 ~select(as_tibble(.), -.distribution))) %>% 
	write_rds("data_output/models_h3.Rds", compress = "gz")

