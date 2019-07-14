# This file applies each forecasting method to the data from each city



# LOAD DATA

# load data
if (!exists("crimes")) {
	
	crimes <- read_csv("data_output/crime_data.csv.gz") %>% 
		filter(!is.na(date_single))
	
} else {
	
	crimes <- crimes %>% 
		filter(!is.na(date_single))
	
}



# PREPARE DATA

# get holiday dates
holiday_dates <- tibble(
	date = as_date(timeDate::holidayNYSE(2010:2018)),
	holiday = TRUE
)

# count crimes per month in each city
crimes_by_month <- crimes %>% 
	mutate(date = as_date(date_single)) %>%
	count(city_name, date, name = "crimes") %>%
	mutate(
		month = yearmonth(date),
		weekday = ifelse(!wday(date, TRUE) %in% c("Sat", "Sun"), TRUE, FALSE)
	) %>% 
	left_join(holiday_dates, by = "date") %>% 
	group_by(city_name, month) %>% 
	summarise(crimes = sum(crimes), count_weekdays = sum(weekday), 
						count_holidays = sum(holiday, na.rm = TRUE)) %>% 
	ungroup() %>% 
	as_tsibble(index = month, key = city_name)

# create tsibble to hold model results and separate data into training/test sets
models_by_month <- tsibble(
	forecast_date = yearmonth(seq.Date(ymd("2013-01-01"), ymd("2015-12-31"), 
																		 by = "months")), 
	index = forecast_date
) %>% 
	mutate(
		training_data = map(
			as_date(forecast_date), 
			~ filter(crimes_by_month, 
							 between(as_date(month), . - months(36), .))
		),
		test_data = map(
			as_date(forecast_date),
			~ filter(crimes_by_month,
							 between(as_date(month), . + months(1), . + years(3)))
		)
	)

# check that the window functions above have worked
# models_by_month %>%
# 	mutate(
# 		training_period = map_chr(training_data, function (x) {
# 			paste(first(x$month), "to", last(x$month))
# 		}),
# 		test_period = map_chr(test_data, function (x) {
# 			paste(first(x$month), "to", last(x$month))
# 		})
# 	) %>%
# 	select(-training_data, -test_data) %>%
# 	View()



# RUN MODELS

# future::plan("multiprocess")

system.time(
	models_by_month$models <- furrr::future_map(
		models_by_month$training_data, model,
		naive = NAIVE(crimes ~ lag()),
		snaive = SNAIVE(crimes ~ lag("year")),
		# common1 = RW(crimes ~ lag(12)),
		# common2 = RW(crimes ~ lag(24)),
		# common3 = RW(crimes ~ lag(36)),
		tslm = TSLM(crimes ~ trend() + season() + count_weekdays + count_holidays),
		stl = decomposition_model(STL, crimes ~ trend() + season(),
															ETS(season_adjust), 
															dcmp_args = list(robust = TRUE)),
		ets = ETS(crimes ~ trend() + season() + error()),
		arima = ARIMA(crimes ~ trend() + season() + count_weekdays + 
										count_holidays),
		# var models excluded because they are much worse than the others, which is
		# interesting but makes seeing the relative differences in accuracy between
		# the other models difficult to see on a plot
		# var = VAR(crimes ~ trend() + season() + AR()),
		neural = NNETAR(crimes ~ trend() + season() + AR() + count_weekdays + 
											count_holidays),
		fasster = FASSTER(crimes ~ poly(1) + trig(12) + ARMA() + count_weekdays + 
												count_holidays),
		prophet = prophet(crimes ~ growth() + season("year") + count_weekdays + 
												count_holidays),
		.progress = TRUE
	) %>%
		map(mutate, combo = (arima + ets + fasster + stl) / 4)
				# common = (common1 + common2 + common3) / 3)
		# map(mutate, common1 = NULL, common2 = NULL, common3 = NULL)
)



# CALCULATE FORECASTS

system.time(
	models_by_month$forecasts <- furrr::future_map2(
		models_by_month$models, models_by_month$forecast_date,
		function (x, y) {
			
			# generate tsibble of new data for 90 days starting on the forecast date
			new_data <- expand.grid(
				city_name = as.character(unique(x$city_name)),
				date = seq.Date(as_date(y), as_date(y) + years(3) - months(1), 
												by = "days")
			) %>% 
				mutate(
					month = yearmonth(date),
					weekday = ifelse(!wday(date, TRUE) %in% c("Sat", "Sun"), TRUE, FALSE)
				) %>% 
				left_join(holiday_dates, by = "date") %>% 
				group_by(city_name, month) %>% 
				summarise(count_weekdays = sum(weekday), 
									count_holidays = sum(holiday, na.rm = TRUE)) %>% 
				ungroup() %>% 
				as_tsibble(index = month, key = city_name)
			
			# forecast based on new data
			# This is very slow for NNETAR() models because prediction intervals are
			# calculated by simulation. Set times = 0 to suppress simulations.
			forecast(x, new_data = new_data, bias_adjust = FALSE) %>% 
				mutate(coef_variation = map_dbl(.distribution, coef_var))
			
		},
		.progress = TRUE
	)
)



# CALCULATE ACCURACY MEASURES

models_by_month$accuracy <- pmap(
	list(models_by_month$forecasts, models_by_month$training_data, 
			 models_by_month$test_data), 
	# ~ accuracy(..1, rbind(..2, ..3))
	~ left_join(
		accuracy(..1, rbind(..2, ..3)),
		as_tibble(..1) %>% 
			group_by(city_name, .model) %>% 
			summarise(coef_var = mean(coef_variation)) %>% 
			ungroup(),
		by = c("city_name", ".model")
	)
)



# PORTMANTEAU TESTS

models_by_month$portmanteau <- map(models_by_month$models, function (x) {
	x %>% 
		augment(type = "response") %>% 
		# lag = 10 chosen following https://robjhyndman.com/hyndsight/ljung-box-test/
		features(.resid, portmanteau_tests, lag = 10)
})



# SAVE MODELS
models_by_month %>% 
	select(-models) %>% 
	mutate(forecasts = map(forecasts, ~select(as_tibble(.), -.distribution))) %>% 
	write_rds("data_output/models_h1.Rds", compress = "gz")

