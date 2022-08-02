# This file applies each forecasting method to the data from each city



# LOAD DATA --------------------------------------------------------------------

# load data
if (!exists("crimes")) {
	
	crimes <- read_csv("data_output/crime_data.csv.gz") %>% 
		filter(!is.na(date_single))
	
} else {
	
	crimes <- crimes %>% 
		filter(!is.na(date_single))
	
}



# PREPARE DATA -----------------------------------------------------------------

# get holiday dates
holiday_dates <- tibble(
	date = as_date(timeDate::holidayNYSE(2010:2019)),
	holiday = TRUE
)

# count crimes per month in each city
crimes_by_month <- crimes %>% 
	mutate(date = as_date(date_single)) %>%
	count(city_name, date, name = "crimes") %>%
	mutate(
		month = yearmonth(date),
		weekday = !wday(date, label = TRUE) %in% c("Sat", "Sun")
	) %>% 
	left_join(holiday_dates, by = "date") %>% 
	group_by(city_name, month) %>% 
	summarise(
		crimes = sum(crimes), 
		count_weekdays = sum(weekday), 
		count_holidays = sum(holiday, na.rm = TRUE),
		.groups = "drop"
	) %>% 
	as_tsibble(index = month, key = city_name)

# save monthly crime counts for use in journal article figures
write_rds(crimes_by_month, here::here("data_output/monthly_crime_counts.rds"))

# The data should be split into 3-year training datasets and 3-year test 
# datasets based on a sliding window from the start to the end of the data:
# 
# |-- training --|---- test ----|
#  |-- training --|---- test ----|
#   |-- training --|---- test ----|
#    etc …
# 
# To do this, start with a tsibble of dates that form the mid-point between the
# training and test data, which is all the months that are at least three years
# from the start of the data and at least three years from the end. This takes
# the form of a tsibble of dates, which can be populated with training and test
# data for the appropriate periods from the main crimes dataset.
models_by_month <- tsibble(
	forecast_date = yearmonth(
		seq(ymd("2013-01-01"), ymd("2017-01-01"), by = "months")
	), 
	index = forecast_date
) %>% 
	mutate(
		# Data for the 
		training_data = map(
			as_date(forecast_date), 
			~ filter(crimes_by_month, between(as_date(month), . - months(36), . - months(1)))
		),
		test_data = map(
			as_date(forecast_date),
			~ filter(crimes_by_month, between(as_date(month), ., . + months(35)))
		)
	)

# check that the window functions above have worked
# models_by_month %>%
# 	mutate(
# 		training_period = map_chr(
# 			training_data, 
# 			~ str_glue("{first(.$month)} to {last(.$month)} ({length(unique(.$month))} months)")
# 		),
# 		test_period = map_chr(
# 			test_data, 
# 			~ str_glue("{first(.$month)} to {last(.$month)} ({length(unique(.$month))} months)")
# 		)
# 	) %>%
# 	select(-training_data, -test_data) %>%
# 	View()



# RUN MODELS -------------------------------------------------------------------

# Remove the raw crime data because otherwise it causes problems when the 
# `future` package sets up multiple sessions on account of `crimes` being very
# large
rm(crimes)

# Set up parallel processing
future::plan("multisession")

system.time(
	models_by_month$models <- models_by_month %>% 
		pluck("training_data") %>% 
		furrr::future_map(
			model,
			naive = NAIVE(crimes ~ lag()),
			snaive = SNAIVE(crimes ~ lag("year")),
			common = RW(crimes ~ lag(12) + lag(24) + lag(36)),
			tslm = TSLM(crimes ~ trend() + season() + count_weekdays + count_holidays),
			stl = decomposition_model(STL(crimes ~ trend() + season()), ETS(season_adjust)),
			ets = ETS(crimes ~ trend() + season() + error()),
			arima = ARIMA(crimes ~ trend() + season() + count_weekdays + count_holidays),
			var = VAR(crimes ~ trend() + season() + AR()),
			neural = NNETAR(crimes ~ trend() + season() + AR() + count_weekdays + count_holidays),
			fasster = FASSTER(crimes ~ trend() + season() + ARMA() + count_weekdays + count_holidays),
			prophet = prophet(crimes ~ growth() + season("year") + count_weekdays + count_holidays),
			.options = furrr::furrr_options(seed = TRUE),
			.progress = TRUE
		) %>%
		map(mutate, combo = (arima + ets + fasster + stl) / 4)
)



# CALCULATE FORECASTS ----------------------------------------------------------

system.time(
	models_by_month$forecasts <- furrr::future_map2(
		models_by_month$models, 
		models_by_month$forecast_date,
		function (x, y) {
			
			rlang::inform(str_glue("Generating forecasts starting in {format(y, '%b %Y')}\n"))
			
			# generate tsibble of new data for 3 years starting on the forecast date
			new_data <- expand_grid(
				city_name = as.character(unique(x$city_name)),
				date = seq(as_date(y), as_date(y) + months(35), by = "days")
			) %>% 
				mutate(
					month = yearmonth(date),
					weekday = !wday(date, label = TRUE) %in% c("Sat", "Sun")
				) %>% 
				left_join(holiday_dates, by = "date") %>% 
				group_by(city_name, month) %>% 
				summarise(
					count_weekdays = sum(weekday), 
					count_holidays = sum(holiday, na.rm = TRUE),
					.groups = "drop"
				) %>% 
				as_tsibble(index = month, key = city_name)
			
			# forecast based on new data
			# This is very slow for NNETAR() models because prediction intervals are
			# calculated by simulation. Set times = 0 to suppress simulations.
			x %>% 
				forecast(new_data = new_data, times = 0) %>%
				mutate(coef_variation = sqrt(variance(crimes)) / abs(mean(crimes)))
			
		},
		.options = furrr::furrr_options(seed = TRUE),
		.progress = TRUE
	)
)



# CALCULATE ACCURACY MEASURES --------------------------------------------------

models_by_month$accuracy <- pmap(
	list(
		models_by_month$forecasts,
		models_by_month$training_data,
		models_by_month$test_data
	), 
	~ accuracy(
		object = ..1,
		data = bind_rows(..2, ..3),
		measures = list(point_accuracy_measures, distribution_accuracy_measures)
	)
	# ~ left_join(
	# 	# Calculate accuracy measures
	# 	accuracy(object = ..1, data = bind_rows(..2, ..3)),
	# 	# Calculate mean co-efficient of variation
	# 	as_tibble(..1) %>% 
	# 		group_by(city_name, .model) %>% 
	# 		summarise(coef_var = mean(coef_variation), .groups = "drop"),
	# 	by = c("city_name", ".model")
	# )
)



# PORTMANTEAU TESTS ------------------------------------------------------------

models_by_month$portmanteau <- map(models_by_month$models, function (x) {
	x %>% 
		augment() %>% 
		# lag = 10 chosen following https://robjhyndman.com/hyndsight/ljung-box-test/
		features(.resid, portmanteau_tests, lag = 10)
})



# SAVE MODELS ------------------------------------------------------------------
models_by_month %>% 
	# select(-models) %>% 
	# mutate(forecasts = map(forecasts, ~ select(as_tibble(.), -crimes))) %>% 
	write_rds("data_output/models_h1.Rds", compress = "gz")



# SAVE FORECASTS AND ERRORS ----------------------------------------------------

forecasts_by_month <- models_by_month %>% 
	pluck("forecasts") %>% 
	set_names(nm = models_by_month$forecast_date) %>% 
	map_dfr(as_tibble, .id = "forecast_date")

models_by_month %>% 
	pluck("test_data") %>% 
	set_names(nm = models_by_month$forecast_date) %>% 
	map_dfr(as_tibble, .id = "forecast_date") %>% 
	select(city_name, forecast_date, month, actual = crimes) %>% 
	right_join(
		forecasts_by_month, 
		by = c("forecast_date", "city_name", "month")
	) %>% 
	mutate(forecast_date = ym(forecast_date)) %>% 
	select(
		city_name, 
		forecast_date, 
		model = .model, 
		month, 
		actual, 
		forecast = .median
	) %>% 
	write_rds(here::here("data_output/models_h1_forecasts.Rds"), compress = "gz")



# WRITE SUMMARY ----------------------------------------------------------------
models_by_month %>% 
	as_tibble() %>% 
	select(forecast_date, accuracy) %>% 
	unnest(cols = "accuracy") %>% 
	select(forecast_date, model = .model, city_name, mape = MAPE) %>%
	write_rds("data_output/models_h1_summary.Rds", compress = "gz")
