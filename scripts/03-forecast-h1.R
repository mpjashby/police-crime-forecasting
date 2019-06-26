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

# count crimes per month in each city
crimes_by_month <- crimes %>% 
	mutate(month = yearmonth(date_single)) %>%
	count(city_name, month, name = "crimes") %>%
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
							 between(as_date(month), . - years(3), . - months(1)))
		),
		test_data = map(
			as_date(forecast_date),
			~ filter(crimes_by_month,
							 between(as_date(month), ., . + years(3) - months(1)))
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

future::plan("multiprocess")

system.time(
	models_by_month$models <- furrr::future_map(
		models_by_month$training_data, model,
		naive = NAIVE(crimes ~ lag()),
		tslm = TSLM(crimes ~ trend() + season()),
		# stl_nai = decomposition_model(STL, crimes ~ trend() + season(),
		# 													SNAIVE(season_adjust)),
		stl = decomposition_model(STL, crimes ~ trend() + season(),
															ETS(season_adjust), 
															dcmp_args = list(robust = TRUE)),
		ets = ETS(crimes ~ trend() + season() + error()),
		arima = ARIMA(crimes ~ trend() + season()),
		# var models excluded because they are much worse than the others, which is
		# interesting but makes seeing the relative differences in accuracy between
		# the other models difficult to see on a plot
		# var = VAR(crimes ~ trend() + season() + AR()),
		neural = NNETAR(crimes ~ trend() + season() + AR()),
		fasster = FASSTER(crimes ~ poly(1) + trig(12) + ARMA()),
		# fasster_sea = FASSTER(crimes ~ poly(2) + trig(12) + ARMA()),
		.progress = TRUE
	) %>%
		map(mutate, combo = (arima + ets + fasster + stl) / 4)
)

# models_by_month$models <- models_by_month$models %>%
# 	map(mutate, combo = (arima + ets + fasster + stl) / 4)



# CALCULATE FORECASTS

models_by_month$forecasts <- map(
	models_by_month$models, 
	forecast, h = "3 years", bias_adjust = FALSE, times = 0
) %>% 
	map(function (x) {
		mutate(
			x,
			coef_variation = map_dbl(.distribution, 
															 ~ ifelse(length(.) > 0, .$sd / .$mean, NA))
		)
	})



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



# SAVE MODELS
models_by_month %>% 
	select(forecast_date, training_data, test_data, forecasts, accuracy) %>% 
	write_rds("data_output/models_h1.Rds", compress = "gz")

