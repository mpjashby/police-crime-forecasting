# This file runs the analysis to test H2, generating forecasts for each city
# using each method



# SET SEED
# since we will be using sample_n() to choose the dates on which to make the
# forecasts, we can set a seed here to make the sample reproducible
set.seed(123)



# LOAD DATA

if (!exists("crimes")) {
	crimes <- read_csv("data_output/crime_data.csv.gz") %>% 
		filter(!is.na(date_single))
}

if (!exists("event_dates")) {
	event_dates <- read_csv("data_output/event_data.csv.gz")
}



# PREPARE DATA

# get range of dates for crime data
date_range <- lubridate::interval(
	as_date(min(crimes$date_single)), 
	as_date(max(crimes$date_single))
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
	mutate(date = date + days(1), black_friday = TRUE)

# count crimes per day
crimes_by_date <- crimes %>% 
	# move early-hour crimes to one day previously, so they are counted with the
	# night shift of the previous day
	mutate(
		date = as_date(date_single),
		date = as_date(ifelse(hour(date_single) < 6, date - days(1), date))
	) %>% 
	count(city_name, date, name = "crimes") %>%
	# some crimes will now be recorded on the last day of the year before the 
	# start of the study period, so we remove these
  filter(date %within% date_range) %>%
	left_join(holiday_dates, by = "date") %>% 
	left_join(black_fridays, by = "date") %>% 
	left_join(event_dates, by = c("date", "city_name")) %>% 
	mutate(
		holiday = ifelse(is.na(holiday), FALSE, holiday),
		black_friday = ifelse(is.na(black_friday), FALSE, black_friday),
		halloween = ifelse(month(date) == 10 & mday(date) == 31, TRUE, FALSE),
		month_last = mday(date + days(1)) == 1,
		year_last = yday(date + days(1)) == 1
	) %>%
	as_tsibble(index = date, key = city_name)

models_by_date <- tibble(
	forecast_date = seq.Date(
		as_date(min(crimes$date_single) + years(3)), 
		as_date(max(crimes$date_single) - days(90)), 
		by = "days"
	)
) %>% 
	sample_n(100, replace = FALSE) %>% 
	arrange(forecast_date) %>% 
	mutate(
		training_data = map(
			forecast_date, 
			~ filter(crimes_by_date, between(date, . - years(3), .))
		),
		test_data = map(
			forecast_date,
			~ filter(crimes_by_date, between(date, . + days(1), . + days(90)))
		)
	)

# check if models_by_date has the correct format
# models_by_date %>%
# 	mutate(
# 		training_period = map_chr(training_data, function (x) {
# 			paste(first(x$date), "to", last(x$date))
# 		}),
# 		test_period = map_chr(test_data, function (x) {
# 			paste(first(x$date), "to", last(x$date))
# 		})
# 	) %>%
# 	select(-training_data, -test_data) %>%
# 	View()



# RUN MODELS

future::plan("multiprocess")

system.time(
	models_by_date$models <- map(
		models_by_date$training_data[1], function (x) {
			map(as.character(unique(x$city_name)), function (y) {
				
				training_data <- x %>% 
					filter(city_name == y) %>% 
					select_if(~ !isTRUE(all.equal(., rep(FALSE, length(.)))))

				xreg_vars <- training_data %>% 
					as_tibble() %>% 
					select_if(is.logical) %>% 
					names()
				
				# message("\nRetaining variables ", paste(xreg_vars, collapse = ", "), 
				# 				" and key ", key(training_data), appendLF = TRUE)
				
				model(
					training_data,
					naive = NAIVE(crimes ~ lag()),
					snaive = SNAIVE(crimes ~ lag("week")),
					tslm = TSLM(as.formula(paste(c("crimes ~ trend() + season()", xreg_vars), collapse = " + "))),
					stl = decomposition_model(STL, crimes ~ trend() + season(), ETS(season_adjust), ETS(season_year), dcmp_args = list(robust = TRUE)),
					ets = ETS(crimes ~ trend() + season() + error()),
					arima = ARIMA(as.formula(paste(c("crimes ~ trend() + season()", xreg_vars), collapse = " + "))),
					# neural = NNETAR(as.formula(paste(c("crimes ~ trend() + season() + AR()", xreg_vars), collapse = " + ")), MaxNWts = 2000, scale_inputs = FALSE),
					fasster = FASSTER(as.formula(paste(c("crimes ~ poly(1) + trig(7) + ARMA()", xreg_vars), collapse = " + "))),
					prophet = prophet(as.formula(paste(c("crimes ~ growth() + season('year') + season('week')", xreg_vars), collapse = " + ")))
				) %>% 
					mutate(combo = (arima + ets + stl + fasster) / 4)
				
		})
	})
)

# system.time(
# 	models_by_date$models <- map(
# 		models_by_date$training_data, model,
# 		naive = NAIVE(crimes ~ lag()),
# 		snaive = SNAIVE(crimes ~ lag("week")),
# 		tslm = TSLM(crimes ~ trend() + season() + holiday + mlb + nfl + fbs + nba + 
# 									nhl + mls + auto + month_last + year_last),
# 		stl = decomposition_model(STL, crimes ~ trend() + season(),
# 															ETS(season_adjust), ETS(season_year),
# 															dcmp_args = list(robust = TRUE)),
# 		ets = ETS(crimes ~ trend() + season() + error()),
# 		arima = ARIMA(crimes ~ trend() + season() + holiday + mlb + nfl + fbs + 
# 										nba + nhl + mls + auto + month_last + year_last),
# 		neural = NNETAR(crimes ~ trend() + season() + AR() + holiday + mlb + nfl + 
# 											fbs + nba + nhl + mls + auto + month_last + year_last,
# 										MaxNWts = 2000),
# 		fasster = FASSTER(crimes ~ poly(1) + trig(7) + ARMA() + holiday + mlb + 
# 												nfl + fbs + nba + nhl + mls + auto + month_last + 
# 												year_last),
# 		prophet = prophet(crimes ~ growth() + season("year") + season("week") + 
# 												holiday + mlb + nfl + fbs + nba + nhl + mls + auto + 
# 												month_last + year_last)
# 	) %>%
# 		map(mutate, combo = (arima + ets + stl + fasster) / 4)
# )



# CALCULATE FORECASTS

system.time(
	models_by_date$forecasts <- map2(
		models_by_date$models[1], models_by_date$forecast_date[1],
		function (x, y) {
			
			# generate tsibble of new data for 90 days starting on the forecast date
			new_data <- expand.grid(
				city_name = map_chr(x, ~ .$city_name),
				date = seq.Date(y, y + days(89), by = "days")
			) %>% 
				arrange(city_name, date) %>% 
				left_join(holiday_dates, by = "date") %>% 
				left_join(black_fridays, by = "date") %>% 
				left_join(event_dates, by = c("date", "city_name")) %>% 
				mutate(
					holiday = ifelse(is.na(holiday), FALSE, holiday),
					black_friday = ifelse(is.na(black_friday), FALSE, black_friday),
					halloween = ifelse(month(date) == 10 & mday(date) == 31, TRUE, FALSE),
					month_last = mday(date + days(1)) == 1,
					year_last = yday(date + days(1)) == 1
				) %>%
				as_tsibble(index = date, key = city_name)
			
			map(x, function (z) {
				
				this_new_data <- new_data %>% 
					filter(city_name == z$city_name) %>% 
					select_at(vars(c("date", "city_name", training_vars(z))))
				
				# forecast based on new data
				# This is very slow for NNETAR() models because prediction intervals are
				# calculated by simulation. Set times = 0 to suppress simulations.
				forecast(z, new_data = this_new_data, bias_adjust = FALSE) %>%
					mutate(coef_variation = map_dbl(.distribution, coef_var))
				  # mutate(
					# 	coef_variation = map_dbl(
					# 		.distribution,
					# 		~ ifelse(length(.) > 0, .$sd / .$mean, NA)
					# 	)
					# )

			})
		
		}
	)
)

# system.time(
# 	models_by_date$forecasts <- furrr::future_map2(
# 		models_by_date$models, models_by_date$forecast_date,
# 		function (x, y) {
# 			
# 			# generate tsibble of new data for 90 days starting on the forecast date
# 			new_data <- expand.grid(
# 				city_name = as.character(unique(x$city_name)),
# 				date = seq.Date(y, y + days(89), by = "days")
# 			) %>% 
# 				arrange(city_name, date) %>% 
# 				mutate(
# 					month_last = mday(date + days(1)) == 1,
# 					year_last = yday(date + days(1)) == 1
# 				) %>%
# 				left_join(holiday_dates, by = "date") %>% 
# 				mutate(holiday = ifelse(is.na(holiday), FALSE, holiday)) %>% 
# 				as_tsibble(index = date, key = city_name)
# 			
# 			# forecast based on new data
# 			# This is very slow for NNETAR() models because prediction intervals are
# 			# calculated by simulation. Set times = 0 to suppress simulations.
# 			forecast(x, new_data = new_data, bias_adjust = FALSE) %>% 
# 				mutate(
# 					coef_variation = map_dbl(.distribution, 
# 																	 ~ ifelse(length(.) > 0, .$sd / .$mean, NA))
# 				)
# 			
# 		},
# 		.progress = TRUE
# 	)
# )



# CALCULATE ACCURACY MEASURES

models_by_date$accuracy <- pmap(
	list(models_by_date$forecasts, models_by_date$training_data,
			 models_by_date$test_data),
	function (...) {
		map(..1, function (x) {
			combined_data <- rbind(..2, ..3) %>% 
				filter(city_name == first(x$city_name))
			mean_cv <- as_tibble(x) %>% 
				group_by(city_name, .model) %>% 
				summarise(coef_var = mean(coef_variation)) %>% 
				ungroup()
			left_join(accuracy(x, combined_data), mean_cv, 
								by = c("city_name", ".model"))
		})
	}
)

# models_by_date$accuracy <- test_accuracy <- pmap(
# 	list(models_by_date$forecasts[1], models_by_date$training_data[1], 
# 			 models_by_date$test_data[1]), 
# 	~ left_join(
# 		accuracy(..1, rbind(..2, ..3)),
# 		as_tibble(..1) %>% 
# 			group_by(city_name, .model) %>% 
# 			summarise(coef_var = mean(coef_variation)) %>% 
# 			ungroup(),
# 		by = c("city_name", ".model")
# 	)
# )



# PORTMANTEAU TESTS

models_by_date$portmanteau <- map(models_by_date$models, map, function (x) {
	x %>% 
		augment(type = "response") %>% 
		# lag = 10 chosen following https://robjhyndman.com/hyndsight/ljung-box-test/
		features(.resid, portmanteau_tests, lag = 10)
})



# SAVE MODELS
models_by_date %>% 
	select(-models) %>% 
	write_rds("data_output/models_h2.Rds", compress = "gz")

