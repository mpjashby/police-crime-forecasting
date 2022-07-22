# This file applies each forecasting method for H3 to the data from each city



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
	# categories of interest
	mutate(
		date = as_date(date_single),
		offense = case_when(
			offense_type  == "aggravated assault" ~ "aggravated assault",
			# offense_group == "arson" ~ "arson",
			offense_group == "burglary/breaking & entering" ~ "burglary",
			offense_group == "homicide offenses" ~ "homicide",
			# offense_group == "larceny/theft offenses" ~ "theft",
			# offense_type  == "rape (except statutory rape)" ~ "rape",
			offense_group == "robbery" ~ "robbery",
			# offense_group == "motor vehicle theft" ~ "vehicle theft",
			TRUE ~ "other"
		)
	) %>% 
	# Filter out cities with no data for one or more of the crimes of interest
	filter(
		city_name %in% c("Detroit", "Los Angeles", "New York", "Tucson"),
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
	as_tsibble(index = month, key = c(city_name, district, offense)) %>% 
	filter(!(city_name == "Detroit" & offense == "rape"))
	
# Create tsibble to hold model results and separate data into training/test sets
models_by_month <- tsibble(
	forecast_date = yearmonth(
		seq(ymd("2013-01-01"), ymd("2019-01-01"), by = "months")
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



<<<<<<< HEAD
# RUN MODELS

tryCatch(
	{
		
		furrr::future_map2(
			models_by_month$training_data, 
			format.Date(models_by_month$forecast_date, "%Y-%m-%d"), 
			function (x, y) {
				
				if (file.exists(glue::glue("data_output/model_h3_{y}.rds")))
					return(invisible(NULL))
				
				model(
					x,
					naive = NAIVE(crimes ~ lag()),
					snaive = SNAIVE(crimes ~ lag("year")),
					tslm = TSLM(crimes ~ trend() + season() + count_weekdays + 
												count_holidays),
					stl = decomposition_model(STL, crimes ~ trend() + season(), 
																		ETS(season_adjust), 
																		dcmp_args = list(robust = TRUE)),
					ets = ETS(crimes ~ trend() + season() + error()),
					arima = ARIMA(crimes ~ trend() + season() + count_weekdays + 
													count_holidays),
					neural = NNETAR(crimes ~ trend() + season() + AR() + count_weekdays + 
														count_holidays),
					fasster = FASSTER(crimes ~ poly(1) + trig(12) + ARMA() + 
															count_weekdays + count_holidays),
					prophet = prophet(crimes ~ growth() + season("year") + 
															count_weekdays + count_holidays)
				) %>% 
					write_rds(glue::glue("data_output/model_h3_{y}.rds"), compress = "gz")
				
				send_notification(glue::glue("Finished estimating models for forecast ",
																		 "date {y}"))
				
			}
		)
		
		send_notification("Finished estimating models for H3")
		
	},
	error = function (e) send_notification(e, "error"),
	warning = function (w) send_notification(w, "warning"),
	message = function (m) send_notification(m)
=======
# RUN MODELS -------------------------------------------------------------------

system.time(
	models_test <- models_by_month %>% 
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
			neural = NNETAR(crimes ~ trend() + season() + AR() + count_weekdays + count_holidays),
			fasster = FASSTER(crimes ~ trend() + season() + ARMA() + count_weekdays + count_holidays),
			prophet = prophet(crimes ~ growth() + season("year") + count_weekdays + count_holidays),
			.options = furrr::furrr_options(seed = TRUE),
			.progress = TRUE
		) %>%
		map(mutate, combo = (arima + ets + fasster + stl) / 4)
>>>>>>> 01d118cdf5eae0c48688065001e7c55f9a0e4d2b
)



<<<<<<< HEAD
# CALCULATE FORECASTS

tryCatch(
	{
		
		walk2(
			format.Date(models_by_month$forecast_date[1], "%Y-%m-%d"), 
			models_by_month$test_data[1], 
			function (x, y) {
				
				if (file.exists(glue::glue("data_output/forecast_h3_{x}.rds"))) {
					send_notification(glue::glue("Skipping forecasting for {x}"))
					return(invisible(NULL))
				}
				
				if (!file.exists(glue::glue("data_output/model_h3_{x}.rds"))) {
					send_notification(glue::glue("Cannot find model file for {x}"),
														"warning")
					return(invisible(NULL))
				}
				
				# forecast based on new data
				# This is very slow for NNETAR() models because prediction intervals are
				# calculated by simulation. Set times = 0 to suppress simulations.
				glue::glue("data_output/model_h3_{x}.rds") %>%
					read_rds() %>%
					filter(!(city_name == "Detroit" & offense == "rape")) %>%
					forecast(new_data = select(y, -crimes), bias_adjust = FALSE) %>%
					mutate(coef_variation = map_dbl(.distribution, coef_var)) %>%
					write_rds(glue::glue("data_output/forecast_h3_{x}.rds"),
										compress = "gz")
				
				send_notification(glue::glue("Finished generating forecasts for {x}"))
				
			}
		)
		
		send_notification("Finished estimating models for H3")
		
	},
	error = function (e) send_notification(e, "error")
=======
# CALCULATE FORECASTS ----------------------------------------------------------

system.time(
	models_by_month$forecasts <- furrr::future_map2(
		models_by_month$models, 
		models_by_month$test_data, 
		function (x, y) {
			
			# Forecast based on new data
			# This is very slow for NNETAR() models because prediction intervals are
			# calculated by simulation. Set times = 0 to suppress simulations.
			x %>% 
				filter(!(city_name == "Detroit" & offense == "rape")) %>% 
				forecast(new_data = select(y, -crimes), bias_adjust = FALSE) %>%
				mutate(coef_variation = sqrt(variance(crimes)) / abs(mean(crimes)))
			
		},
		.progress = TRUE
	)
>>>>>>> 01d118cdf5eae0c48688065001e7c55f9a0e4d2b
)

				

# CALCULATE ACCURACY MEASURES --------------------------------------------------

<<<<<<< HEAD
# system.time({
# 	models_by_month$accuracy <- furrr::future_pmap(
# 		list(models_by_month$forecasts[1], models_by_month$training_data[1],
# 				 models_by_month$test_data[1]), 
# 		~ left_join(
# 			accuracy(..1, rbind(..2, ..3)),
# 			as_tibble(..1) %>% 
# 				group_by(city_name, district, offense, .model) %>% 
# 				summarise(coef_var = mean(coef_variation)) %>% 
# 				ungroup(),
# 			by = c("city_name", "district", "offense", ".model")
# 		),
# 		.progress = TRUE
# 	)
# 	
# 	slackr_bot("Finished calculating accuracy statistics for H3")
# })
=======
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

>>>>>>> 01d118cdf5eae0c48688065001e7c55f9a0e4d2b


# SAVE MODELS ------------------------------------------------------------------

<<<<<<< HEAD
# SAVE MODELS
# models_by_month %>% 
# 	select(-models) %>%
# 	mutate(forecasts = map(forecasts, ~select(as_tibble(.), -.distribution))) %>%
# 	write_rds("data_output/models_h3.Rds", compress = "gz")
=======
models_by_month %>% 
	select(-models) %>%
	mutate(forecasts = map(forecasts, ~select(as_tibble(.), -.distribution))) %>%
	write_rds("data_output/models_h3.Rds", compress = "gz")
>>>>>>> 01d118cdf5eae0c48688065001e7c55f9a0e4d2b

