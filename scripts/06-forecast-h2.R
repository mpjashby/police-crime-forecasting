# This file runs the analysis to test H2, generating forecasts for each city
# using each method



if (!isNamespaceLoaded("tidyverse")) {
	source(here::here("scripts/00-initialise.R"))
}



# SET SEED
# since we will be using sample_n() to choose the dates on which to make the
# forecasts, we can set a seed here to make the sample reproducible
set.seed(123)



# LOAD DATA --------------------------------------------------------------------

if (!exists("crimes")) {
	crimes <- read_csv("data_output/crime_data.csv.gz") %>% 
		filter(!is.na(date_single))
}
if (!exists("event_dates")) {
	event_dates <- read_csv("data_output/event_data.csv.gz")
}



# PREPARE DATA -----------------------------------------------------------------

# Set range of dates for crime data, based on 3 years of training data and 4
# years of test data
date_range <- lubridate::interval(dmy("1 Jan 2010"), dmy("31 Dec 2019"))

# get holiday dates
holiday_dates <- tibble(
	date = as_date(timeDate::holidayNYSE(2010:2019)),
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
	left_join(holiday_dates, by = "date") %>% 
	left_join(black_fridays, by = "date") %>% 
	left_join(event_dates, by = c("date", "city_name")) %>% 
	replace_na(list(
		holiday = FALSE, 
		black_friday = FALSE,
		nfl = FALSE,
		mlb = FALSE,
		nba = FALSE,
		nhl = FALSE,
		mls = FALSE,
		auto = FALSE,
		fbs = FALSE
	)) %>% 
	mutate(
		halloween = month(date) == 10 & mday(date) == 31,
		month_last = mday(date + days(1)) == 1,
		year_last = yday(date + days(1)) == 1
	) %>%
	# some crimes will now be recorded on the last day of the year before the 
	# start of the study period, so we remove these
	filter(date %within% date_range) %>%
	as_tsibble(index = date, key = city_name)

models_by_date <- tibble(
	forecast_date = seq(
		min(crimes_by_date$date) + years(3), 
		min(crimes_by_date$date) + years(3) + months(47), 
		by = "days"
	)
) %>% 
	# Select a single day from each month for 48 months to create the same number
	# of comparisons as in the other Scenarios.
	mutate(
		month = month(forecast_date, label = TRUE), 
		year = year(forecast_date)
	) %>% 
	group_by(year, month) %>% 
	sample_n(1) %>% 
	ungroup() %>% 
	select(-month, -year) %>% 
	arrange(forecast_date) %>% 
	mutate(
		training_data = map(
			forecast_date, 
			~ filter(crimes_by_date, between(date, . - years(3), . - days(1)))
		),
		test_data = map(
			forecast_date,
			~ filter(crimes_by_date, between(date, ., . + days(90)))
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
# Models must be run separately for each city because some cities have constant
# values for some predictors (e.g. if they don't have certain types of event),
# so the right-hand side of the regression formula varies for each city and must
# be constructed separately

# Set up parallel processing
future::plan("multisession")

# remove crimes object because it is too big to copy to each parallel instance
rm(crimes, crimes_by_date)

system.time(
	models_by_date$models <- furrr::future_map(
		models_by_date$training_data, 
		function (x) {
			map(as.character(unique(x$city_name)), function (y) {
				
				# Remove constant variables
				training_data <- x %>% 
					filter(city_name == y) %>% 
					select(!where(is_constant))
				
				# Get names of non-constant logical variables, which will be included
				# in the model formulae
				xreg_vars <- training_data %>% 
					as_tibble() %>% 
					select(where(is.logical)) %>% 
					names()
				
				model(
					training_data,
					naive = NAIVE(crimes ~ lag()),
					snaive = SNAIVE(crimes ~ lag("week")),
					common = RW(crimes ~ lag(7) + lag(14) + lag(21) + lag(28)),
					tslm = TSLM(as.formula(paste(c("crimes ~ trend() + season()", xreg_vars), collapse = " + "))),
					stl = decomposition_model(STL(crimes ~ trend() + season()), ETS(season_adjust), ETS(season_year)),
					ets = ETS(crimes ~ trend() + season() + error()),
					arima = ARIMA(as.formula(paste(c("crimes ~ trend() + season()", xreg_vars), collapse = " + "))),
					neural = NNETAR(as.formula(paste(c("crimes ~ trend() + season() + AR()", xreg_vars), collapse = " + ")), MaxNWts = 2000, scale_inputs = FALSE),
					fasster = FASSTER(as.formula(paste(c("crimes ~ trend() + season() + ARMA()", xreg_vars), collapse = " + "))),
					prophet = prophet(as.formula(paste(c("crimes ~ growth() + season('year') + season('week')", xreg_vars), collapse = " + ")))
				) %>%
					mutate(
						city_name = y,
						combo = (arima + prophet + tslm) / 3
					)
				
			})
		},
		.options = furrr::furrr_options(seed = TRUE),
		.progress = TRUE
	)
)



# CALCULATE FORECASTS

system.time(
	models_by_date$forecasts <- furrr::future_map2(
		models_by_date$models, models_by_date$test_data,
		function (x, y) {
			
			map(x, function (z) {
				
				this_new_data <- y %>% 
					filter(city_name == z$city_name) %>% 
					select_at(vars(c("date", training_vars(z))))
				
				# forecast based on new data
				# This is very slow for NNETAR() models because prediction intervals are
				# calculated by simulation. Set times = 0 to suppress simulations.
				forecast(z, new_data = this_new_data) %>%
					mutate(
						city_name = z$city_name,
						coef_variation = sqrt(distributional::variance(crimes)) / abs(mean(crimes))
					)
				
			})
		
		},
		.options = furrr::furrr_options(seed = TRUE),
		.progress = TRUE
	)
)



# CALCULATE ACCURACY MEASURES

models_by_date$accuracy <- pmap(
	list(
		models_by_date$forecasts, 
		models_by_date$training_data,
		models_by_date$test_data
	),
	function (...) {
		map(..1, function (x) {
			combined_data <- bind_rows(..2, ..3) %>% 
				filter(city_name == first(x$city_name))
			mean_cv <- as_tibble(x) %>% 
				group_by(city_name, .model) %>% 
				summarise(coef_var = mean(coef_variation), .groups = "drop")
			left_join(accuracy(x, combined_data), mean_cv, 
								by = c("city_name", ".model"))
		})
	}
)



# PORTMANTEAU TESTS

models_by_date$portmanteau <- map_depth(
	models_by_date$models, 
	.depth = 2, 
	function (x) {
		x %>% 
			augment(type = "response") %>% 
			# lag = 10 chosen following 
			# https://robjhyndman.com/hyndsight/ljung-box-test/
			features(.resid, portmanteau_tests, lag = 10)
	}
)



# SAVE MODELS
models_by_date %>% 
	# select(-models) %>% 
	# mutate(
	# 	forecasts = map_depth(forecasts, 2, ~select(as_tibble(.), -crimes))
	# ) %>% 
	write_rds("data_output/models_h2_full.Rds", compress = "gz")



# WRITE SUMMARY
models_by_date %>% 
	as_tibble() %>% 
	select(forecast_date, accuracy) %>% 
	# Un-nest data twice because the data for H2 had to be double nested because
	# of the different predictors used for different cities
	unnest(cols = "accuracy") %>% 
	unnest(cols = "accuracy") %>% 
	select(forecast_date, model = .model, city_name, mape = MAPE) %>%
	write_rds("data_output/models_h2_summary.Rds", compress = "gz")
