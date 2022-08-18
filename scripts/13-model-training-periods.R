# This script generates forecasts for models based on different volumes of input
# data, to establish how much data is needed to generate reliable forecasts

if (!isNamespaceLoaded("tidyverse")) {
	source(here::here("scripts/00-initialise.R"))
}



# LOAD DATA --------------------------------------------------------------------

# Load data
if (!exists("crimes")) {
	
	crimes <- read_csv("data_output/crime_data.csv.gz") %>% 
		filter(!is.na(date_single))
	
}



# BUILD OBJECTS ----------------------------------------------------------------

# Get holiday dates
holiday_dates <- tibble(
	date = as_date(timeDate::holidayNYSE(2010:2019)),
	holiday = TRUE
)

# Create counts of monthly crimes
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

# Create training and test datasets
models_by_month <- expand_grid(
	forecast_date = yearmonth(seq(my("Jan 2015"), my("Dec 2016"), by = "months")),
	periods = 60:1
) %>% 
	as_tsibble(index = forecast_date, key = periods) %>% 
	mutate(
		training_data = map2(
			as_date(forecast_date), 
			periods,
			~ filter(
				crimes_by_month, 
				between(as_date(month), . - months(.y), .x - months(1))
			)
		),
		test_data = map(
			as_date(forecast_date),
			~ filter(crimes_by_month, between(as_date(month), ., . + months(35)))
		)
	) %>% 
	arrange(forecast_date)

models_dir <- here::here("data_output/models_training_periods")

if (!dir.exists(models_dir)) dir.create(models_dir)



# ESTIMATE MODELS --------------------------------------------------------------

# Remove the raw crime data because otherwise it causes problems when the 
# `future` package sets up multiple sessions on account of `crimes` being very
# large
rm(crimes)

# Set up parallel processing
future::plan("multisession")

system.time(
	furrr::future_pwalk(
		list(
			pluck(models_by_month, "training_data"),
			pluck(models_by_month, "forecast_date"),
			pluck(models_by_month, "periods")
		),
		function(x, y, z) {
			x %>%
				model(
					naive = NAIVE(crimes ~ lag()),
					snaive = SNAIVE(crimes ~ lag(12)),
					tslm = TSLM(crimes ~ trend() + season() + count_weekdays + count_holidays),
					stl = decomposition_model(STL(crimes ~ trend() + season()), ETS(season_adjust)),
					ets = ETS(crimes ~ trend() + season() + error()),
					arima = ARIMA(crimes ~ trend() + season() + count_weekdays + count_holidays),
					neural = NNETAR(crimes ~ trend() + season() + AR() + count_weekdays + count_holidays),
				) %>%
				write_rds(
					str_glue(
						"{models_dir}/models_tp_{str_replace_all(y, ' ', '_')}_",
						"{str_pad(z, width = 2, side = 'left', pad = 0)}.Rds"
					), 
					compress = "gz"
				)
		},
		.options = furrr::furrr_options(seed = TRUE, globals = "models_dir"),
		.progress = TRUE
	)
)



# MAKE FORECASTS ---------------------------------------------------------------

system.time(
	here::here("data_output/models_training_periods") %>%
		dir(pattern = "^models", full.names = TRUE) %>%
		furrr::future_walk(
			function(x) {
				
				this_model <- read_rds(x)
				
				this_date <- lubridate::as_date(last(this_model$snaive[[1]]$data$month)) + months(1)
				
				this_data <- models_by_month %>% 
					filter(lubridate::as_date(forecast_date) == this_date) %>% 
					pluck("test_data", 1) %>%
					select(-crimes)
				
				this_model %>% 
					fabletools::forecast(new_data = this_data) %>% 
					write_rds(
						stringr::str_glue(
							"{models_dir}/forecasts_tp_{as.character(this_date)}_",
							"{stringr::str_pad(nrow(this_model$snaive[[1]]$data), width = 2, side = 'left', pad = 0)}.Rds"
						), 
						compress = "gz"
					)
				
			},
			.options = furrr::furrr_options(
				seed = TRUE, 
				globals = c("models_dir", "models_by_month")
			),
			.progress = TRUE
		)
)



# SAVE MODELS ------------------------------------------------------------------

write_rds(
	models_by_month, 
	"data_output/models_training_periods.Rds", 
	compress = "gz"
)
