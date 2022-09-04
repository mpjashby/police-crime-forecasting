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
if (!exists("event_dates")) {
	event_dates <- read_csv("data_output/event_data.csv.gz")
}



# PROCESS DATA ----------------------------------------------------------------

# Get holiday dates
holiday_dates <- tibble(
	date = as_date(timeDate::holidayNYSE(2010:2019)),
	holiday = TRUE
)

# Get Black Friday dates
black_fridays <- holiday_dates %>% 
	filter(month(date) == 11) %>% 
	select(date) %>% 
	mutate(date = date + days(1), black_friday = TRUE)


## Create counts of monthly crimes for Scenario 1 ----
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


## Create counts of daily crime for Scenario 2 ----
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
	filter(date %within% lubridate::interval(dmy("1 Jan 2010"), dmy("31 Dec 2019"))) %>%
	as_tsibble(index = date, key = city_name)


## Create counts of monthly assaults per district for Scenario 3 ----
crimes_by_district <- crimes %>% 
	filter(!is.na(district)) %>% 
	mutate(date = as_date(date_single)) %>% 
	rename(offense = offense_type) %>% 
	# Choose one district from each city except NYC. This was done randomly using
	# `round(runif(1, 1, n))` where `n` is the number of districts in each city,
	# excluding special districts with very little crime (such as airports). The
	# chosen district in each case is the nth district alphabetically, rather than
	# the district with that number in cities that have numbered rather than named
	# districts. Since we don't have district boundaries for Kansas City, two
	# districts from NYC are chosen to keep the overall number of forecasts the
	# same across Scenarios.
	# We will only keep aggravated assaults to keep computational time reasonable. 
	# Agg assaults have been chosen because there are reasonable numbers (i.e. not 
	# too few) of offences in all cities.
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
	offense == "aggravated assault"
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



# BUILD OBJECTS ----------------------------------------------------------------


## Scenario 1 ----
models_h1 <- expand_grid(
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


## Scenario 2 ----
models_h2 <- tibble(
	forecast_date = seq(
		min(crimes_by_date$date) + years(6), 
		min(crimes_by_date$date) + years(6) + months(23), 
		by = "days"
	)
) %>% 
	# Select a single day from each month for 24 months to create the same number
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
	expand_grid(periods = 1:60) %>% 
	as_tsibble(index = forecast_date, key = periods) %>% 
	mutate(
		training_data = map2(
			forecast_date, 
			periods,
			~ filter(crimes_by_date, between(date, .x - weeks(.y), .x - days(1)))
		),
		test_data = map(
			as_date(forecast_date),
			~ filter(crimes_by_date, between(date, ., . + days(90)))
		)
	)


## Scenario 3 ----
models_h3 <- expand_grid(
	forecast_date = yearmonth(seq(my("Jan 2015"), my("Dec 2016"), by = "months")),
	periods = 12:60
) %>% 
	as_tsibble(index = forecast_date, key = periods) %>% 
	mutate(
		training_data = map2(
			as_date(forecast_date), 
			periods,
			~ filter(
				crimes_by_district, 
				between(as_date(month), .x - months(.y), .x - months(1))
			)
		),
		test_data = map(
			as_date(forecast_date),
			~ filter(crimes_by_district, between(as_date(month), ., . + months(11)))
		)
	)



# SET UP MODELS/FORECASTS DIRECTORY --------------------------------------------

models_dir <- here::here("data_output/models_training_periods")

if (!dir.exists(models_dir)) dir.create(models_dir)



# ESTIMATE MODELS --------------------------------------------------------------

# Remove the raw crime data because otherwise it causes problems when the 
# `future` package sets up multiple sessions on account of `crimes` being very
# large
rm(crimes, crimes_by_month, crimes_by_date, crimes_by_district, holiday_dates)

# Set up parallel processing
future::plan("multisession")


## Scenario 1 ----
models_h1 %>% 
	select(training_data, forecast_date, periods) %>% 
	furrr::future_pwalk(
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
						"{models_dir}/h1_models_tp_{str_replace_all(y, ' ', '_')}_",
						"{str_pad(z, width = 2, side = 'left', pad = 0)}.Rds"
					), 
					compress = "gz"
				)
		},
		.options = furrr::furrr_options(seed = TRUE, globals = "models_dir"),
		.progress = TRUE
	)


## Scenario 2 ----
models_h2 %>% 
	select(training_data, forecast_date, periods) %>% 
	furrr::future_pwalk(
		function (training_data, forecast_date, periods) {
			
			this_file <- str_glue(
				here::here("data_output/models_training_periods"),
				"/h2_models_tp_{str_replace_all(forecast_date, ' ', '_')}_",
				"{str_pad(periods, width = 2, side = 'left', pad = 0)}.Rds"
			)
			
			if (file.exists(this_file)) return(NULL)
			
			training_data %>% 
				pluck("city_name") %>% 
				unique() %>% 
				as.character() %>% 
				set_names() %>% 
				map(function(y) {

					# Remove constant variables
					data_for_model <- training_data %>% 
						filter(city_name == y) %>% 
						select(!where(is_constant))
					
					# Get names of non-constant logical variables, which will be included
					# in the model formulae
					xreg_vars <- data_for_model %>% 
						as_tibble() %>% 
						select(where(is.logical)) %>% 
						names()
					
					# Convert vector of non-constant variables to a string
					this_formula <- paste(
						c("crimes ~ trend() + season()", xreg_vars), 
						collapse = " + "
					)

					# Estimate models
					model(
						data_for_model,
						naive = NAIVE(crimes ~ lag()),
						snaive = SNAIVE(crimes ~ lag("week")),
						tslm = TSLM(as.formula(this_formula)),
						stl = decomposition_model(
							STL(crimes ~ trend() + season()), 
							ETS(season_adjust), 
							ETS(season_year)
						),
						ets = ETS(crimes ~ trend() + season() + error()),
						arima = ARIMA(as.formula(this_formula)),
						neural = NNETAR(
							as.formula(str_glue("{this_formula} + AR()")), 
							MaxNWts = 2000, 
							scale_inputs = FALSE
						)
					)
					
				}) %>% 
				write_rds(this_file, compress = "gz")

		},
		.options = furrr::furrr_options(seed = TRUE),
		.progress = TRUE
	)	


## Scenario 3 ----
models_h3 %>% 
	select(training_data, forecast_date, periods) %>% 
	furrr::future_pwalk(
		function(training_data, forecast_date, periods) {
			
			this_file <- str_glue(
				here::here("data_output/models_training_periods"),
				"/h3_models_tp_{str_replace_all(forecast_date, ' ', '_')}_",
				"{str_pad(periods, width = 2, side = 'left', pad = 0)}.Rds"
			)
			
			if (file.exists(this_file)) return(NULL)
			
			training_data %>%
				model(
					naive = NAIVE(crimes ~ lag()),
					snaive = SNAIVE(crimes ~ lag(12)),
					tslm = TSLM(crimes ~ trend() + season() + count_weekdays + count_holidays),
					stl = decomposition_model(STL(crimes ~ trend() + season()), ETS(season_adjust)),
					ets = ETS(crimes ~ trend() + season() + error()),
					arima = ARIMA(crimes ~ trend() + season() + count_weekdays + count_holidays),
					neural = NNETAR(crimes ~ trend() + season() + AR() + count_weekdays + count_holidays),
				) %>%
				write_rds(this_file, compress = "gz")
			
		},
		.options = furrr::furrr_options(seed = TRUE),
		.progress = TRUE
	)



# MAKE FORECASTS ---------------------------------------------------------------


## Scenario 1 ----
furrr::future_pwalk(
	models_h1,
	function(forecast_date, periods, training_data, test_data) {
		
		this_file <- here::here(str_glue(
			"data_output/models_training_periods/h1_forecasts_tp_",
			"{str_replace_all(forecast_date, ' ', '_')}_",
			str_pad(periods, width = 2, side = 'left', pad = 0),
			".Rds"
		))
		
		if (file.exists(this_file)) return(NULL)
		
		this_file %>%
			str_replace("forecasts_tp", "models_tp") %>% 
			read_rds() %>% 
			forecast(new_data = test_data) %>% 
			write_rds(this_file, compress = "gz")
		
	},
	.options = furrr::furrr_options(seed = TRUE),
	.progress = TRUE
)


## Scenario 2 ----
furrr::future_pwalk(
	head(models_h2, 2),
	function(forecast_date, periods, training_data, test_data) {
		
		this_file <- here::here(str_glue(
			"data_output/models_training_periods/h2_forecasts_tp_",
			"{str_replace_all(forecast_date, ' ', '_')}_",
			str_pad(periods, width = 2, side = 'left', pad = 0),
			".Rds"
		))
		
		if (file.exists(this_file)) return(NULL)
		
		this_model <- this_file %>%
			str_replace("forecasts_tp", "models_tp") %>% 
			read_rds()
		
		1:length(this_model) %>% 
			set_names(nm = names(this_model)) %>% 
			map(function(i) {
				
				new_data <- test_data %>% 
					filter(city_name == names(this_model)[i]) %>% 
					select(-city_name)
				
				forecast(this_model[i], new_data = new_data)
				
			}) %>% 
			write_rds(this_file, compress = "gz")
		
	},
	.options = furrr::furrr_options(seed = TRUE),
	.progress = TRUE
)


## Scenario 3 ----
furrr::future_pwalk(
	models_h3,
	function(forecast_date, periods, training_data, test_data) {
		
		this_file <- here::here(str_glue(
			"data_output/models_training_periods/h3_forecasts_tp_",
			"{str_replace_all(forecast_date, ' ', '_')}_",
			str_pad(periods, width = 2, side = 'left', pad = 0),
			".Rds"
		))
		
		if (file.exists(this_file)) return(NULL)
		
		this_file %>%
			str_replace("forecasts_tp", "models_tp") %>% 
			read_rds() %>% 
			forecast(new_data = test_data) %>% 
			write_rds(this_file, compress = "gz")
		
	},
	.options = furrr::furrr_options(seed = TRUE),
	.progress = TRUE
)



# SAVE MODELS ------------------------------------------------------------------

write_rds(
	models_h1, 
	"data_output/models_training_periods_h1.Rds", 
	compress = "gz"
)



# SAVE FORECASTS ---------------------------------------------------------------


## Scenario 1 ----
models_h1 %>% 
	select(forecast_date, periods, test_data) %>% 
	furrr::future_pmap_dfr(
		function(forecast_date, periods, test_data) {
			
			test_data <- select(test_data, city_name, month, actual = crimes)
			
			str_glue(
				"data_output/models_training_periods/h1_forecasts_tp_",
				str_replace_all(forecast_date, ' ', '_'),
				"_",
				str_pad(periods, width = 2, side = 'left', pad = 0),
				".Rds"
			) %>% 
				here::here() %>%  
				read_rds() %>% 
				mutate(
					forecast_date = forecast_date, 
					periods = periods
				) %>% 
				left_join(test_data, by = c("city_name", "month")) %>%
				as_tibble() %>%
				select(
					city_name, 
					forecast_date, 
					periods,
					model = .model, 
					month, 
					actual, 
					forecast = .mean
				)
			
		}
	) %>%
	write_rds(
		here::here("data_output/models_training_periods_forecasts_h1.Rds"), 
		compress = "gz"
	)

