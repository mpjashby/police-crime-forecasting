# SETUP ------------------------------------------------------------------------

# Load packages (packages are not loaded unless functions from it are used more
# than once)
library(fable)
library(lubridate)
library(tsibble)
library(tidyverse)



# LOAD DATA --------------------------------------------------------------------

# Load 10 years of Los Angeles crime data from the Crime Open Database
# Detailed documentation for the data is available at https://osf.io/zyaqn/
crimes <- crimedata::get_crime_data(
	years = 2011:2020, 
	cities = "Los Angeles", 
	type = "core"
)

# Load the dates of LA Dodgers baseball games using the baseballr
# package from http://github.com/BillPetti/baseballr/
game_days <- map_dfr(2011:2020, ~ baseballr::team_results_bref("LAD", .)) %>% 
	# Convert the column names to a consistent format
	janitor::clean_names() %>% 
	# Keep only home games
	filter(h_a == "H") %>% 
	mutate(
		# Remove numbers in brackets after dates on days with multiple games
		date = str_remove(date, "\\s\\(.+?\\)"),
		# Parse game dates
		game_day = as_date(parse_date_time(str_glue("{date} {year}"), "a b d Y"))
	) %>% 
	# Convert the data to a vector of game dates
	pull(game_day) %>% 
	# Remove the duplicate game dates caused by days with multiple games
	unique()

# Get dates of federal public holidays
holidays <- as_date(timeDate::holidayNYSE(2011:2020))



# PREPARE DATA -----------------------------------------------------------------

# Count the number of aggravated assaults each day
assault_count <- crimes %>% 
	# Keep only aggravated assaults
	filter(offense_code == "13A") %>% 
	# Count the number of assaults each week
	count(date = as_date(date_single), name = "count") %>% 
	# Remove a small number of offences for which yearweek() returns NA
	filter(!is.na(date)) %>% 
	# Convert the data to a time-series-aware format called a tsibble
	as_tsibble(index = date) %>% 
	# Fill in any gaps in the time series that result from days with zero assaults
	fill_gaps(count = 0) %>% 
	mutate(
		# Create a variable for the day of the week
		weekday = factor(wday(date, label = TRUE), ordered = FALSE),
		# Create a variable showing if a day was a holiday or not
		holiday = date %in% holidays,
		# Create a variable showing if a day was a game day or not
		game_day = date %in% game_days
	)

# Plot the time-series using the autoplot() function that comes with fable
autoplot(assault_count)



# MODEL DATA -------------------------------------------------------------------

# Run the ARIMA models and automatically detect the optimal values for p, d, q
assault_models <- model(
	assault_count, 
	# A fully automatic model
	auto = ARIMA(count),
	# A model with specific components for a linear trend and different days of
	# the week
	simple = ARIMA(count ~ trend() + weekday),
	# A model that also includes whether a day is a public holiday and/or a Giants
	# game day
	full = ARIMA(count ~ trend() + weekday + holiday + game_day)
)

# Compare error metrics for the models
accuracy(assault_models)

# Look at co-efficients for individual model terms
assault_models %>% 
	# Extract model co-efficients
	broom::tidy() %>% 
	filter(.model == "full") %>% 
	# Format p-values so they are easier to read
	mutate(p.value = scales::pvalue(p.value))



# FORECASTING ------------------------------------------------------------------

# Count number of personal robberies each week
robbery_count <- crimes %>% 
	filter(offense_code == "12A") %>% 
	count(week = yearweek(date_single), name = "count") %>% 
	# Remove a small number of offences for which yearweek() returns NA
	filter(!is.na(week)) %>% 
	# Remove the first and last week because the data start/end part-way through
	slice(2:(n() - 1)) %>% 
	as_tsibble(index = week) %>% 
	fill_gaps(count = 0)

# Model robbery counts before pandemic
robbery_model <- robbery_count %>% 
	filter(week <= yearweek(ymd("2020-01-20"))) %>% 
	model(arima = ARIMA(count))

# Forecast robberies from model and compare to actual values during pandemic
robbery_model %>% 
	forecast(h = "1 year") %>% 
	autoplot() + 
	geom_line(
		aes(week, count), 
		data = filter(robbery_count, week > yearweek(ymd("2020-01-01")))
	) +
	scale_y_continuous(limits = c(0, NA)) + 
	theme_minimal()

