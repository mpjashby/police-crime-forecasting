# This file sets up the system for working on this project, and should be run
# every time a new RStudio session begins. All packages should be loaded from
# this file and all custom function defined here.


# load packages
library("cowplot")   # marginal plots
library("crimedata") # crime data
library("fable")     # forecasting
library("feasts")    # also forecasting
library("fasster")   # yet more forecasting
library("ggrepel")   # repelled text labels
library("ggridges")  # ridge plots
library("lubridate") # handle dates
library("sf")        # spatial processing 
library("tsibble")   # tidy time series
library("tidyverse") # utility functions, **load this after all other packages**



# function for calculating absolute percentage error
# mape <- function (actual, predicted) {
# 	map2_dbl(actual, predicted, ~ mean(abs((.x - .y) / .x)))
# }
ape <- function (actual, predicted) {
	map2(actual, predicted, function (x, y) {
		x <- as.numeric(x)
		y %>% 
			mutate(error = abs((point_forecast - x) / x)) %>% 
			select(month = rowname, step, error)
	})
}


# function to extract training data from time series
extract_training_data <- function (series, end_date) {
	window(
		series, 
		start = c(year(end_date - years(5)), month(end_date + months(1))), 
		end = c(year(end_date), month(end_date))
	)
}


# function to extract forecasts for particular steps ahead
steps_ahead <- function (data, ...) {
	slice(data, ...)
}


# function to convert a forecast object to a tibble
forecast_to_tibble <- function (x) {
	x %>% 
		as.data.frame() %>% 
		rownames_to_column() %>% 
		as_tibble() %>% 
		janitor::clean_names() %>% 
		mutate(step = row_number()) %>% 
		select(step, everything())
}


# function to convert a forecast object to a tsibble
forecast_to_tsibble <- function (x) {
	x %>% 
		as.data.frame() %>% 
		rownames_to_column(var = "forecast_period") %>% 
		as_tibble() %>% 
		janitor::clean_names() %>% 
		mutate(
			forecast_period = date(parsedate::parse_date(forecast_period)),
			step = row_number()
		) %>% 
		select(step, everything())
}


# function to combine forecast with the data used to produce it
combine_forecast_data <- function (model, data) {
	
	# convert time series into tibble
	model_data <- stats:::.preformat.ts(data) %>% 
		as_tibble(rownames = "year") %>% 
		gather(key = "month", value = "crimes", -year) %>% 
		mutate(
			crimes = as.numeric(crimes),
			period = paste(month, year),
			date = ymd(paste(year, month, "01"))
		) %>% 
		arrange(year)
	
	# convert forecast object into tibble
	model_forecast <- as.data.frame(model) %>% 
		janitor::clean_names() %>% 
		as_tibble(rownames = "period")
	
	# join data to forecasts
	left_join(model_data, model_forecast, by = "period")
	
}


# plot the accuracy of a forecast, using the output of combine_forecast_data()
# as the input
plot_forecast_accuracy <- function (model_data, title) {
	
	ggplot(model_data, aes(x = date, y = crimes)) +
		geom_ribbon(aes(ymin = lo_95, ymax = hi_95), fill = "grey85") +
		geom_ribbon(aes(ymin = lo_80, ymax = hi_80), fill = "grey70") +
		geom_line(aes(y = point_forecast), linetype = "11", na.rm = TRUE) +
		geom_line() +
		scale_y_continuous(limits = c(0, NA)) +
		labs(
			title = title,
			x = NULL,
			y = "number of crimes"
		) +
		theme_minimal()
	
}

