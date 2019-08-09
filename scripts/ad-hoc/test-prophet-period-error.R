library("crimedata")
library("fable")
library("fable.prophet")
library("tsibble")
library("tidyverse")

daily_crimes <- get_crime_data(years = 2010:2013, cities = "New York") %>% 
	mutate(date = as_date(date_single)) %>% 
	count(date) %>% 
	filter(!is.na(date)) %>% 
	as_tsibble(index = date)

prophet_models <- model(
	daily_crimes,
	prophet_year = prophet(n ~ growth() + season(period = "year")),
	prophet_365 = prophet(n ~ growth() + season(period = 365)),
	prophet_365_order = prophet(n ~ growth() + season(period = 365, order = 4)),
	prophet_week = prophet(n ~ growth() + season(period = "week"))
)
