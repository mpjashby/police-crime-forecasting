library(fable)
library(lubridate)
library(tsibble)
library(tidyverse)

test_models <- tsibbledata::vic_elec %>% 
	filter(Time < ymd_hm("2012-02-01 00:00")) %>% 
	as_tibble() %>% 
	mutate(Time = floor_date(Time, "8 hours")) %>% 
	group_by(Time) %>% 
	summarise(Demand = mean(Demand), Temperature = mean(Temperature), 
						Holiday = first(Holiday)) %>% 
	mutate(
		Weekday = wday(Time, label = TRUE),
		FirstLast = case_when(
			mday(Time) == 1 ~ "first", 
			mday(Time) == days_in_month(Time) ~ "last",
			TRUE ~ "other"
		)
	) %>% 
	as_tsibble(index = Time) %>% 
	model(
		tslm = TSLM(Demand ~ trend() + season() + Weekday + Temperature + Holiday),
		works = ARIMA(Demand ~ trend() + season() + Temperature + Holiday),
		doesnt = ARIMA(Demand ~ trend() + season() + Weekday + Temperature + 
												 	Holiday),
		also_doesnt = ARIMA(Demand ~ trend() + season() + FirstLast + Temperature + 
													Holiday),
		
	)

test_models

new_data <- tsibbledata::vic_elec %>% 
	filter(between(Time, ymd_hm("2012-02-01 00:00"), 
								 ymd_hm("2012-03-01 00:00") - min(1))) %>% 
	as_tibble() %>% 
	mutate(Time = floor_date(Time, "8 hours")) %>% 
	group_by(Time) %>% 
	summarise(Demand = mean(Demand), Temperature = mean(Temperature), 
						Holiday = first(Holiday)) %>% 
	mutate(
		Weekday = wday(Time, label = TRUE),
		FirstLast = case_when(
			mday(Time) == 1 ~ "first", 
			mday(Time) == days_in_month(Time) ~ "last",
			TRUE ~ "other"
		)
	) %>% 
	as_tsibble(index = Time)

forecast(test_models, new_data = new_data)


