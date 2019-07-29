library(fable)
library(fasster)
library(lubridate)
library(rbenchmark)
library(tsibble)
library(tidyverse)

test_data <- tsibbledata::PBS %>% 
	filter(
		Concession == "General", Type == "Safety net", 
		ATC2 %in% c("A01", "A02", "A03", "A04", "A05"),
		between(Month, ymd("2005-01-01"), ymd("2007-12-31"))
	) %>% 
	update_tsibble(key = c("ATC2"))

test_models <- list(
	tslm = model(test_data, TSLM(Scripts ~ trend() + season())),
	stl = model(test_data, decomposition_model(STL, Scripts ~ trend() + season(), 
																						 NAIVE(season_adjust))),
	ets = model(test_data, ETS(Scripts ~ trend() + season() + error())),
	# arima = model(test_data, ARIMA(Scripts ~ trend() + season())),
	var = model(test_data, VAR(Scripts ~ trend() + season() + AR())),
	neural = model(test_data, NNETAR(Scripts ~ trend() + season() + AR())),
	fasster = model(test_data, FASSTER(crimes ~ poly(1) + trig(12) + ARMA()))
)

test_forecast_times2 <- map_dfr(
	test_models, 
	~ benchmark(forecast(., h = "3 years", times = 0), replications = 1), 
	.id = "model"
) %>% 
	select(model, execution_time = elapsed)
