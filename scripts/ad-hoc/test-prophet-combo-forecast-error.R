library("fable")
library("fable.prophet")
library("lubridate")
library("tidyverse")

# generate some models based on the first month of `vic_elec` data, using an
# arima and tslm model for comparison
models <- tsibbledata::vic_elec %>% 
	filter(between(Date, ymd("2012-01-01"), ymd("2012-01-31"))) %>% 
	model(
		arima = ARIMA(Demand ~ trend() + season() + Temperature + Holiday),
		prophet = prophet(Demand ~ growth() + season('week') + Temperature + Holiday),
		tslm = TSLM(Demand ~ trend() + season() + Temperature + Holiday),
		combo = combination_model(
			ARIMA(Demand ~ trend() + season() + Temperature + Holiday),
			prophet(Demand ~ growth() + season('week') + Temperature + Holiday),
			TSLM(Demand ~ trend() + season() + Temperature + Holiday)
		)
	)

# check the models are not NULL etc.
report(models$combo[[1]])

# use the second month of data for forecasts
new_data <- tsibbledata::vic_elec %>% 
	filter(between(Date, ymd("2012-02-01"), ymd("2012-02-28")))

# generate combination models and forecast
models %>% 
	select(combo) %>% 
	forecast(new_data = new_data) %>% 
	head()

models %>% 
	mutate(combo = (arima + tslm) / 2) %>% 
	forecast(new_data = new_data) %>% 
	head()

models %>% 
	mutate(combo = (arima + prophet) / 2) %>% 
	forecast(new_data = new_data) %>% 
	head()

models %>% 
	mutate(combo = (prophet + tslm) / 2) %>% 
	forecast(new_data = new_data) %>% 
	head()
