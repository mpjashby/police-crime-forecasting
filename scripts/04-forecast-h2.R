# This file runs the analysis to test H2, generating forecasts for each city
# using each method



# LOAD DATA

# load data
if (!exists("crimes")) {
	
	crimes <- read_csv("data_output/crime_data.csv.gz") %>% 
		filter(!is.na(date_single))
	
} else {
	
	crimes <- crimes %>% 
		filter(!is.na(date_single))
	
}



# PREPARE DATA

# create data for H2
crimes_by_date <- crimes %>% 
	mutate(date = date(date_single)) %>%
	count(city_name, date, name = "crimes") %>%
	as_tsibble(index = date)

models_by_date <- expand.grid(
	city_name = unique(crimes_by_date$city_name),
	forecast_date = seq.Date(ymd("2017-01-01"), ymd("2017-12-31"), by = "days")
) %>% 
	as_tibble() %>% 
	mutate(training_data = map2(forecast_date, city_name, function (x, y) {
			filter(crimes_by_date$data[[y]], date >= x - years(3) & date < x)
	}))
	
crimes_by_date <- crimes %>%
	mutate(date = date(date_single)) %>%
	count(city_name, date, name = "crimes") %>%
	nest(-city_name) %>%
	mutate(
		tsy = map(data, ~ ts(.$crimes, frequency = 365, start = c(2010, 1))),
		tsw = map(data, ~ ts(.$crimes, frequency = 7)),
		mts = map(data, ~ msts(.$crimes, seasonal.periods = c(7, 365),
													 start = c(2010, 1))),
		test = map(tsy, window, start = c(2016, 1))
	)



# NAIVE MODEL

crimes_by_date <- mutate(crimes_by_date, naive = map(tsy, function (x) {
	map(ymd("2014-12-01") + days(0:364), function (y) {
		extract_training_data(x, y)
	})
}))

crimes_by_month <- mutate(crimes_by_month, naive = map(ts, function (x) {
	map_dfr(ymd("2014-12-01") + months(0:35), function (y) {
		extract_training_data(x, y) %>% 
			rwf(h = last(forecast_horizon), lambda = BoxCox.lambda(.), 
					biasadj = TRUE) %>% 
			forecast_to_tibble() %>% 
			steps_ahead(forecast_horizon)
	})
}))
beepr::beep()
