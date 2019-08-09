library("fable")
library("feasts")
library("lubridate")
library("tsibble")
library("tsibbledata")
library("tidyverse")

# convert half-hourly electricity demand data to daily mean demand, since
# tsibbledata doesn't include a regular daily time series (`gafa_stock` is
# daily but irregular)
daily_demand <- vic_elec %>% 
	as_tibble() %>% 
	mutate(date = as_date(Time)) %>% 
	group_by(date) %>% 
	summarise(demand = mean(Demand)) %>% 
	as_tsibble(index = date)

# estimate an STL model (FAILS)
multiyear_leap <- daily_demand %>% 
	model(
		stl_snaive = decomposition_model(STL, demand ~ trend() + season(), SNAIVE(season_adjust)),
		stl_ets = decomposition_model(STL, demand ~ trend() + season(), ETS(season_adjust))
	)

# filter out 2012 data (because 2012 was a leap year) and estimate again (WORKS)
multiyear_noleap <- daily_demand %>% 
	filter(between(date, ymd("2013-01-01"), ymd("2014-12-31"))) %>% 
	model(
		stl_snaive = decomposition_model(STL, demand ~ trend() + season(), SNAIVE(season_adjust)),
		stl_ets = decomposition_model(STL, demand ~ trend() + season(), ETS(season_adjust))
	)

# do the opposite: estimate using only 2012 (WORKS)
oneyear_leap <- daily_demand %>% 
	filter(between(date, ymd("2012-01-01"), ymd("2012-12-31"))) %>% 
	model(
		stl_snaive = decomposition_model(STL, demand ~ trend() + season(), SNAIVE(season_adjust)),
		stl_ets = decomposition_model(STL, demand ~ trend() + season(), ETS(season_adjust))
	)


# filter out the leap day, keeping later dates in 2012, and estimate again (FAILS)
partyear_leap <- daily_demand %>% 
	filter(between(date, ymd("2012-03-01"), ymd("2014-12-31"))) %>% 
	model(
		stl_snaive = decomposition_model(STL, demand ~ trend() + season(), SNAIVE(season_adjust)),
		stl_ets = decomposition_model(STL, demand ~ trend() + season(), ETS(season_adjust))
	)

# filter out everything except 2012 before the leap day, and estimate again (WORKS)
oneyear_before <- daily_demand %>% 
	filter(between(date, ymd("2012-01-01"), ymd("2012-02-28"))) %>% 
	model(
		stl_snaive = decomposition_model(STL, demand ~ trend() + season(), SNAIVE(season_adjust)),
		stl_ets = decomposition_model(STL, demand ~ trend() + season(), ETS(season_adjust))
	)

# filter out everything except 2012 after the leap day, and estimate again (WORKS)
oneyear_after <- daily_demand %>% 
	filter(between(date, ymd("2012-03-01"), ymd("2012-12-31"))) %>% 
	model(
		stl_snaive = decomposition_model(STL, demand ~ trend() + season(), SNAIVE(season_adjust)),
		stl_ets = decomposition_model(STL, demand ~ trend() + season(), ETS(season_adjust))
	)

# check model types
rbind(multiyear_leap, multiyear_noleap, oneyear_leap, partyear_leap, oneyear_before, oneyear_after)

# check warnings for models that did not fit
warnings()
