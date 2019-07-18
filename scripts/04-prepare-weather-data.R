# this script downloads weather data and stores it

# get a list of codes for cities with weather stations
# sorting by desc(id) retrieves the US stations first, because the country code
# is the first part of the station ID, followed by the state FIPS code
noaa_cities <- ncdc_locs(datasetid = "GHCND", locationcategoryid = "CITY",
												 startdate = "2010-01-01", enddate = "2018-12-31",
												 sortfield = "id", sortorder = "desc", limit = 1000) %>% 
	pluck("data")

# from this list, manually choose the cities of interest
weather_cities <- c(
	"CITY:US480005", # Austin
	"CITY:US170006", # Chicago
	"CITY:US260006", # Detroit
	"CITY:US480023", # Fort Worth
	"CITY:US290008", # Kansas City
	"CITY:US060013", # Los Angeles
	"CITY:US210009", # Louisville
	"CITY:US360019", # New York
	"CITY:US060031", # San Francisco
	"CITY:US040014"  # Tucson
)

# get a list of stations in each city
noaa_stations <- weather_cities %>% 
	set_names() %>% 
	map(~ ncdc_stations(locationid = ., limit = 100, sortfield = "maxdate", 
											sortorder = "desc")) %>% 
	pluck("data") %>% 
	map(filter, ymd(mindate) <= ymd("2010-01-01"), datacoverage == 1)

# manually choose a station for each city
weather_stations <- tribble(
	~station, ~station_name, ~city,
	"GHCND:USW00013958", "AUSTIN CAMP MABRY, TX US", "Austin",
	"GHCND:USC00111577", "CHICAGO MIDWAY AIRPORT 3 SW, IL US", "Chicago",
	"GHCND:USW00094847", "DETROIT METRO AIRPORT, MI US", "Detroit",
	"GHCND:USC00413285", "FORT WORTH WSFO, TX US", "Fort Worth",
	"GHCND:USW00013988", "KANSAS CITY DOWNTOWN AIRPORT, MO US", "Kansas City",
	"GHCND:USW00023174", "LOS ANGELES INTERNATIONAL AIRPORT, CA US", "Los Angeles",
	"GHCND:USC00154958", "LOUISVILLE WEATHER FORECAST OFFICE, KY US", "Louisville",
	"GHCND:USW00094728", "NY CITY CENTRAL PARK, NY US", "New York",
	"GHCND:USW00023234", "SAN FRANCISCO INTERNATIONAL AIRPORT, CA US", "San Francisco",
	"GHCND:USW00023160", "TUCSON INTERNATIONAL AIRPORT, AZ US", "Tucson"
)

# define function to return GHCND data as a tibble
# ncdc() can only retrieve 1,000 rows of data, which with 10 cities and four
# data types equates to 25 days of data
get_weather <- function (date, stations) {
	
	pluck(ncdc(
		datasetid = "GHCND", 
		datatypeid = c("PRCP", "SNOW", "TMAX", "TMIN"), 
		stationid = stations,
		startdate = format(date, "%F"), 
		enddate = format(date + days(24), "%F"),
		limit = 1000
	), "data")

}

# get data
# details of the row data format are at 
# <ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt>
weather_data <- seq.Date(ymd("2010-01-01"), ymd("2018-12-31"), 
												 by = "25 days") %>% 
	map_dfr(
		function (x) {
			
			message(paste("Getting data for 25 days starting", format(x, "%d %b %Y")), 
							appendLF = TRUE)
			
			get_weather(x, weather_stations$station)
			
		})

# SFO data are missing for a period so these records can be replaced by values
# from a different site, which can't be used overall because of other missing
# data but which does have values for all the dates when SFO data are missing
sf_weather_data <- get_weather(ymd("2018-06-22"), "GHCND:USW00023272")

# add the replacement SF data to the main dataset
weather_data_processed <- rbind(
	filter(
		weather_data, 
		!(station == "GHCND:USW00023234" & 
				between(as_date(parse_datetime(date)), ymd("2018-06-22"), 
								ymd("2018-07-11")))
	),
	filter(sf_weather_data, between(as_date(parse_datetime(date)), 
																	ymd("2018-06-22"), ymd("2018-07-11")))
) %>% 
	arrange(date)

# process data
weather_data_processed <- weather_data_processed %>% 
	left_join(weather_stations, by = "station") %>% 
	mutate(city = ifelse(station == "GHCND:USW00023272", "San Francisco", 
											 city)) %>% 
	select(-fl_m, -fl_q, -fl_so, -fl_t, -station, -station_name) %>%
	spread(datatype, value) %>%
	janitor::clean_names() %>%
	# snow is very rare and seems to be very often missing from the data, so if
	# there is no snow value set it to zero (LA data don't have values for snow 
	# at all because it hasn't snowed there since 1961)
	mutate(
		date = as_date(date),
		snow = ifelse(is.na(snow), 0, snow)
	) %>%
	filter(between(date, ymd("2010-01-01"), ymd("2018-12-31"))) %>%
	# precipitation and temperature values are in tenths of units (mm and ºC)
	mutate_at(vars("prcp", "tmax", "tmin"), ~ . / 10) %>% 
	as_tsibble(key = city, index = date)

# report missing data
weather_data_processed %>% 
	as_tibble() %>% 
	filter_all(any_vars(is.na(.))) %>% 
	arrange(city, date) %>% 
	write_csv(here::here("fig_output/missing_weather_data.csv"))

# impute missing data
weather_data_imputed <- weather_data_processed %>% 
	# some dates are missing from the data altogether, so we add these
	fill_gaps() %>% 
	# if only some data are missing for a day, the row will exist but the missing
	# values will be NA, which we replace with the rolling mean of a seven-day
	# period centred on the missing date
	mutate_if(
		is.numeric, 
		~ ifelse(
			is.na(.), 
			mean(c(lag(., 3), lag(., 2), lag(.), lead(.), lead(., 2), lead(., 3)), 
					 na.rm = TRUE), 
			.
		)
	)

# save data
write_csv(weather_data_imputed, here::here("data_output/weather_data.csv.gz"))
