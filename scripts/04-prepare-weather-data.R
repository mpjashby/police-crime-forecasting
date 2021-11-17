# This script downloads weather data and stores it

# Note a NOAA NCDC API key is needed to download weather data -- it is stored in
# the .Renviron file as NOAA_KEY

# Get a list of codes for cities with weather stations
# Note: by ordering the results in descending order of the station ID, we get
# the US stations near the top of the results and so can restrict the number of
# records retrieved to 1,000
noaa_cities <- ncdc_locs(
	datasetid = "GHCND", 
	locationcategoryid = "CITY",
	startdate = "2010-01-01", enddate = "2019-12-31",
	sortfield = "id", 
	sortorder = "desc", 
	limit = 1000
) %>% 
	pluck("data") %>% 
	filter(str_detect(id, "CITY:US")) %>% 
	arrange(name) %>% 
	as_tibble()

# from this list, manually (using `str_detect()` in `filter()`) choose the 
# weather stations of interest
weather_cities <- c(
	"CITY:US480005", # Austin
	"CITY:US170006", # Chicago
	"CITY:US260006", # Detroit
	"CITY:US290008", # Kansas City
	"CITY:US060013", # Los Angeles
	"CITY:US210009", # Louisville
	"CITY:US470013", # Memphis
	"CITY:US360019", # New York
	"CITY:US060031", # San Francisco
	"CITY:US530018", # Seattle
	"CITY:US290021", # St Louis
	"CITY:US040014"  # Tucson
)

# get a list of stations in each city
noaa_stations <- weather_cities %>% 
	set_names() %>% 
	map(~ ncdc_stations(locationid = ., limit = 100, sortfield = "maxdate", 
											sortorder = "desc")) %>% 
	pluck("data") %>% 
	map(filter, ymd(mindate) <= ymd("2010-01-01"), datacoverage == 1)

# manually choose a station for each city (stations beginning USW preferred)
weather_stations <- tribble(
	~station, ~station_name, ~city,
	"GHCND:USW00013958", "AUSTIN CAMP MABRY, TX US", "Austin",
	"GHCND:USW00014819", "CHICAGO MIDWAY AIRPORT 3 SW, IL US", "Chicago",
	"GHCND:USW00094847", "DETROIT METRO AIRPORT, MI US", "Detroit",
	"GHCND:USW00013988", "KANSAS CITY DOWNTOWN AIRPORT, MO US", "Kansas City",
	"GHCND:USW00023174", "LOS ANGELES INTERNATIONAL AIRPORT, CA US", "Los Angeles",
	"GHCND:USW00013810", "LOUISVILLE BOWMAN FIELD, KY US", "Louisville",
	"GHCND:USW00013893", "MEMPHIS INTERNATIONAL AIRPORT, TN US", "Memphis",
	"GHCND:USW00094728", "NY CITY CENTRAL PARK, NY US", "New York",
	"GHCND:USW00023234", "SAN FRANCISCO INTERNATIONAL AIRPORT, CA US", "San Francisco",
	"GHCND:USW00024234", "SEATTLE BOEING FIELD, WA US", "Seattle",
	"GHCND:USW00013994", "ST. CHARLES 2.3 NE, MO US", "St Louis",
	"GHCND:USW00023160", "TUCSON INTERNATIONAL AIRPORT, AZ US", "Tucson"
)

# define function to return GHCND data as a tibble
# ncdc() can only retrieve 1,000 rows of data, which with 12 cities and four
# data types equates to 20 days of data
get_weather <- function (date, stations) {
	
	pluck(ncdc(
		datasetid = "GHCND", 
		datatypeid = c("PRCP", "SNOW", "TMAX", "TMIN"), 
		stationid = stations,
		startdate = format(date, "%F"), 
		enddate = format(date + days(19), "%F"),
		limit = 1000
	), "data")

}

# get data
# details of the row data format are at 
# <ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt>
weather_data <- seq.Date(ymd("2010-01-01"), ymd("2019-12-31"), by = "20 days") %>% 
	map_dfr(
		function (x) {
			
			message(paste("Getting data for 20 days starting", format(x, "%d %b %Y")), 
							appendLF = TRUE)
			
			get_weather(x, weather_stations$station)
			
		})

# Process data
weather_data_processed <- weather_data %>% 
	left_join(weather_stations, by = "station") %>% 
	mutate(date = as_date(date)) %>%
	select(date, city, datatype, value) %>% 
	filter(between(date, ymd("2010-01-01"), ymd("2019-12-31"))) %>%
	pivot_wider(names_from = datatype, values_from = value) %>% 
	janitor::clean_names() %>% 
	# Snow is very rare and seems to be very often missing from the data, so if
	# there is no snow value set it to zero (LA data don't have values for snow 
	# at all because it hasn't snowed there since 1961)
	replace_na(list(snow = 0)) %>% 
	# Precipitation and temperature values are in tenths of units (mm and ºC)
	mutate(across(c(prcp, tmax, tmin), ~ . / 10)) %>% 
	as_tsibble(index = date, key = city)

# Store missing weather data for later reporting
weather_data_processed %>% 
	scan_gaps() %>% 
	write_csv(here::here("data_output/weather_data_missing.csv"))

# Fill any gaps in the time series (both missing rows and missing individual
# values) with the 7-day centre-weighted moving average
weather_data_imputed <- weather_data_processed %>% 
	fill_gaps() %>% 
	mutate(across(
		where(is.numeric), 
		~ slider::slide_dbl(., .f = mean, .before = 3, .after = 3)
	))
	
# save data
write_csv(weather_data_imputed, here::here("data_output/weather_data.csv.gz"))
