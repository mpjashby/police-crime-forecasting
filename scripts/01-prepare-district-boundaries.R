# This file downloads police district boundaries for each city and saves them
# as geopackages. The original downloded boundary files are stored in case they
# are needed later

districts <- list()



# AUSTIN -----------------------------------------------------------------------
# Austin data are in a zipped shapefile. There are three levels of police unit
# nested within one another, so sector will be used as the main level.

# download data
if (!file.exists("data/austin_district_boundaries.zip")) {
	download.file(
		"https://data.austintexas.gov/api/geospatial/9jeg-fsk5?method=export&format=Shapefile", 
		"data/austin_district_boundaries.zip"
	)
}

# process data
districts[["austin"]] <- "data/austin_district_boundaries.zip" %>% 
	unzip(exdir = tempdir()) %>% 
	str_subset(".shp$") %>%
	read_sf() %>% 
	# Merging the districts into sectors doesn't work for lat/lon data because of
	# a problem with some of the geometry, so first convert to a local CRS and 
	# then convert back afterwards for compatibility with other cities
	st_transform(2278) %>% 
	group_by(sector_nam) %>% 
	summarise() %>% 
	select(district = sector_nam) %>% 
	mutate(city = "Austin")



# CHICAGO ----------------------------------------------------------------------
# Chicago data are in GeoJSON format. Police District is the main geography.

# download data
if (!file.exists("data/chicago_district_boundaries.geojson")) {
	download.file(
		"https://data.cityofchicago.org/api/geospatial/fthy-xz3r?method=export&format=GeoJSON", 
		"data/chicago_district_boundaries.geojson"
	)
}

# process data
districts[["chicago"]] <- read_sf("data/chicago_district_boundaries.geojson") %>% 
	select(district = dist_num) %>% 
	mutate(district = str_pad(district, 2, pad = "0"), city = "Chicago")



# DETROIT ----------------------------------------------------------------------

# download data
# Detroit data are in a zipped shapefile.
if (!file.exists("data/detroit_district_boundaries.zip")) {
	download.file(
		"https://opendata.arcgis.com/datasets/d99a3244a8ab4d81911c81ef8a18c7d1_0.zip?outSR=%7B%22latestWkid%22%3A2898%2C%22wkid%22%3A2898%7D", 
		"data/detroit_district_boundaries.zip"
	)
}

# process data
districts[["detroit"]] <- "data/detroit_district_boundaries.zip" %>% 
	unzip(exdir = tempdir()) %>% 
	str_subset(".shp$") %>%
	# these next three lines are needed, rather than `st_read(as_tibble = TRUE)`
	# because the data has duplicate column names and `st_read()` doesn't seem to
	# pass through the .name_repair argument to `as_tibble()` to fix this
	read_sf() %>% 
	select(district = Precinct) %>% 
	mutate(district = str_pad(district, 2, pad = "0"), city = "Detroit")



# FORT WORTH -------------------------------------------------------------------

# download data
# data seem to have been removed from the Fort Worth website, but were still
# available on koordinates.com but behind a login page. The data have been
# manually downloaded in a zipped geopackage so they can be used here. Zone is
# the primary geometry.

# process data
districts[["fort_worth"]] <- "data/fort_worth_district_boundaries.zip" %>% 
	unzip(exdir = "data/temp_fort_worth") %>% 
	str_subset(".gpkg$") %>%
	read_sf() %>% 
	group_by(ZONE) %>% 
	summarise() %>% 
	rename(district = ZONE, geometry = geom) %>% 
	mutate(city = "Fort Worth")



# KANSAS CITY ------------------------------------------------------------------

# Download data
if (!file.exists("data/kansas_city_district_boundaries.zip")) {
	download.file(
		"https://data.kcmo.org/api/geospatial/pxe5-449t?method=export&format=Shapefile",
		"data/kansas_city_district_boundaries.zip"
	)
}

# Process data
districts[["kansas_city"]] <- "data/kansas_city_district_boundaries.zip" %>% 
	unzip(exdir = tempdir()) %>% 
	str_subset(".shp$") %>% 
	read_sf() %>% 
	select(district = divisionna) %>% 
	mutate(city = "Kansas City")



# LOS ANGELES ------------------------------------------------------------------
# LA data are in a zipped shapefile.

# download data
if (!file.exists("data/los_angeles_district_boundaries.zip")) {
	download.file(
		"https://opendata.arcgis.com/datasets/031d488e158144d0b3aecaa9c888b7b3_0.zip",
		"data/los_angeles_district_boundaries.zip"
	)
}

# process data
districts[["los_angeles"]] <- "data/los_angeles_district_boundaries.zip" %>% 
	unzip(exdir = tempdir()) %>%
	str_subset(".shp$") %>%
	read_sf() %>% 
	select(district = APREC) %>% 
	mutate(city = "Los Angeles")



# LOUISVILLE -------------------------------------------------------------------
# Louisville data are in a zipped shapefile.

# download data
if (!file.exists("data/louisville_district_boundaries.zip")) {
	download.file(
		"https://data.louisvilleky.gov/sites/default/files/lmpd_beat.zip",
		"data/louisville_district_boundaries.zip"
	)
}

# process data
districts[["louisville"]] <- "data/louisville_district_boundaries.zip" %>% 
	unzip(exdir = tempdir()) %>%
	str_subset(".shp$") %>%
	read_sf() %>%
	filter(str_detect(DIST_DESC, "^Louisville Metro Police Department")) %>%
	group_by(DISTRICT) %>%
	summarise() %>%
	rename(district = DISTRICT) %>%
	mutate(city = "Louisville")



# MEMPHIS  ---------------------------------------------------------------------

# download data
if (!file.exists("data/memphis_district_boundaries.zip")) {
	download.file(
		"https://data.memphistn.gov/api/geospatial/9xxh-4f2m?method=export&format=Shapefile",
		"data/memphis_district_boundaries.zip"
	)
}

# Process data
districts[["memphis"]] <- "data/memphis_district_boundaries.zip" %>% 
	unzip(exdir = tempdir()) %>% 
	str_subset(".shp$") %>% 
	read_sf() %>% 
	select(district = precinct) %>% 
	mutate(city = "Memphis")



# NEW YORK ---------------------------------------------------------------------
# New York data are in GeoJSON format. Precinct is the main geography.

# download data
if (!file.exists("data/new_york_district_boundaries.geojson")) {
	download.file(
		"https://data.cityofnewyork.us/api/geospatial/78dh-3ptz?method=export&format=GeoJSON", 
		"data/new_york_district_boundaries.geojson"
	)
}

# process data
districts[["new_york"]] <- read_sf("data/new_york_district_boundaries.geojson") %>% 
	select(district = precinct) %>% 
	mutate(district = str_pad(district, 3, pad = "0"), city = "New York")



# SAN FRANCISCO ----------------------------------------------------------------
# San Francisco data are in GeoJSON format. District is the main geography.

# download data
if (!file.exists("data/san_francisco_district_boundaries.geojson")) {
	download.file(
		"https://data.sfgov.org/api/geospatial/wkhw-cjsf?method=export&format=GeoJSON", 
		"data/san_francisco_district_boundaries.geojson"
	)
}

# process data
districts[["san_francisco"]] <- "data/san_francisco_district_boundaries.geojson" %>% 
	read_sf() %>% 
	select(district) %>% 
	mutate(city = "San Francisco")



# SEATTLE  ---------------------------------------------------------------------

# Download data
if (!file.exists("data/seattle_district_boundaries.zip")) {
	download.file(
		"https://data.seattle.gov/api/geospatial/3rtr-jjhz?method=export&format=Shapefile", 
		"data/seattle_district_boundaries.zip"
	)
}

# Process data
districts[["seattle"]] <- "data/seattle_district_boundaries.zip" %>% 
	unzip(exdir = tempdir()) %>% 
	str_subset(".shp$") %>% 
	read_sf() %>% 
	select(district = name) %>% 
	mutate(city = "Seattle")



# TUCSON -----------------------------------------------------------------------
# Tucson data are in a zipped shapefile

# download data
if (!file.exists("data/tucson_district_boundaries.zip")) {
	download.file(
		"https://opendata.arcgis.com/datasets/69ec88bf13844fc58c8164786ad10fa7_50.zip?outSR=%7B%22latestWkid%22%3A2868%2C%22wkid%22%3A2868%7D",
		"data/tucson_district_boundaries.zip"
	)
}

# process data
districts[["tucson"]] <- "data/tucson_district_boundaries.zip" %>% 
	unzip(exdir = tempdir()) %>%
	str_subset(".shp$") %>%
	read_sf() %>% 
	select(district = DIVISION) %>% 
	mutate(city = "Tucson")



# MERGE AND SAVE DATA ----------------------------------------------------------
districts %>% 
	map(st_transform, 4326) %>% 
	bind_rows() %>% 
	select(city, district, geometry) %>% 
	st_write("data_output/police_boundaries.gpkg")
