# This file downloads police district boundaries for each city and saves them
# as geopackages. The original doanloded boundary files are stored in case they
# are needed later

districts <- list()



# AUSTIN
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
districts[["austin"]] <- unzip("data/austin_district_boundaries.zip", 
															 exdir = "data/temp_austin") %>% 
	str_subset(".shp$") %>%
	# these next three lines are needed, rather than `st_read(as_tibble = TRUE)`
	# because the data has duplicate column names and `st_read()` doesn't seem to
	# pass through the .name_repair argument to `as_tibble()` to fix this
	st_read() %>% 
	as_tibble() %>% 
	st_as_sf() %>% 
	group_by(sector_nam) %>% 
	summarise() %>% 
	rename(district = sector_nam)

# clean up
unlink("data/temp_austin", recursive = TRUE)



# CHICAGO
# Chicago data are in GeoJSON format. Police District is the main geography.

# download data
if (!file.exists("data/chicago_district_boundaries.geojson")) {
	download.file(
		"https://data.cityofchicago.org/api/geospatial/fthy-xz3r?method=export&format=GeoJSON", 
		"data/chicago_district_boundaries.geojson"
	)
}

# process data
districts[["chicago"]] <- st_read("data/chicago_district_boundaries.geojson", 
																	as_tibble = TRUE) %>% 
	select(district = dist_num) %>% 
	mutate(district = str_pad(district, 2, pad = "0"))



# DETROIT

# download data
# Detroit data are in a zipped shapefile.
if (!file.exists("data/detroit_district_boundaries.zip")) {
	download.file(
		"https://opendata.arcgis.com/datasets/d99a3244a8ab4d81911c81ef8a18c7d1_0.zip?outSR=%7B%22latestWkid%22%3A2898%2C%22wkid%22%3A2898%7D", 
		"data/detroit_district_boundaries.zip"
	)
}

# process data
districts[["detroit"]] <- unzip("data/detroit_district_boundaries.zip", 
																exdir = "data/temp_detroit") %>% 
	str_subset(".shp$") %>%
	# these next three lines are needed, rather than `st_read(as_tibble = TRUE)`
	# because the data has duplicate column names and `st_read()` doesn't seem to
	# pass through the .name_repair argument to `as_tibble()` to fix this
	st_read(as_tibble = TRUE) %>% 
	select(district = Precinct) %>% 
	mutate(district = str_pad(district, 2, pad = "0"))

# clean up
unlink("data/temp_detroit", recursive = TRUE)



# FORT WORTH

# download data
# data seem to have been removed from the Fort Worth website, but were still
# available on koordinates.com but behind a login page. The data have been
# manually downloaded in a zipped geopackage so they can be used here. Zone is
# the primary geometry.

# process data
districts[["fort_worth"]] <- unzip("data/fort_worth_district_boundaries.zip", 
																	 exdir = "data/temp_fort_worth") %>% 
	str_subset(".gpkg$") %>%
	st_read(as_tibble = TRUE) %>% 
	group_by(ZONE) %>% 
	summarise() %>% 
	rename(district = ZONE, geometry = geom)

# clean up
unlink("data/temp_fort_worth", recursive = TRUE)



# KANSAS CITY
# KCMO data don't seem to be available



# LOS ANGELES
# LA data are in a zipped shapefile.

# download data
if (!file.exists("data/los_angeles_district_boundaries.zip")) {
	download.file(
		"https://opendata.arcgis.com/datasets/031d488e158144d0b3aecaa9c888b7b3_0.zip",
		"data/los_angeles_district_boundaries.zip"
	)
}

# process data
districts[["los_angeles"]] <- unzip("data/los_angeles_district_boundaries.zip",
																		exdir = "data/temp_los_angeles") %>%
	str_subset(".shp$") %>%
	st_read(as_tibble = TRUE) %>% 
	select(district = APREC)

# clean up
unlink("data/temp_los_angeles", recursive = TRUE)



# LOUISVILLE
# Louisville data are in a zipped shapefile.

# download data
if (!file.exists("data/louisville_district_boundaries.zip")) {
	download.file(
		"https://data.louisvilleky.gov/sites/default/files/lmpd_beat.zip",
		"data/louisville_district_boundaries.zip"
	)
}

# process data
districts[["louisville"]] <- unzip("data/louisville_district_boundaries.zip",
															 exdir = "data/temp_louisville") %>%
	str_subset(".shp$") %>%
	st_read(as_tibble = TRUE) %>%
	filter(str_detect(DIST_DESC, "^Louisville Metro Police Department")) %>%
	group_by(DISTRICT) %>%
	summarise() %>%
	rename(district = DISTRICT) %>%
	st_transform(4326)

# clean up
unlink("data/temp_louisville", recursive = TRUE)



# NEW YORK
# New York data are in GeoJSON format. Precinct is the main geography.

# download data
if (!file.exists("data/new_york_district_boundaries.geojson")) {
	download.file(
		"https://data.cityofnewyork.us/api/geospatial/78dh-3ptz?method=export&format=GeoJSON", 
		"data/new_york_district_boundaries.geojson"
	)
}

# process data
districts[["new_york"]] <- st_read("data/new_york_district_boundaries.geojson", 
																	 as_tibble = TRUE) %>% 
	select(district = precinct) %>% 
	mutate(district = str_pad(district, 3, pad = "0"))



# SAN FRANCISCO
# San Francisco data are in GeoJSON format. District is the main geography.

# download data
if (!file.exists("data/san_francisco_district_boundaries.geojson")) {
	download.file(
		"https://data.sfgov.org/api/geospatial/wkhw-cjsf?method=export&format=GeoJSON", 
		"data/san_francisco_district_boundaries.geojson"
	)
}

# process data
districts[["san_francisco"]] <- st_read(
	"data/san_francisco_district_boundaries.geojson", 
	as_tibble = TRUE
) %>% 
	select(district)



# TUCSON
# Tucson data are in a zipped shapefile

# download data
if (!file.exists("data/tucson_district_boundaries.zip")) {
	download.file(
		"https://opendata.arcgis.com/datasets/69ec88bf13844fc58c8164786ad10fa7_50.zip?outSR=%7B%22latestWkid%22%3A2868%2C%22wkid%22%3A2868%7D",
		"data/tucson_district_boundaries.zip"
	)
}

# process data
districts[["tucson"]] <- unzip("data/tucson_district_boundaries.zip",
															 exdir = "data/temp_tucson") %>%
	str_subset(".shp$") %>%
	st_read(as_tibble = TRUE) %>% 
	select(district = DIVISION)

# clean up
unlink("data/temp_tucson", recursive = TRUE)



# MERGE AND SAVE DATA
districts %>% 
	map(st_transform, st_crs(districts[[1]])) %>% # transform all to same CRS
	map(st_cast) %>% # convert all geometries to multipolygon
	map(mutate, district = as.character(district)) %>% 
	map(as_tibble) %>% 
	bind_rows(.id = "city") %>% 
	st_as_sf(crs = 4326) %>% 
	mutate(city = str_to_title(str_replace_all(city, "_", " "))) %>% 
	st_write("data_output/police_boundaries.gpkg")
