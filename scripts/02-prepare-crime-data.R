# This file downloads data from the Crime Open Database for cities for which
# data are available for at least 10 years and stores them in a single table,
# adding a variable corresponding to the district (precinct etc.) in which the
# crime occurred. Fort Worth is excluded because the data only have dates, not
# times.



# DOWNLOAD CRIME DATA
crimes <- get_crime_data(
	years = 2010:2018, # 5 years training + 1 year forecast horizon + 3 years test
	cities = c("Austin", "Chicago", "Detroit", "Kansas City", "Los Angeles", 
						 "Louisville", "New York", "San Francisco", "Tucson"),
	type = "core",
	output = "sf"
) %>% 
	select(-uid, -date_start, -date_end, -census_block, -location_type, 
				 -location_category)



# ADD DISTRICT
districts <- st_read("data_output/police_boundaries.gpkg") %>% 
	st_transform(102003)

crimes <- crimes %>% 
	st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% 
	# transform to Albers equal area projection because st_join() assumes planar
	# co-ordinates
	st_transform(102003) %>% 
	st_join(districts) %>% 
	# remove geometry
	st_drop_geometry() %>% 
	# remove duplicate city column
	select(-city)



# SAVE DATA
crimes <- crimes %>% 
	write_csv("data_output/crime_data.csv.gz")
