# This file downloads data from the Crime Open Database for cities for which
# data are available for at least 10 years and stores them in a single table,
# adding a variable corresponding to the district (precinct etc.) in which the
# crime occurred.



# DOWNLOAD CRIME DATA
crimes <- get_crime_data(
	years = 2010:2018, # 5 years training + 1 year forecast horizon + 3 years test
	cities = c("Austin", "Chicago", "Detroit", "Fort Worth", "Kansas City", 
						 "Los Angeles", "Louisville", "New York", "San Francisco", 
						 "Tucson"),
	type = "core",
	output = "sf"
) %>% 
	select(-uid, -date_start, -date_end, -census_block, -location_type, 
				 -location_category)



# ADD DISTRICT
# districts <- st_read("data_output/police_boundaries.gpkg")
# crimes <- st_join(crimes, districts)



# SAVE DATA
crimes <- crimes %>% 
	# select(-city) %>% 
	st_drop_geometry() %>% 
	write_csv("data_output/crime_data.csv.gz")
