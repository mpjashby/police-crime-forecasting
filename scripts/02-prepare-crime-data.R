# This file downloads data from the Crime Open Database for cities for which
# data are available for at least 10 years and stores them in a single table,
# adding a variable corresponding to the district (precinct etc.) in which the
# crime occurred. Fort Worth is excluded because the data only have dates, not
# times.



if (!isNamespaceLoaded("tidyverse")) {
	source(here::here("scripts/00-initialise.R"))
}



# DOWNLOAD CRIME DATA
crimes <- get_crime_data(
	# 5 years training + 1 year forecast horizon + 4 years test
	years = 2010:2019, 
	cities = c(
		"Austin", "Chicago", "Detroit", "Kansas City", "Los Angeles", "Louisville", 
		"Memphis", "New York", "San Francisco", "Seattle", "St Louis", "Tucson"
	),
	type = "core",
	output = "sf"
) %>% 
	select(-uid, -date_start, -date_end, -census_block, -location_type, 
				 -location_category)



# ADD DISTRICT
# Use a projected CRS to avoid spherical geometry problems
districts <- read_sf("data_output/police_boundaries.gpkg") %>% st_transform(2163)
crimes_with_districts <- crimes %>% 
	st_transform(2163) %>% 
	st_join(districts) %>% 
	# remove geometry
	st_drop_geometry() %>% 
	# remove duplicate city column
	select(-city)



# SAVE DATA
write_csv(crimes_with_districts, "data_output/crime_data_with_districts.csv.gz")
