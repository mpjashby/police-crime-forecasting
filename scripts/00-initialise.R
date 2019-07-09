# This file sets up the system for working on this project, and should be run
# every time a new RStudio session begins. All packages should be loaded from
# this file and all custom function defined here.


# if needed, install non-CRAN packages
remotes::install_github("abresler/nbastatR")
remotes::install_github("baseballr")
remotes::install_github("maksimhorowitz/nflscrapR")

# load packages
library("baseballr") # MLB data
library("cowplot")   # marginal plots
library("crimedata") # crime data
library("fable")     # forecasting
library("feasts")    # also forecasting
library("fasster")   # yet more forecasting
# library("ggrepel")   # repelled text labels
# library("ggridges")  # ridge plots
library("ggthemes")  # Tufte box plots
library("lubridate") # handle dates
library("fable.prophet") # Facebook forecasting
library("nbastatR")  # NBA data
library("nflscrapR") # NFL data
library("sf")        # spatial processing 
library("tsibble")   # tidy time series
library("tidyverse") # utility functions, **load this after all other packages**



# function for extracting the coefficient of variation from an `fcdist` object
coef_var <- function (dist) {
	
	# stopifnot(inherits(dist, "fcdist"))
	
	# only the first element of the object is of interest
	# dist <- dist[[1]]
	
	# fcdist objects can either be a two-item list with mean and SD, or a numeric
	# vector of bootstrapped samples
	if (length(dist[[1]][[1]]) > 1 & is.numeric(dist[[1]][[1]])) {
		sd(dist[[1]][[1]]) / abs(mean(dist[[1]][[1]]))
	} else if (has_name(dist, "mean") & has_name(dist, "sd")) {
		dist$sd / abs(dist$mean)
	} else {
		NA_real_
	}

}
