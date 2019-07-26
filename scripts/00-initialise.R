# This file sets up the system for working on this project, and should be run
# every time a new RStudio session begins. All packages should be loaded from
# this file and all custom function defined here.


# if needed, install non-CRAN packages
if (!"nbastatR" %in% installed.packages())
	remotes::install_github("abresler/nbastatR")
if (!"baseballr" %in% installed.packages())
	remotes::install_github("BillPetti/baseballr")
if (!"nflscrapR" %in% installed.packages())
	remotes::install_github("maksimhorowitz/nflscrapR")

# always get latest versions of fable and related packages, because they are in 
# active development
remotes::install_github("tidyverts/fablelite")
remotes::install_github("tidyverts/fable")
remotes::install_github("tidyverts/fasster")
remotes::install_github("mitchelloharawild/fable.prophet")

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
library("rnoaa")     # weather data
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



# identify dummy variables used to train models
training_vars <- function (x) {
	
	dimnames(x$tslm[[1]]$fit$coef)[[1]] %>% 
		str_remove("TRUE$") %>% 
		str_replace("^weekday.+?$", "weekday") %>% 
		unique() %>% 
		str_subset("\\(", negate = TRUE)
	
}



# check if all values in a vector are the same
is_constant <- function (x) {
	
	stopifnot(is.atomic(x))

	if (is.character(x) | is.factor(x)) {
		ifelse(length(unique(as.character(x))) == 1, TRUE, FALSE)
	} else {
		fable:::is.constant(x)
	}
	
}



# calculate probability forecast will be over the threshold value
prob_extreme <- Vectorize(function (dist, threshold) {
	
	# fcdist objects can either be a two-item list with mean and SD, or a numeric
	# vector of bootstrapped samples
	if (length(dist[[1]][[1]]) > 1 & is.numeric(dist[[1]][[1]])) {
		sum(dist[[1]][[1]] > threshold) / length(dist[[1]][[1]])
	} else if (has_name(dist, "mean") & has_name(dist, "sd")) {
		pnorm(threshold, dist$mean, dist$sd, lower.tail = FALSE)	
	} else {
		stop("Could not extract mean and standard distribution from distribution ",
				 "object")
	}

	
})
