# This file sets up the system for working on this project, and should be run
# every time a new RStudio session begins. All packages should be loaded from
# this file and all custom function defined here.

# if needed, install non-CRAN packages
if (!"nbastatR" %in% installed.packages())
	remotes::install_github("abresler/nbastatR")
if (!"baseballr" %in% installed.packages())
	remotes::install_github("BillPetti/baseballr")
if (!"rMLS" %in% installed.packages())
	remotes::install_github("ryang73/rMLS")

# always get latest versions of fable and related packages, because they are in 
# active development
if (!"fabletools" %in% installed.packages())
	remotes::install_github("tidyverts/fabletools")
if (!"fable" %in% installed.packages())
	remotes::install_github("tidyverts/fable")
if (!"fasster" %in% installed.packages())
	remotes::install_github("tidyverts/fasster")
if (!"feasts" %in% installed.packages())
	remotes::install_github("tidyverts/feasts")
if (!"fable.prophet" %in% installed.packages())
	remotes::install_github("mitchelloharawild/fable.prophet")

# load packages
library("baseballr") # MLB data
library("cowplot")   # marginal plots
library("crimedata") # crime data
library("fable")     # forecasting
library("fasster")   # more forecasting
library("feasts")    # yet more forecasting
# library("ggrepel")   # repelled text labels
# library("ggridges")  # ridge plots
library("ggthemes")  # Tufte box plots
library("lubridate") # handle dates
library("fable.prophet") # Facebook forecasting
library("nbastatR")  # NBA data
library(nflfastR)    # NFL data
library(nhlapi)      # NHL data
library(rMLS)        # MLS data
library("rnoaa")     # weather data
library(rvest)       # HTML scraping
library("sf")        # spatial processing 
library("slackr")    # notifications
library("tsibble")   # tidy time series
library("tidyverse") # utility functions, **load this after all other packages**



# load common helpers
# this must be done after the packages are loaded, since it requires ggplot2
source("https://github.com/mpjashby/r-helper/raw/main/helpers.R")



# setup slackr
# slackr_setup()



# handle errors, warnings etc
send_notification <- function (content = "Finished!", level = "message") {
	
	stopifnot(level %in% c("stop", "error", "warning", "message"))
	if (level == "error") level <- "stop"

	if (Sys.getenv("SLACK_INCOMING_URL_PREFIX") == "") slackr_setup()
	try(slackr_bot(content), silent = TRUE)
	beepr::beep(sound = 8)
	do.call(level, list(content))
	invisible(NULL)
	
}



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
		# warning("Could not extract mean and standard distribution from ",
		# 		 "distribution object")
		NA
	}

})


# function to turn a vector into a printed list
vector_to_text <- function (x, sep = ", ", final_sep = "and") {
	
	paste(paste(x[1:length(x) - 1], collapse = sep), final_sep, x[[length(x)]])
	
}
