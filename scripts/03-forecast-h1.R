# This file applies each forecasting method to the data from each city



# LOAD DATA

# load data
if (!exists("crimes")) {
	
	crimes <- read_csv("data_output/crime_data.csv.gz") %>% 
		filter(!is.na(date_single))
	
} else {
	
	crimes <- crimes %>% 
		filter(!is.na(date_single))
	
}



# PREPARE DATA

# count crimes per month in each city
crimes_by_month <- crimes %>% 
	mutate(month = yearmonth(date_single)) %>%
	count(city_name, month, name = "crimes") %>%
	as_tsibble(index = month, key = city_name)

# create tsibble to hold model results and separate data into training/test sets
models_by_month <- expand.grid(
	city_name = unique(crimes_by_month$city_name),
	forecast_date = yearmonth(seq.Date(ymd("2013-01-01"), ymd("2015-12-31"), 
																		 by = "months"))
) %>% 
	as_tsibble(index = forecast_date, key = city_name) %>%
	mutate(
		training_data = map2(forecast_date, city_name, function (x, y) {
			crimes_by_month %>% 
				filter(city_name == y, 
							 as_date(month) >= as_date(x) - years(3), 
							 as_date(month) < as_date(x)) %>% 
				select(-city_name)
		}),
		test_data = map2(forecast_date, city_name, function (x, y) {
			crimes_by_month %>% 
				filter(city_name == y,
							 as_date(month) >= as_date(x),
							 as_date(month) < as_date(x) + years(3)) %>% 
				select(-city_name)
		})
	)

# check that the window functions above have worked
# models_by_month %>% 
# 	mutate(
# 		training_period = map_chr(training_data, function (x) {
# 			paste(first(x$month), "to", last(x$month))
# 		}),
# 		test_period = map_chr(test_data, function (x) {
# 			paste(first(x$month), "to", last(x$month))
# 		}),
# 		forecast_period = map_chr(average, function (x) {
# 			paste(first(x$forecast_period), "to", last(x$forecast_period))
# 		})
# 	) %>% 
# 	select(-training_data, -test_data) %>% 
# 	View()

# set forecast steps to retain for analysis
# the final value of this vector is the maximum forecast horizon
forecast_horizon <- c(25:36)

# create data for H3
# crimes_by_district <- crimes %>% 
# 	filter(!is.na(district)) %>% 
# 	mutate(date = date(date_single)) %>% 
# 	count(city_name, district, date, name = "crimes") %>% 
# 	nest(-city_name)



# The following functions will give the warning "In window.default(x, ...) : 
# 'start' value not changed" once for each city for each type of model, because
# the first month uses the data back to the start of the series, so calling
# window() inside extract_training_data() generates the warning that the start
# of the series has not changed. This can be ignored as long as it only occurs
# once for each city.



# AVERAGE MODEL

# models_by_month <- mutate(models_by_month, average = map(
# 	training_data, 
# 	~ meanf(as.ts(.), h = max(forecast_horizon), lambda = BoxCox.lambda(.), 
# 					biasadj = TRUE)
# ))
# beepr::beep()



# NAIVE MODEL

models_by_month <- mutate(models_by_month, naive = map(
	training_data, 
	~ rwf(as.ts(.), h = max(forecast_horizon), lambda = BoxCox.lambda(.), 
				biasadj = TRUE)
))
beepr::beep()



# NAIVE MODEL WITH DRIFT

# models_by_month <- mutate(models_by_month, naive_drift = map(
# 	training_data, 
# 	~ rwf(as.ts(.), h = max(forecast_horizon), drift = TRUE, 
# 				lambda = BoxCox.lambda(.), biasadj = TRUE)
# ))
# beepr::beep()



# LINEAR REGRESSION MODEL

# make_tslm_data_train <- function (x, y) {
# 	tibble(crimes = as.ts(x)) %>% 
# 		mutate(
# 			month_days = monthdays(crimes),
# 			biz_days = bizdays(crimes)
# 		) %>% 
# 		as.ts(start = c(year(y), month(y)), frequency = 12)
# }
# 
# make_tslm_data_test <- function (x) {
# 	tibble(date = ts(rep(1, max(forecast_horizon)), start = c(year(x), month(x)), 
# 									 frequency = 12)) %>% 
# 		mutate(
# 			month_days = monthdays(date),
# 			biz_days = bizdays(date)
# 		) %>% 
# 		select(-date)
# }

models_by_month <- mutate(models_by_month, lm = map(
	training_data, 
	function (x) {
		crimes <- x
		forecast(tslm(crimes ~ trend + season, lambda = BoxCox.lambda(x), 
									biasadj = TRUE))
	}
))
beepr::beep()



# STL MODEL

models_by_month <- mutate(models_by_month, stl = map(
	training_data, 
	~ stlf(as.ts(.), method = "naive", h = max(forecast_horizon), robust = TRUE, 
				 lambda = BoxCox.lambda(.), biasadj = TRUE)
))
beepr::beep()



# ETS MODEL

models_by_month <- mutate(models_by_month, ets = map(
	training_data, 
	~ forecast(ets(as.ts(.), lambda = BoxCox.lambda(.), biasadj = TRUE), 
						 max(forecast_horizon))
))
beepr::beep()



# (SEASONAL) ARIMA MODEL

make_arima_data <- function (x) {
	
	if (is.Date(x)) {
		x <- ts(rep(1, max(forecast_horizon)), start = c(year(x), month(x)), 
						frequency = 12)
	} else if (is_tsibble(x)) {
		x <- as.ts(x)
	} else {
		stop("x must be either a date or a tsibble")
	}
	
	as.matrix(seasonaldummy(x))
	
}

# is_rank_deficient <- function (x) {
# 	sv <- svd(na.omit(cbind(rep(1, NROW(x)), x)))$d
# 	ifelse(min(sv) / sum(sv) < .Machine$double.eps, TRUE, FALSE)
# }

models_by_month <- mutate(models_by_month, arima = map2(
	training_data, forecast_date,
	function (x, y) {
		forecast(
			auto.arima(as.ts(x), xreg = make_arima_data(x)), 
			h = max(forecast_horizon),
			xreg = make_arima_data(y)
		)
	}
))
beepr::beep()



# TBATS MODEL

models_by_month <- mutate(models_by_month, tbats = map(
	training_data, ~ forecast(tbats(as.ts(.)), h = max(forecast_horizon))
))
beepr::beep()



# NEURAL NETWORK MODEL

models_by_month <- mutate(models_by_month, neural = map(
	training_data,
	~ forecast(nnetar(as.ts(.), lambda = BoxCox.lambda(x), biasadj = TRUE), 
						 h = max(forecast_horizon), PI = TRUE)
))
beepr::beep()



# FASSTER MODEL



# EXTRACT FORECASTS FROM MODELS

forecasts_by_month <- models_by_month %>%
	mutate_at(
		vars(average, naive, naive_drift, lm, stl, ets, arima, neural),
		map2,
		.$test_data,
		function (x, y) {
			x %>%
				forecast_to_tsibble() %>%
				mutate(crimes = y$crimes)
		}
	) %>%
	select(-training_data, -test_data) %>%
	gather(key = "model", value = "data", -city_name, -forecast_date) %>%
	as_tibble() %>% 
	unnest() %>%
	filter(step %in% forecast_horizon) %>% 
	mutate(abs_perc_error = abs((point_forecast - crimes) / crimes))



# SELECT BEST COMBINATION OF MODELS

combinations <- unlist(map(
	1:length(unique(forecasts_by_month$model)), 
	~ combn(unique(forecasts_by_month$model), ., simplify = FALSE)
), recursive = FALSE) %>% 
	map_dfr(function (x) {
		forecasts_by_month %>% 
			filter(model %in% x) %>% 
			group_by(city_name, forecast_date) %>% 
			summarise(model = list(x), terms = length(x), mape = mean(abs_perc_error), 
								hi = max(abs_perc_error))
	}) %>% 
	mutate(model = map_chr(model, paste, collapse = ", "))

combinations %>%
	arrange(mape) %>% 
	mutate(rank = row_number()) %>% 
	group_by(model) %>% 
	mutate(mean_rank = mean(rank)) %>% 
	summarise(mape = mean(mape), mean_rank = mean(mean_rank)) %>% 
	ungroup() %>% 
	filter(mean_rank < quantile(mean_rank, 0.25)) %>%
	mutate(model = fct_reorder(model, mean_rank)) %>% 
	ggplot(aes(model, mape)) + 
	geom_point() + 
	scale_y_continuous(limits = c(0, NA)) +
	coord_flip() + 
	theme_minimal() +
	theme(
		panel.grid.major.y = element_blank()
	)



# PLOT MEAN ABSOLUTE PERCENTAGE ERRORS

errors_by_month <- forecasts_by_month %>% 
	# {
	# 	# add rows to the end of the 
	# 	data <- .
	# 	combined <- data %>% 
	# 		filter(model %in% c("stl", "ets", "arima")) %>% 
	# 		group_by(city_name, forecast_date, step) %>% 
	# 		summarise_if(is.numeric, mean) %>% 
	# 		mutate(model = "combined")
	# 	bind_rows(data, combined)
	# } %>% 
	group_by(city_name, forecast_date, model) %>%
	summarise(mape = mean(abs_perc_error)) %>%
	ungroup() %>% 
	filter(!model %in% c("average", "naive_drift")) %>% 
	mutate(model = fct_recode(model, `ARIMA` = "arima", `ETS` = "ets", 
														`linear model` = "lm", `naïve` = "naive", 
														`neural network` = "neural", `STL` = "stl"))

errors_by_month_mean <- errors_by_month %>% 
	group_by(city_name) %>% 
	summarise(lo = quantile(mape, 0.25), median = median(mape), 
						hi = quantile(mape, 0.75)) %>% 
	arrange(median)

errors_by_month_mean_values <- pluck(errors_by_month_mean, "city_name")

errors_by_month_cities <- errors_by_month %>% 
	group_by(city_name, model) %>% 
	summarise(mean = median(mape)) %>% 
	mutate(city_model = paste(model, city_name)) %>% 
	arrange(mean) %>% 
	group_by(city_name) %>% 
	mutate(rank = row_number()) %>% 
	ungroup()

errors_by_month_cities_values <- pluck(errors_by_month_cities, "city_model")

annotations <- tribble(
	~city_name, ~x, ~mape, ~label, ~hjust, ~vjust,
	as.character(errors_by_month_mean_values[[1]]), 1.4, max(errors_by_month$mape) * 0.975, "forecast method\n(ordered by median accuracy)", 1, 1,
	as.character(errors_by_month_mean_values[[1]]), 1.4, errors_by_month_mean[errors_by_month_mean$city_name == as.character(errors_by_month_mean_values[[1]]), ]$median, "cross-method\nmedian accuracy", 0, 1,
	as.character(errors_by_month_mean_values[[1]]), 1.4, errors_by_month_mean[errors_by_month_mean$city_name == as.character(errors_by_month_mean_values[[1]]), ]$hi, "cross-method inter-quartile\nrange of accuracy", 0, 1,
	as.character(errors_by_month_mean_values[[1]]), 0.7, quantile(errors_by_month[errors_by_month$city_name == as.character(errors_by_month_mean_values[[1]]) & errors_by_month$model == last(pluck(filter(errors_by_month_cities, city_name == "Tucson"), "model")), ]$mape, 0.85), "single-method\nboxplot of accuracy", 0.5, 1
)
	
errors_by_month %>% 
	group_by(model, city_name) %>%
	mutate(
		min = min(mape),
		lo = quantile(mape, 0.1), 
		mean = mean(mape), 
		hi = quantile(mape, 0.9),
		max = max(mape)
	) %>% 
	ungroup() %>%
	mutate(
		label = ifelse(forecast_date == first(forecast_date),
									 paste0(" ", model, " "), NA),
		city_name = fct_rev(fct_relevel(city_name, 
																		as.character(errors_by_month_mean_values))),
		city_model = fct_rev(fct_relevel(paste(model, city_name), 
																		 errors_by_month_cities_values))
	) %>% 
	ggplot(aes(x = 1, y = mape)) +
	geom_tile(aes(y = lo + 0.5 * (hi - lo), height = hi - lo, width = Inf), 
						data = errors_by_month_mean, fill = "grey90") +
	geom_hline(aes(yintercept = median), data = errors_by_month_mean) +
	geom_hline(aes(yintercept = lo), data = errors_by_month_mean, linetype = "12") +
	geom_hline(aes(yintercept = hi), data = errors_by_month_mean, linetype = "12") +
	geom_boxplot(aes(group = city_model), colour = "grey33", fill = NA, 
							 outlier.size = 0.75, 
							 position = position_dodge2(width = 0.8, padding = 0.4)) + 
	# geom_pointrange(aes(y = mean, ymin = lo, ymax = hi, group = city_model), 
	# 								fatten = 1.5, colour = "grey75", 
	# 								position = position_dodge(width = 0.8)) +
	# geom_point(aes(group = city_model), shape = "|", size = 1, 
	# 					 position = position_dodge(width = 0.8)) +
	geom_text(aes(y = max(mape) * 1.05, group = city_model, label = label), 
						hjust = 1, colour = "grey33", position = position_dodge(width = 0.8), 
						size = 8 / (14 / 5), na.rm = TRUE) +
	geom_text_repel(aes(x = x, label = label, hjust = hjust, vjust = vjust),
									data = annotations, size = 9 / (14 / 5), lineheight = 0.9,
									min.segment.length = 0, nudge_x = 1.5, xlim = c(2.25, NA), 
									arrow = arrow(length = unit(4, "pt")), force = 2) +
	scale_y_continuous(limits = c(0, NA), expand = c(0, 0), 
										 labels = scales::percent_format(accuracy = 1)) +
	coord_flip(clip = FALSE) +
	facet_grid(vars(fct_rev(city_name))) +
	labs(x = NULL, y = "mean absolute percentage error") +
	theme_minimal() +
	theme(
		axis.text.y = element_blank(),
		legend.position = "bottom",
		panel.grid.major.y = element_blank(),
		panel.grid.minor.y = element_blank(),
		plot.margin = margin(5.5 * 8, 5.5, 5.5, 5.5, "pt")
	)



# PLOT EXAMPLE FORECASTS

example_ts <- as.ts(models_by_month$training_data[[1]])
example_ts_train <- window(example_ts, end = c(2012, 12))
example_horizon <- 36 # maximum months to forecast

example_models <- list(
	`mean value` = 
		meanf(example_ts_train, h = example_horizon, 
					lambda = BoxCox.lambda(example_ts_train), biasadj = TRUE),
	`naïve` = 
		rwf(example_ts_train, h = example_horizon, 
				lambda = BoxCox.lambda(example_ts_train), biasadj = TRUE, drift = FALSE),
	`naïve with drift` = 
		rwf(example_ts_train, h = example_horizon, 
				lambda = BoxCox.lambda(example_ts_train), biasadj = TRUE, drift = TRUE),
	`linear regression` = 
		tslm(example_ts_train ~ trend + season, 
				 lambda = BoxCox.lambda(example_ts_train), biasadj = TRUE) %>% 
		forecast(h = example_horizon),
	`time-series decomposition (STL)` = 
		tslm(example_ts_train ~ trend + season, 
				 lambda = BoxCox.lambda(example_ts_train), biasadj = TRUE) %>% 
		forecast(h = example_horizon),
	`exponential smoothing (ETS)` = 
		ets(example_ts_train, lambda = BoxCox.lambda(example_ts_train), 
				biasadj = TRUE) %>% forecast(h = example_horizon),
	`seasonal ARIMA` = 
		auto.arima(example_ts_train) %>% forecast(h = example_horizon),
	`TBATS` = tbats(example_ts_train) %>% forecast(h = example_horizon)
) %>% 
	map(combine_forecast_data, example_ts) %>% 
	bind_rows(.id = "model") %>% 
	{
		combined <- filter(., !model %in% c("mean value", "naïve", 
																				"naïve with drift")) %>% 
			group_by(date, year, month, period) %>% 
			summarise_all(mean, na.rm = TRUE) %>% 
			mutate(model = "combined model")
		bind_rows(., combined)
	} %>% 
	group_by(model) %>% 
	mutate(
		ape = ifelse(date == last(date), scales::percent(abs((crimes - point_forecast) / crimes)), NA)
	)

example_annotations <- tribble(
	~date, ~crimes, ~label, ~hjust, ~model,
	ymd("2013-01-01"), 0, "data used   \n← in forecast   ", 1, "combined model",
	ymd("2013-01-01"), 0, "   forecast vs.\n   actual values →", 0, "combined model"
)

ggplot(example_models, aes(x = date, y = crimes)) +
	# geom_ribbon(aes(ymin = lo_95, ymax = hi_95), fill = "grey85") +
	# geom_ribbon(aes(ymin = lo_80, ymax = hi_80), fill = "grey70") +
	geom_vline(aes(xintercept = ymd("2016-01-01")), linetype = "11") +
	geom_ribbon(aes(ymin = crimes, ymax = point_forecast), fill = "grey80", 
							na.rm = TRUE) +
	geom_line(aes(y = point_forecast), colour = "grey50", na.rm = TRUE) +
	geom_line() +
	geom_text(aes(label = label, hjust = hjust), data = example_annotations, 
						vjust = "bottom", colour = "grey33", size = 9 / (14 / 5), 
						lineheight = 0.9) + 
	geom_label_repel(aes(y = point_forecast, label = ape), hjust = "right", 
									 vjust = "bottom", colour = "grey33", size = 9 / (14 / 5), 
									 na.rm = TRUE, box.padding = 3, min.segment.length = 0) +
	scale_y_continuous(labels = scales::comma_format()) +
	facet_wrap(vars(model)) +
	labs(
		x = NULL,
		y = "number of crimes"
	) +
	theme_minimal() +
	theme(
		axis.ticks.x = element_line(),
		panel.grid.major.x = element_blank(),
		panel.grid.minor.x = element_blank()
	)

