# This file runs the analysis to test H2, generating forecasts for each city
# using each method



# SET SEED
# since we will be using sample_n() to choose the dates on which to make the
# forecasts, we can set a seed here to make the sample reproducible
set.seed(123)



# LOAD DATA

# load data
if (!exists("crimes")) {
	
	crimes <- read_csv("data_output/crime_data.csv.gz") %>% 
		filter(!is.na(date_single))
	
}



# PREPARE DATA

# get range of dates for crime data
date_range <- lubridate::interval(
	as_date(min(crimes$date_single)), 
	as_date(max(crimes$date_single))
)

# get holiday dates
holiday_dates <- tibble(
	date = as_date(timeDate::holidayNYSE(2010:2018)),
	holiday = TRUE
)

# count crimes per day
crimes_by_date <- crimes %>% 
	# move early-hour crimes to one day previously, so they are counted with the
	# night shift of the previous day
	mutate(
		date = as_date(date_single),
		date = as_date(ifelse(hour(date_single) < 6, date - days(1), date))
	) %>% 
	count(city_name, date, name = "crimes") %>%
	# some crimes will now be recorded on the last day of the year before the 
	# start of the study period, so we remove these
  filter(date %within% date_range) %>%
	mutate(
		month_last = mday(date + days(1)) == 1,
		year_last = yday(date + days(1)) == 1
	) %>%
	left_join(holiday_dates, by = "date") %>% 
	mutate(holiday = ifelse(is.na(holiday), FALSE, holiday)) %>% 
	as_tsibble(index = date, key = city_name)

models_by_date <- tibble(
	forecast_date = seq.Date(
		as_date(min(crimes$date_single) + years(3)), 
		as_date(max(crimes$date_single) - days(90)), 
		by = "days"
	)
) %>% 
	sample_n(100, replace = FALSE) %>% 
	arrange(forecast_date) %>% 
	mutate(
		training_data = map(
			forecast_date, 
			~ filter(crimes_by_date, 
							 between(date, . - years(3), . - days(1)))
		),
		test_data = map(
			forecast_date,
			~ filter(crimes_by_date,
							 between(date, ., . + days(89)))
		)
	)

# check if models_by_date has the correct format
# models_by_date %>%
# 	mutate(
# 		training_period = map_chr(training_data, function (x) {
# 			paste(first(x$date), "to", last(x$date))
# 		}),
# 		test_period = map_chr(test_data, function (x) {
# 			paste(first(x$date), "to", last(x$date))
# 		})
# 	) %>%
# 	select(-training_data, -test_data) %>%
# 	View()



# RUN MODELS

future::plan("multiprocess")

system.time(
	models_by_date$models <- furrr::future_map(
		models_by_date$training_data, model,
		naive = NAIVE(crimes ~ lag()),
		tslm = TSLM(crimes ~ trend() + season() + holiday + month_last + year_last),
		stl = decomposition_model(STL, crimes ~ trend() + season(),
															ETS(season_adjust), ETS(season_year),
															dcmp_args = list(robust = TRUE)),
		ets = ETS(crimes ~ trend() + season() + error()),
		arima = ARIMA(crimes ~ trend() + season() + holiday + month_last + 
										year_last),
		neural = NNETAR(crimes ~ trend() + season() + AR() + holiday + month_last + 
											year_last),
		fasster = FASSTER(crimes ~ poly(1) + trig(7) + ARMA() + holiday + 
												month_last + year_last),
		.progress = TRUE
	) %>%
		map(mutate, combo = (arima + ets + stl + fasster) / 4)
)



# CALCULATE FORECASTS

models_by_date$forecasts <- furrr::future_map2(
	models_by_date$models, models_by_date$forecast_date,
	function (x, y) {
		
		# generate tsibble of new data for 90 days starting on the forecast date
		new_data <- expand.grid(
			city_name = as.character(unique(x$city_name)),
			date = seq.Date(y, y + days(89), by = "days")
		) %>% 
			arrange(city_name, date) %>% 
			mutate(
				month_last = mday(date + days(1)) == 1,
				year_last = yday(date + days(1)) == 1
			) %>%
			left_join(holiday_dates, by = "date") %>% 
			mutate(holiday = ifelse(is.na(holiday), FALSE, holiday)) %>% 
			as_tsibble(index = date, key = city_name)
		
		# forecast based on new data
		forecast(x, new_data = new_data, bias_adjust = FALSE, times = 0) %>% 
			mutate(
				coef_variation = map_dbl(.distribution, 
																 ~ ifelse(length(.) > 0, .$sd / .$mean, NA))
			)
		
	},
	.progress = TRUE
)



# CALCULATE ACCURACY MEASURES

models_by_date$accuracy <- pmap(
	list(models_by_date$forecasts, models_by_date$training_data, 
			 models_by_date$test_data), 
	~ left_join(
		accuracy(..1, rbind(..2, ..3)),
		as_tibble(..1) %>% 
			group_by(city_name, .model) %>% 
			summarise(coef_var = mean(coef_variation)) %>% 
			ungroup(),
		by = c("city_name", ".model")
	)
)



# SAVE MODELS
models_by_date %>% 
	select(forecast_date, training_data, test_data, forecasts, accuracy) %>% 
	write_rds("data_output/models_h2.Rds", compress = "gz")



# PLOT ACCURACY

model_accuracy <- models_by_date %>% 
	as_tibble() %>% 
	select(forecast_date, accuracy) %>% 
	unnest() %>% 
	select(forecast_date, model = `.model`, city_name, mape = MAPE, coef_var) %>% 
	mutate(
		city_name = str_replace_all(city_name, "\\s+", "\n"),
		mape = mape / 100,
		model = fct_recode(model, `ARIMA` = "arima", `ETS` = "ets", 
											 `FASSTER` = "fasster", `linear model` = "tslm", 
											 `naïve` = "naive", `neural network` = "neural", 
											 `STL` = "stl", `combined` = "combo"),
		city_model = paste(model, "-", city_name)
	)

model_accuracy_means <- model_accuracy %>% 
	group_by(city_name, model, city_model) %>% 
	summarise(median = median(mape), mean_coef_var = mean(coef_var)) %>% 
	arrange(median) %>% 
	ungroup() %>% 
	group_by(city_name) %>% 
	mutate(
		rank = row_number(),
		model_label = case_when(
			rank == 1 ~ 
				paste0("bold(`", model, "`) - ", city_name),
			model == "naïve" ~ 
				paste0("italic(`naïve`) - ", city_name),
			TRUE ~ 
				paste0("`", model, "` - ", city_name)
		)
	) %>% 
	ungroup()

model_accuracy <- left_join(
	model_accuracy,
	select(model_accuracy_means, city_model, model_label),
	by = "city_model"
)

model_accuracy_mean_values <- pluck(model_accuracy_means, "model_label")

model_accuracy_group_means <- model_accuracy %>% 
	group_by(city_name) %>% 
	summarise(median = median(mape), lo = quantile(mape, 0.25), 
						hi = quantile(mape, 0.75)) %>% 
	arrange(median) %>% 
	mutate(city_name = reorder(city_name, median))

model_accuracy_group_mean_values <- pluck(model_accuracy_group_means, 
																					"city_name")

# annotations <- tribble(
# 	~city_name, ~x, ~mape, ~label, ~hjust, ~vjust,
# 	as.character(errors_by_month_mean_values[[1]]), 1.4, max(errors_by_month$mape) * 0.975, "forecast method\n(ordered by median accuracy)", 1, 1,
# 	as.character(errors_by_month_mean_values[[1]]), 1.4, errors_by_month_mean[errors_by_month_mean$city_name == as.character(errors_by_month_mean_values[[1]]), ]$median, "cross-method\nmedian accuracy", 0, 1,
# 	as.character(errors_by_month_mean_values[[1]]), 1.4, errors_by_month_mean[errors_by_month_mean$city_name == as.character(errors_by_month_mean_values[[1]]), ]$hi, "cross-method inter-quartile\nrange of accuracy", 0, 1,
# 	as.character(errors_by_month_mean_values[[1]]), 0.7, quantile(errors_by_month[errors_by_month$city_name == as.character(errors_by_month_mean_values[[1]]) & errors_by_month$model == last(pluck(filter(errors_by_month_cities, city_name == "Tucson"), "model")), ]$mape, 0.85), "single-method\nboxplot of accuracy", 0.5, 1
# )

model_accuracy %>% 
	mutate(
		city_name = fct_relevel(city_name, 
														as.character(model_accuracy_group_mean_values)),
		model_label = fct_rev(fct_relevel(model_label, 
																			as.character(model_accuracy_mean_values)))
	) %>% 
	ggplot(aes(model_label, mape)) + 
	geom_tile(aes(x = 1, y = lo + 0.5 * (hi - lo), height = hi - lo, width = Inf),
						data = model_accuracy_group_means, inherit.aes = FALSE, 
						fill = "grey90") +
	geom_boxplot(colour = "grey33", fill = NA, outlier.size = 0.75) +
	geom_hline(aes(yintercept = median), data = model_accuracy_group_means) +
	geom_hline(aes(yintercept = lo), data = model_accuracy_group_means, 
						 linetype = "12") +
	geom_hline(aes(yintercept = hi), data = model_accuracy_group_means, 
						 linetype = "12") +
	# geom_text_repel(aes(x = x, label = label, hjust = hjust, vjust = vjust),
	# 								data = annotations, size = 9 / (14 / 5), lineheight = 0.9,
	# 								min.segment.length = 0, nudge_x = 1.5, xlim = c(2.25, NA), 
	# 								arrow = arrow(length = unit(4, "pt")), force = 2) +
	scale_x_discrete(
		labels = function (x) parse(text = str_remove(x, " - (.|\n)+$"))
	) +
	scale_y_continuous(
		limits = c(0, 0.5),
		expand = c(0, 0),
		labels = scales::percent_format(accuracy = 1)
	) + 
	coord_flip() +
	facet_grid(rows = vars(city_name), scales = "free_y") +
	labs(x = NULL, y = "mean absolute percentage error") +
	theme_minimal() +
	theme(
		legend.position = "bottom",
		panel.grid.major.y = element_blank(),
		panel.grid.minor.y = element_blank(),
		plot.margin = margin(5.5 * 8, 5.5, 5.5, 5.5, "pt"),
		strip.text.y = element_text(angle = 0, hjust = 0)
	)



# PLOT EXAMPLE FORECASTS

example_data <- models_by_date %>% 
	as_tibble %>% 
	mutate(full_data = map2(training_data, test_data, rbind)) %>% 
	select(forecast_date, full_data) %>% 
	unnest() %>% 
	filter(
		forecast_date == last(forecast_date),
		city_name %in% c("Louisville", "Los Angeles", "Chicago")
	) %>% 
	select(date, city_name, actual_crimes = crimes)

example_means <- example_data %>% 
	group_by(city_name) %>% 
	summarise(mean_crimes = mean(actual_crimes))

example_forecasts <- models_by_date %>% 
	as_tibble() %>% 
	select(forecast_date, forecasts) %>% 
	unnest() %>% 
	filter(
		forecast_date == last(forecast_date), 
		city_name %in% c("Louisville", "Los Angeles", "Chicago")
	) %>% 
	select(-`.distribution`) %>% 
	left_join(example_data, by = c("date", "city_name")) %>% 
	left_join(example_means, by = "city_name") %>% 
	mutate(
		crimes = (crimes - mean_crimes) / mean_crimes,
		actual_crimes = (actual_crimes - mean_crimes) / mean_crimes
	)

example_data <- example_data %>% 
	left_join(example_means, by = "city_name") %>% 
	mutate(
		actual_crimes = (actual_crimes - mean_crimes) / mean_crimes
	)

example_annotations <- tribble(
	~date, ~actual_crimes, ~city_name, ~label, ~hjust, ~`.model`,
	as_date(last(example_forecasts$forecast_date)), -0.75, "Los Angeles",
	"final 90 days of data   \n← used in forecast   ", 1, "ARIMA",
	as_date(last(example_forecasts$forecast_date)), -0.75, "Los Angeles", 
	"   forecast vs.\n   actual values →", 0, "ARIMA"
)

example_forecasts %>% 
	mutate(
		.model = fct_recode(.model, `ARIMA` = "arima", `ETS` = "ets", 
												`FASSTER` = "fasster", `linear model` = "tslm", 
												`naïve` = "naive", `neural network` = "neural", 
												`STL` = "stl", `combined model` = "combo")
	) %>% 
	ggplot(aes(x = date, y = actual_crimes)) + 
	geom_label(aes(label = label, hjust = hjust), data = example_annotations,
						 vjust = "bottom", colour = "grey33", size = 9 / (14 / 5),
						 lineheight = 0.9, label.size = NA) +
	geom_vline(aes(xintercept = as_date(last(example_forecasts$forecast_date))), 
						 linetype = "11") +
	geom_ribbon(aes(ymin = crimes, ymax = actual_crimes), fill = "grey80") + 
	geom_line(data = example_data) +
	geom_line(aes(y = crimes), colour = "grey50") +
	scale_x_date(
		limits = c(first(example_forecasts$forecast_date) - days(90), NA)
	) +
	scale_y_continuous(
		labels = scales::percent_format(accuracy = 1)
	) +
	facet_grid(rows = vars(`.model`), cols = vars(city_name)) +
	labs(
		x = NULL,
		y = "number of crimes (relative to mean count in each city)"
	) +
	theme_minimal() +
	theme(
		axis.ticks.x = element_line(),
		panel.grid.major.x = element_blank(),
		panel.grid.minor.x = element_blank()
	)




