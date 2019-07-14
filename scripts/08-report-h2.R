# This file currently contains code moved from 05-forecast-h2.R, which will need
# to be updated using the lessons learned from 07-report-h1.R

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
