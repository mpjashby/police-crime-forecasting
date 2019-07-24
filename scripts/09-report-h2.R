# this file plots the results of the models produced for H2
# this is in a separate file from the models so that the models can be 
# estimated in parallel on a powerful machine rather than on a laptop



# LOAD MODEL OBJECT IF NECESSARY

if (!exists("models_by_date")) {
	
	models_by_date <- read_rds("data_output/models_h2.Rds")
	
}



# PLOT ACCURACY, CERTAINTY AND COMPLETENESS

# This plot consists of three panels, each showing a different evaluation of the
# models, below which are two panels showing how to interpret the main panels

# construct a summary tibble containing the three criteria for each model for
# each city at each time period, then format model and city names
model_summary <- models_by_date %>% 
	as_tibble() %>% 
	mutate(accuracy = map2(accuracy, portmanteau, map2, left_join, 
												 by = c("city_name", ".model"))) %>% 
	select(forecast_date, accuracy) %>% 
	unnest() %>% 
	unnest() %>% 
	select(forecast_date, model = `.model`, city_name, mape = MAPE, coef_var, 
				 lb_test = lb_stat) %>% 
	mutate(
		city_name = str_replace_all(city_name, "\\s+", "\n"),
		mape = mape / 100,
		model = fct_recode(model, `ARIMA` = "arima", `ETS` = "ets", 
											 `FASSTER` = "fasster", `linear mod` = "tslm", 
											 `naïve` = "naive", `neural net` = "neural", 
											 `STL` = "stl", `combined` = "combo", 
											 `seas naïve` = "snaive"),
		city_model = paste(model, "-", city_name)
	)

# calculate median value for each summary statistic
model_summary_means <- model_summary %>% 
	group_by(city_name, model, city_model) %>% 
	summarise(
		median_acc = median(mape), 
		median_cer = median(coef_var),
		median_cpl = median(lb_test),
		cpl_95 = quantile(lb_test, 0.95, na.rm = TRUE)
	)

# order the models within each city by accuracy, highlighting best model and
# naïve model for each city
model_summary_means_a <- model_summary_means %>% 
	arrange(median_acc) %>% 
	ungroup() %>% 
	group_by(city_name) %>% 
	mutate(
		rank = row_number(),
		model_label = case_when(
			rank == 1 ~ paste0("bold(`", model, "`) - ", city_name),
			model == "naïve" ~ paste0("italic(`naïve`) - ", city_name),
			TRUE ~ paste0("`", model, "` - ", city_name)
		)
	) %>% 
	ungroup() %>% 
	mutate(model_label = fct_reorder(model_label, row_number()))

# order the models within each city by certainty, highlighting best model and
# naïve model for each city
model_summary_means_b <- model_summary_means %>% 
	arrange(median_cer) %>% 
	ungroup() %>% 
	group_by(city_name) %>% 
	mutate(
		rank = row_number(),
		model_label = case_when(
			rank == 1 ~ paste0("bold(`", model, "`) - ", city_name),
			model == "naïve" ~ paste0("italic(`naïve`) - ", city_name),
			TRUE ~ paste0("`", model, "` - ", city_name)
		)
	) %>% 
	ungroup() %>% 
	mutate(model_label = fct_reorder(model_label, row_number()))

# order the models within each city by completeness, highlighting best model and
# naïve model for each city
model_summary_means_c <- model_summary_means %>% 
	arrange(median_cpl) %>% 
	ungroup() %>% 
	group_by(city_name) %>% 
	mutate(
		rank = row_number(),
		model_label = case_when(
			rank == 1 ~ paste0("bold(`", model, "`) - ", city_name),
			model == "naïve" ~ paste0("italic(`naïve`) - ", city_name),
			TRUE ~ paste0("`", model, "` - ", city_name)
		)
	) %>% 
	ungroup() %>% 
	mutate(
		model_label = fct_reorder(model_label, row_number()),
		sig = ifelse(cpl_95 > 18.33, TRUE, FALSE)
	)

# order cities by the overall accuracy of all models in them
model_summary_overall_means <- model_summary %>% 
	group_by(city_name) %>% 
	summarise(
		median_acc = median(mape, na.rm = TRUE), 
		lo_acc = quantile(mape, 0.25, na.rm = TRUE), 
		hi_acc = quantile(mape, 0.75, na.rm = TRUE),
		median_cer = median(coef_var, na.rm = TRUE),
		lo_cer = quantile(coef_var, 0.25, na.rm = TRUE),
		hi_cer = quantile(coef_var, 0.75, na.rm = TRUE),
		median_cpl = median(lb_test, na.rm = TRUE),
		lo_cpl = quantile(lb_test, 0.25, na.rm = TRUE),
		hi_cpl = quantile(lb_test, 0.75, na.rm = TRUE)
	) %>% 
	arrange(median_acc) %>% 
	mutate(city_name = reorder(city_name, median_acc))

# reorder city_name and model in summary tibble
model_summary <- model_summary %>% 
	mutate(
		city_name = fct_relevel(
			city_name, 
			as.character(pluck(model_summary_overall_means, "city_name"))
		)
	)

# create function for parsing facet labels
parse_facet_label <- function (x) parse(text = str_remove(x, " - (.|\n)+$"))

# specify common theme for panels
theme_panel <- function (...) {
	theme_minimal(base_family = "Arial", ...) %+replace%
		theme(
			axis.title.x = element_text(size = 10),
			axis.text.y = element_text(size = 7, hjust = 1),
			legend.position = "bottom",
			panel.grid.major.y = element_blank(),
			panel.grid.minor.y = element_blank(),
			plot.margin = margin(5.5 * 3.5, 5.5, 5.5, 5.5, "pt"),
			strip.placement = "outside",
			# strip.text.y = element_text(angle = 180, hjust = 1)
			strip.text.y = element_blank()
		)
}



# construct accuracy panel of plot
accuracy_plot_a <- model_summary %>% 
	left_join(select(model_summary_means_a, city_model, model_label), 
						by = "city_model") %>%
	mutate(
		model_label = fct_rev(fct_relevel(
			model_label,
			as.character(fct_rev(pluck(model_summary_means_a, "model_label")))
		))
	) %>%
	ggplot(aes(model_label, mape)) + 
	geom_hline(aes(yintercept = 0), colour = "grey67") + 
	geom_tile(aes(x = 1, y = lo_acc + 0.5 * (hi_acc - lo_acc), 
								height = hi_acc - lo_acc, width = Inf),
						data = model_summary_overall_means, inherit.aes = FALSE, 
						fill = "grey90") +
	geom_segment(aes(xend = model_label, y = median_acc, yend = 0), 
							 data = model_summary_means_a, colour = "grey67", 
							 linetype = "13") +
	geom_tufteboxplot(colour = "grey33", median.type = "line", hoffset = 0, 
										voffset = 0.005, width = 2, outlier.size = 0.25,
										position = position_identity()) +
	geom_hline(aes(yintercept = median_acc), data = model_summary_overall_means) +
	scale_x_discrete(labels = parse_facet_label) +
	scale_y_continuous(
		limits = c(0, as.numeric(quantile(model_summary$mape, 0.99))),
		expand = c(0, 0),
		labels = scales::percent_format(accuracy = 1)
	) + 
	coord_flip() +
	facet_grid(rows = vars(city_name), scales = "free_y", switch = "y") +
	labs(x = NULL, y = "mean absolute percentage error") +
	theme_panel()

# construct certainty panel of plot
accuracy_plot_b <- model_summary %>% 
	left_join(select(model_summary_means_b, city_model, model_label), 
						by = "city_model") %>%
	mutate(
		model_label = fct_rev(fct_relevel(
			model_label,
			as.character(fct_rev(pluck(model_summary_means_b, "model_label")))
		))
	) %>%
	arrange(as.numeric(model_label)) %>% 
	group_by(city_name) %>% 
	mutate(
		city_label = ifelse(
			model_label == last(model_label) & forecast_date == first(forecast_date), 
			as.character(city_name), 
			NA
		)
	) %>% 
	ungroup() %>% 
	ggplot(aes(model_label, abs(coef_var))) +
	geom_hline(aes(yintercept = 0), colour = "grey67") + 
	geom_tile(aes(x = 1, y = lo_cer + 0.5 * (hi_cer - lo_cer), 
								height = hi_cer - lo_cer, width = Inf),
						data = model_summary_overall_means, inherit.aes = FALSE, 
						fill = "grey90") +
	geom_tufteboxplot(colour = "grey33", median.type = "line", hoffset = 0, 
										voffset = 0.005, width = 2, outlier.size = 0.25,
										position = position_identity()) +
	geom_hline(aes(yintercept = median_cer), data = model_summary_overall_means) +
	geom_label(
		aes(x = nrow(model_summary_overall_means), 
				y = as.numeric(quantile(model_summary$coef_var, 0.99)), 
				label = city_label), 
		inherit.aes = FALSE, na.rm = TRUE, 
		hjust = 1, vjust = 1, size = 8 / (14 / 5), lineheight = 0.9, label.size = NA 
	) + 
	coord_flip() +
	scale_x_discrete(labels = parse_facet_label) +
	scale_y_continuous(
		limits = c(0, as.numeric(quantile(abs(model_summary$coef_var), 0.99))), 
		expand = c(0, 0)
	) + 
	facet_grid(rows = vars(city_name), scales = "free_y") +
	labs(x = NULL, y = "coefficient of variation") +
	theme_panel()

# construct completeness panel of plot
accuracy_plot_c <- model_summary %>% 
	left_join(select(model_summary_means_c, city_model, model_label, sig), 
						by = "city_model") %>%
	mutate(
		model_label = fct_rev(fct_relevel(
			model_label,
			as.character(fct_rev(pluck(model_summary_means_c, "model_label")))
		))
	) %>%
	ggplot(aes(model_label, lb_test, colour = sig)) +
	geom_hline(aes(yintercept = 0), colour = "grey67") + 
	geom_tile(aes(x = 1, y = lo_cpl + 0.5 * (hi_cpl - lo_cpl), 
								height = hi_cpl - lo_cpl, width = Inf),
						data = model_summary_overall_means, inherit.aes = FALSE, 
						fill = "grey90") +
	geom_tufteboxplot(median.type = "line", hoffset = 0,
										voffset = 0.005, width = 2, outlier.size = 0.25,
										position = position_identity(), 
										key_glyph = draw_key_path) +
	geom_hline(aes(yintercept = median_cpl), data = model_summary_overall_means) +
	geom_hline(aes(yintercept = 18.33), linetype = "21") + 
	coord_flip() +
	scale_x_discrete(labels = parse_facet_label) +
	scale_y_continuous(
		limits = c(0, as.numeric(quantile(model_summary$lb_test, 0.99))), 
		expand = c(0, 0)
	) + 
	scale_colour_manual(
		values = c(`TRUE` = "grey33", `FALSE` = "grey60"),
		# labels = c(`TRUE` = bquote(italic(p)<0.05 ~ "in" ~ phantom()>plain(`95%`) ~ "of tests"), 
		# 					 `FALSE` = bquote(italic(p)<0.05 ~ "in" ~ phantom()<plain(`95%`) ~ "of tests"))
	) + 
	facet_grid(rows = vars(city_name), scales = "free_y") +
	labs(x = NULL, y = "Ljung–Box statistic") +
	theme_panel() +
	theme(
		legend.background = element_rect(fill = "white", colour = NA),
		legend.box.spacing = unit(0, "lines"),
		legend.direction = "vertical",
		legend.justification = c(1, 1),
		legend.margin = margin(),
		legend.position = c(1, 1),
		legend.spacing = unit(0, "lines"),
		legend.title = element_blank()
	)

# combine panels into final plot
summary_plot <- plot_grid(
	accuracy_plot_a, 
	accuracy_plot_b, 
	accuracy_plot_c, 
	cols = 3,
	labels = c("accuracy", "certainty", "completeness"), 
	label_size = 12, label_fontface = "plain"
)

ggsave("fig_output/h2_accuracy.png", plot = summary_plot)





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
