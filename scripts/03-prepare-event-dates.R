# This code identifies whether each date is associated with particular types of
# event.



# SET UP VARIABLES
dates <- expand.grid(
	date = seq.Date(ymd("2010-01-01"), ymd("2018-12-31"), by = "days"),
	city_name = c("Austin", "Chicago", "Detroit", "Fort Worth", "Kansas City", 
								"Los Angeles", "Louisville", "New York", "San Francisco", 
								"Tucson"),
	stringsAsFactors = FALSE
) %>% 
	as_tibble()
events <- list()
teams <- list()



# DOWNLOAD DATA
# for each sport, this code should produce a tibble with three columns: the
# game `date`, the `city_name` and a logical showing TRUE for the sport (`nfl`,
# `mlb` etc.)

# NFL
teams$nfl <- tribble(
	~code, ~city_name,
	"CHI", "Chicago",
	"DET", "Detroit",
	"KC", "Kansas City",
	"LA", "Los Angeles"
)
events$nfl <- map(2010:2018, season_games, sleep.seconds = 5) %>% 
	bind_rows() %>% 
	as_tibble() %>% 
	left_join(teams$nfl, by = c("home" = "code")) %>% 
	filter(!is.na(city_name)) %>% 
	mutate(nfl = TRUE) %>% 
	select(date, city_name, nfl)

# MLB
teams$mlb <- tribble(
	~code, ~city_name,
	"CHC", "Chicago", 
	"CHW", "Chicago", 
	"DET", "Detroit", 
	"KCR", "Kansas City", 
	"LAD", "Los Angeles", 
	"NYM", "New York", 
	"NYY", "New York", 
	"SFG", "San Francisco"
)
events$mlb <- pmap(
	expand.grid(teams$mlb$code, 2010:2018), 
	function (...) team_results_bref(..1, ..2)
) %>% 
	bind_rows() %>% 
	as_tibble() %>% 
	left_join(teams$mlb, by = c("Tm" = "code")) %>% 
	filter(!is.na(city_name), H_A == "H") %>% 
	mutate(
		date = as_date(parse_date_time(
			paste(str_remove(Date, " \\(.+?\\)$"), Year), 
			orders = "AbdY")),
		mlb = TRUE
	) %>% 
	select(date, city_name, mlb)

# NBA
# There was a lockout in 2011, so there are fewer games in that year
teams$nba <- tribble(
	~team, ~city_name,
	"Chicago Bulls", "Chicago",
	"Detroit Pistons", "Detroit",
	"LA Clippers", "Los Angeles",
	"Los Angeles Clippers", "Los Angeles",
	"Los Angeles Lakers", "Los Angeles",
	"Brooklyn Nets", "New York",
	"New York Knicks", "New York",
	"Golden State Warriors", "San Francisco"
)
events$nba <- map(
	2010:2019, 
	game_logs, 
	result_types = "team", 
	season_types = c("Regular Season", "Playoffs", "Pre Season", "All Star")
) %>% 
	bind_rows() %>% 
	filter(
		locationGame == "H", 
		between(dateGame, ymd("2010-01-01"), ymd("2018-12-31"))
	) %>% 
	left_join(teams$nba, by = c("nameTeam" = "team")) %>% 
	filter(!is.na(city_name)) %>% 
	mutate(nba = TRUE) %>% 
	select(date = dateGame, city_name, nba)


# NHL
# PROBLEM: not enough NHL games for LA for 2012 or 2015, or NYC for 2012 or 2018
teams$nhl <- tribble(
	~team, ~city_name,
	"Chicago Blackhawks", "Chicago",
	"Detroit Red Wings", "Detroit",
	"Los Angeles Kings", "Los Angeles",
	"New York Rangers", "New York"
)
events$nhl <- glue::glue("https://www.hockey-reference.com/leagues/NHL_{2010:2019}_games.html") %>% 
	map(read_html) %>% 
	map(function (x) {
	# each web page contains two tables, one for the regular season and one for
	# the playoffs, so we rbind them first
	bind_rows(
		janitor::clean_names(html_table(html_node(x, "table#games"))),
		janitor::clean_names(html_table(html_node(x, "table#games_playoffs")))
	)
}) %>% 
	bind_rows() %>% 
	as_tibble() %>% 
	left_join(teams$nhl, by = c("home" = "team")) %>% 
	mutate(date = ymd(date), nhl = TRUE) %>% 
	filter(
		!is.na("g"), # exclude postponed games, which have no score in the data
		!is.na(city_name),
		between(date, ymd("2010-01-01"), ymd("2018-12-31"))
	) %>% 
	select(date, city_name, nhl)



# MLS
teams$mls <- tribble(
	~team, ~city_name,
	"Los Angeles FC", "Los Angeles",
	"NYCFC", "New York"
)
events$mls <- c(
	"https://fbref.com/en/comps/22/442/schedule/2010-Major-League-Soccer-Stats",
	"https://fbref.com/en/comps/22/509/schedule/2011-Major-League-Soccer-Stats",
	"https://fbref.com/en/comps/22/577/schedule/2012-Major-League-Soccer-Stats",
	"https://fbref.com/en/comps/22/643/schedule/2013-Major-League-Soccer-Stats",
	"https://fbref.com/en/comps/22/708/schedule/2014-Major-League-Soccer-Stats",
	"https://fbref.com/en/comps/22/1369/schedule/2015-Major-League-Soccer-Stats",
	"https://fbref.com/en/comps/22/1503/schedule/2016-Major-League-Soccer-Stats",
	"https://fbref.com/en/comps/22/1558/schedule/2017-Major-League-Soccer-Stats",
	"https://fbref.com/en/comps/22/1759/schedule/2018-Major-League-Soccer-Stats"
) %>% 
	map(read_html)

events$mls <- events$mls %>% 
	map(html_node, "table#sched_ks_all") %>% 
	map(html_table) %>% 
	map(janitor::clean_names) %>% 
	bind_rows() %>% 
	as_tibble() %>% 
	filter(round != "Round", !str_detect(round, "MLS Cup")) %>% 
	left_join(teams$mls, by = c("home" = "team")) %>% 
	mutate(date = ymd(date), mls = TRUE) %>% 
	filter(!is.na(city_name)) %>% 
	select(date, city_name, mls)

# Motor sport
# Formula 1 in Austin, IndyCar in Detroit and Fort Worth (not Austin because
# IndyCar there only started in 2019) and NASCAR Cup Series in Fort Worth
# IndyCar: https://www.indycar.com/Stats
# NASCAR: https://results.motorsportstats.com/series/nascar-cup-series
teams$auto <- tribble(
	~track, ~city_name,
	"Austin", "Austin",
	"Belle Isle", "Detroit",
	"Fort Worth", "Fort Worth"
)
events$auto <- expand.grid(series = c("F", "O", "W"), year = 2010:2018) %>% 
	pmap(function (...) {
		glue::glue("https://www.racing-reference.info/season-stats/{..2}/{..1}/")
	}) %>% 
	map(read_html) %>% 
	map(html_nodes, ".table-row") %>% 
	map_dfr(function (x) {
		tibble(
			date = html_text(html_node(x, ".date")), 
			track = html_text(html_node(x, ".track a"))
		)
	}) %>% 
	left_join(teams$auto, by = "track") %>% 
	mutate(date = mdy(date), auto = TRUE) %>% 
	filter(!is.na(city_name)) %>% 
	select(date, city_name, auto)


# NCAA Division I Football Bowl Subdivision (FBS)
teams$ncaa <- tribble(
	~location, ~school, ~city_name,
	"Austin Texas", "Texas", "Austin",
	"Chicago Illinois", NA, "Chicago",
	"Detroit Michigan", NA, "Detroit",
	"Fort Worth Texas", "Texas Christian", "Fort Worth",
	"Kansas City Missouri", NA, "Kansas City",
	"Los Angeles California", "Southern California", "Los Angeles",
	"Louisville Kentucky", "Louisville", "Louisville",
	"Bronx NY", NA, "New York",
	"Pinstripe Bowl Bronx NY", NA, "New York",
	"New York New York", NA, "New York",
	"San Francisco California", NA, "San Francisco",
	"San Francisco CA", NA, "San Francisco",
	"Fight Hunger Bowl San Francisco CA", NA, "San Francisco",
	"Tucson Arizona", "Arizona", "Tucson"
)
events$ncaa <- glue::glue("https://www.sports-reference.com/cfb/years/{2010:2018}-schedule.html") %>% 
	map(read_html) %>% 
	map(html_node, "table#schedule") %>% 
	map(html_table, fill = TRUE) %>% 
	map(janitor::clean_names) %>% 
	bind_rows() %>% 
	as_tibble() %>% 
	filter(rk != "Rk") %>% 
	mutate(
		date = mdy(date),
		x = ifelse(is.na(x), x_2, x),
		home = str_remove(trimws(ifelse(x == "@", loser, winner)), 
											"^\\(\\d+?\\)\\s+?"),
		home_team = home %in% as.character(na.omit(teams$ncaa$school)),
		location = str_remove_all(str_remove(trimws(notes), "^.*? - "), 
															"[^\\w\\s+?]"),
		at_venue = str_detect(
			location, 
			paste0("(", paste0(teams$ncaa$location, collapse = "|"), ")")
		),
		fbs = case_when(
			home_team == TRUE & trimws(location) == "" ~ TRUE,
			at_venue == TRUE ~ TRUE,
			TRUE ~ FALSE
		)
	) %>% 
	filter(fbs) %>%
	left_join(
		select(teams$ncaa, location, city_name),
		by = c("location" = "location")
	) %>%
	left_join(
		select(teams$ncaa, school, city_name),
		by = c("home" = "school")
	) %>%
	mutate(city_name = ifelse(!is.na(city_name.x), city_name.x, city_name.y)) %>%
	select(date, city_name, fbs)


# MERGE DATA
event_dates <- events %>% 
	reduce(full_join, by = c("date", "city_name")) %>% 
	# to remove duplicate rows (produced when more than one team in a city is 
	# playing a sport on a given date, or when a team plays a double header)
	# we create an index column for games in a city on a date, then filter any
	# rows that aren't the first for that day
	arrange(date, city_name) %>% 
	group_by(date, city_name) %>% 
	mutate(row = row_number()) %>% 
	ungroup() %>% 
	filter(row == 1) %>%
	select(-row) %>% 
	# add in dates with no games
	right_join(dates, by = c("date", "city_name")) %>%
	mutate_if(is.logical, replace_na, replace = FALSE)


# SAVE DATA
write_csv(event_dates, "data_output/event_data.csv.gz")
