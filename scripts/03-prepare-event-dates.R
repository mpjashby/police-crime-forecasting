# This code identifies whether each date is associated with particular types of
# event.



# SET UP VARIABLES -------------------------------------------------------------
dates <- expand_grid(
	date = seq.Date(ymd("2010-01-01"), ymd("2018-12-31"), by = "days"),
	city_name = c(
		"Austin", "Chicago", "Detroit", "Kansas City", "Los Angeles", "Louisville", 
		"Memphis", "New York", "San Francisco", "Seattle", "St Louis", "Tucson"
	)
) %>% 
	as_tibble()
events <- list()
teams <- list()



# DOWNLOAD DATA ----------------------------------------------------------------
# for each sport, this code should produce a tibble with three columns: the
# game `date`, the `city_name` and a logical showing TRUE for the sport (`nfl`,
# `mlb` etc.)


## NFL ----
teams$nfl <- tribble(
	~code, ~city_name,
	"CHI", "Chicago",
	"DET", "Detroit",
	"KC", "Kansas City",
	"LA", "Los Angeles",
	"SEA", "Seattle"
)
events$nfl <- fast_scraper_schedules(2010:2019) %>% 
	left_join(teams$nfl, by = c("home_team" = "code")) %>% 
	filter(!is.na(city_name)) %>% 
	mutate(date = ymd(gameday), nfl = TRUE) %>% 
	select(date, city_name, nfl)



## MLB ----
teams$mlb <- tribble(
	~code, ~city_name,
	"CHC", "Chicago", 
	"CHW", "Chicago", 
	"DET", "Detroit", 
	"KCR", "Kansas City", 
	"LAD", "Los Angeles", 
	"NYM", "New York", 
	"NYY", "New York", 
	"SFG", "San Francisco",
	"SEA", "Seattle",
	"STL", "St Louis"
)
events$mlb <- expand_grid(Tm = teams$mlb$code, year = 2010:2019) %>% 
	pmap_dfr(team_results_bref) %>% 
	janitor::clean_names() %>% 
	left_join(teams$mlb, by = c("tm" = "code")) %>% 
	filter(!is.na(city_name), h_a == "H") %>% 
	mutate(
		# Remove numbers in brackets after dates on days with multiple games
		date = str_remove(date, "\\s\\(.+?\\)"),
		# Parse game dates
		date = as_date(parse_date_time(str_glue("{date} {year}"), "a b d Y")),
		mlb = TRUE
	) %>% 
	select(date, city_name, mlb)


## NBA ----
# There was a lockout in 2011, so there are fewer games in that year
teams$nba <- tribble(
	~team, ~city_name,
	"Chicago Bulls", "Chicago",
	"Detroit Pistons", "Detroit",
	"LA Clippers", "Los Angeles",
	"Los Angeles Clippers", "Los Angeles",
	"Los Angeles Lakers", "Los Angeles",
	"Memphis Grizzlies", "Memphis",
	"Brooklyn Nets", "New York",
	"New York Knicks", "New York",
	"Golden State Warriors", "San Francisco"
)
events$nba <- map_dfr(
	2010:2020, 
	game_logs, 
	result_types = "team", 
	season_types = c("Regular Season", "Playoffs", "Pre Season", "All Star")
) %>% 
	janitor::clean_names() %>% 
	filter(
		location_game == "H", 
		between(date_game, ymd("2010-01-01"), ymd("2019-12-31"))
	) %>% 
	left_join(teams$nba, by = c("name_team" = "team")) %>% 
	filter(!is.na(city_name)) %>% 
	mutate(nba = TRUE) %>% 
	select(date = date_game, city_name, nba)


## NHL ----
# There was a lockout at the start of the 2012-13 season, so 2012 has fewer 
# games than other seasons. The number of games each season is also variable
# between teams because of home playoff games, where venue depends on team 
# ranking.
teams$nhl <- tribble(
	~team, ~city_name,
	"Chicago Blackhawks", "Chicago",
	"Detroit Red Wings", "Detroit",
	"Los Angeles Kings", "Los Angeles",
	"New York Rangers", "New York",
	"Seattle Kraken", "Seattle",
	"St. Louis Blues", "St Louis"
)
# The NHL API returns data in a nested list containing data frames that 
# themselves contain list columns, so we need to unpack these
events$nhl <- nhl_schedule_date_range("2010-01-01", "2019-12-31") %>% 
	map(pluck, "dates") %>% 
	map_dfr(select, date, games) %>% 
	unnest(cols = games) %>% 
	janitor::clean_names() %>% 
	select(date, teams_home_team_name) %>% 
	left_join(teams$nhl, by = c("teams_home_team_name" = "team")) %>% 
	filter(!is.na(city_name)) %>% 
	mutate(date = ymd(date), nhl = TRUE) %>% 
	select(date, city_name, nhl)


## MLS ----
# Note that NY Red Bulls play in New Jersey and Sporting Kansas City play in
# Kansas City, *Kansas*
teams$mls <- tribble(
	~team, ~city_name,
	"Austin FC", "Austin", # Joined league in 2021 so not currently in data
	"Chicago Fire FC", "Chicago",
	"Los Angeles FC", "Los Angeles",
	"New York City FC", "New York",
	"Seattle Sounders FC", "Seattle"
)
events$mls <- fixtures(2010, 2019) %>% 
	as_tibble() %>% 
	janitor::clean_names() %>% 
	# Remove games played at neutral venues or venues of other teams outside the
	# cities in the data, which is particularly important for MLS because ground
	# sharing means they have to sometimes move games due to conflicts
	filter(!venue %in% c(
		"ESPN Wide World of Sports Complex", # Disney in Florida
		"Red Bull Arena", # NY Red Bulls home ground in Harrison, NJ
		"Toyota Stadium", # FC Dallas home ground
		"Dignity Health Sports Park", # LA Galaxy home ground in Carson, CA
		"Pratt & Whitney Stadium at Rentschler Field" # reserve ground in East Hartford, CT
	)) %>% 
	left_join(teams$mls, by = c("home" = "team")) %>% 
	filter(!is.na(city_name)) %>% 
	mutate(mls = TRUE) %>% 
	select(date, city_name, mls)


## Motor sport ----
# Formula 1 in Austin, IndyCar in Austin, Detroit and Fort Worth and NASCAR Cup 
# Series in Fort Worth
# IndyCar: https://www.indycar.com/Stats
# NASCAR: https://results.motorsportstats.com/series/nascar-cup-series
teams$auto <- tribble(
	~track, ~city_name,
	"Austin", "Austin",
	"Belle Isle", "Detroit"
)
events$auto <- expand_grid(series = c("F", "O", "W"), year = 2010:2019) %>% 
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


## NCAA Division I Football Bowl Subdivision (FBS) ----
# These are included because typical FBS attendance is much larger than for many
# teams in the big-four leagues. Lots of FBS games are played at neutral venues
# so we need to account for both teams playing at home venues in the city and
# games in the city played by other teams. Note some cities are replicated in 
# the `teams$ncaa` object because the cities are referenced by more than one 
# name in the data.
teams$ncaa <- tribble(
	~location, ~school, ~city_name,
	"Austin, Texas", "Texas", "Austin",
	"Chicago Illinois", NA, "Chicago",
	"Chicago, IL", NA, "Chicago",
	"Chicago, Illinois", NA, "Chicago",
	"Detroit, MI", NA, "Detroit",
	"Detroit, Michigan", NA, "Detroit",
	"Ford Field - Detroit, MI", NA, "Detroit",
	"Ford Field, Detroit, MI", NA, "Detroit",
	"Los Angeles, California", "Southern California", "Los Angeles",
	"Louisville, Kentucky", "Louisville", "Louisville",
	"Memphis, Tennessee", "Memphis", "Memphis",
	"Bronx, New York", NA, "New York",
	"New York, New York", NA, "New York",
	"New York, NY", NA, "New York",
	"Bronx, NY", NA, "New York",
	"Seattle, WA", "Washington", "Seattle",
	"Seattle, Washington", NA, "Seattle",
	"Tucson, Arizona", "Arizona", "Tucson"
)
events$ncaa <- glue::glue("https://www.sports-reference.com/cfb/years/{2010:2019}-schedule.html") %>% 
	map(read_html) %>% 
	map(html_node, "table#schedule") %>% 
	map(html_table, fill = TRUE) %>% 
	map_dfr(janitor::clean_names) %>% 
	filter(rk != "Rk") %>% 
	mutate(
		date = mdy(date),
		# Find which team was a home (if x == "@" the away team won)
		home_team = ifelse(x == "@", loser, winner),
		# Remove number in parentheses from team name
		home_team = str_remove(home_team, "^\\(\\d+?\\)\\s+?"),
		# If the game was not at the home Team's usual stadium, the venue will be in
		# the notes column, sometimes with a dash separating the stadium and city
		# and sometimes with the city in parentheses
		other_venue_f1 = str_remove(str_extract(notes, " - .+?$"), "^ - "),
		other_venue_f2 = str_remove_all(str_extract(notes, "\\(.+?\\)$"), "[\\(\\)]"),
		other_venue = str_squish(ifelse(
			other_venue_f1 == "" | is.na(other_venue_f1), 
			other_venue_f2, 
			str_remove(other_venue_f1, "\\)$")
		)),
		# Set flag
		fbs = TRUE
	) %>% 
	left_join(select(teams$ncaa, school, city_name), by = c("home_team" = "school")) %>% 
	left_join(select(teams$ncaa, location, city_name), by = c("other_venue" = "location")) %>% 
	mutate(city_name = ifelse(is.na(city_name.x), city_name.y, city_name.x)) %>% 
	select(date, city_name, fbs)



# MERGE DATA -------------------------------------------------------------------
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
	mutate(across(where(is.logical), replace_na))



# SAVE DATA --------------------------------------------------------------------
write_csv(event_dates, "data_output/event_data.csv.gz")
