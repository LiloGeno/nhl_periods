# plumber_api.R — local HTTP API on top of nhl_periods.R
# Start:  plumber::pr_run(plumber::pr("plumber_api.R"), port = 8000)
# Then open http://127.0.0.1:8000/__docs__/ (interactive docs) or e.g.
#   http://127.0.0.1:8000/today                        games of the day + notes
#   http://127.0.0.1:8000/preview?date=2026-11-04       only games passing your bet rule
#   http://127.0.0.1:8000/results?from=2026-10-27&to=2026-11-02
#   http://127.0.0.1:8000/results/plot?from=2026-10-27&to=2026-11-02   (image)
#   http://127.0.0.1:8000/key_players?team=BOS
#   http://127.0.0.1:8000/matchup?home=BOS&away=TOR&last_n=10
#   http://127.0.0.1:8000/refresh        (downloads newly finished games)

source("nhl_periods.R")
D <- NULL; PL <- NULL
load_all <- function() {
  D <<- build_dataset(); PL <<- build_players()
  tryCatch(update_log_results(D), error = function(e)      # never block the API because of the log
    warning("Prediction log not updated: ", conditionMessage(e), call. = FALSE))
}
# At startup: download the games finished since last time (e.g. last night),
# then load everything and fill Won/Lost in the prediction log.
tryCatch(update_data(), error = function(e) warning("Update failed, using cached games: ", conditionMessage(e)))
load_all()

# Predict tonight's games right away so they are in the log without calling /today
predict_tonight <- function() tryCatch({
  out <- predict_day(D, PL, nhl_today())
  message("Predictions logged for ", nhl_today(), ": ", nrow(out), " games")
  nrow(out)
}, error = function(e) { warning("Could not predict today's games: ", conditionMessage(e), call. = FALSE); 0L })
predict_tonight()
message("API ready: ", length(unique(D$game_id)), " finished games (last: ", max(D$date),
        "), today's NHL date: ", nhl_today())

#* Games of the day: P(goal) per period and in all 3 periods (%), min odds, absence notes
#* @param date YYYY-MM-DD (North-American date; default = today)
#* @param refresh true to download the latest finished games first
#* @param last_n only each team's last N games at that venue (optional)
#* @get /today
function(date = "", refresh = "true", last_n = "") {
  if (tolower(refresh) == "true") { update_data(); load_all() }
  predict_day(D, PL, if (nzchar(date)) date else nhl_today(),
              last_n = if (nzchar(last_n)) as.integer(last_n))
}

#* Bet preview: only games where BOTH teams had a goal in every period in >= min_hits of their last n_last games AND model probability > p_min
#* @param date YYYY-MM-DD (North-American date; default = today). A past date also shows Won/Lost.
#* @param n_last window of last games per team (default 10)
#* @param min_hits minimum games with a goal in all 3 periods in that window (default 9)
#* @param p_min minimum probability, 0-1 (default 0.70)
#* @param model either (Poisson OR empirical), both, poisson or empirical
#* @param refresh true to download the latest finished games first
#* @get /preview
function(date = "", n_last = "10", min_hits = "9", p_min = "0.70", model = "either",
         refresh = "true") {
  if (tolower(refresh) == "true") { update_data(); load_all() }
  bet_preview(D, PL, if (nzchar(date)) date else nhl_today(),
              n_last = as.integer(n_last), min_hits = as.integer(min_hits),
              p_min = as.numeric(p_min), model = model)
}

#* Played games: pre-game probabilities, bet won/lost, absences, summary
#* @param from YYYY-MM-DD (default: 7 days ago)
#* @param to YYYY-MM-DD (default: yesterday)
#* @get /results
function(from = "", to = "") {
  from <- if (nzchar(from)) from else as.character(as.Date(nhl_today()) - 7)
  to   <- if (nzchar(to))   to   else as.character(as.Date(nhl_today()) - 1)
  res <- results_table(D, PL, from, to)
  list(summary = results_summary(res), games = res)
}

#* Same as /results, as an image
#* @param from YYYY-MM-DD
#* @param to YYYY-MM-DD
#* @param threshold optional vertical line, e.g. 0.7
#* @serializer png list(width = 1650, height = 1500)
#* @get /results/plot
function(from = "", to = "", threshold = "") {
  from <- if (nzchar(from)) from else as.character(as.Date(nhl_today()) - 7)
  to   <- if (nzchar(to))   to   else as.character(as.Date(nhl_today()) - 1)
  f <- tempfile(fileext = ".png")
  p <- plot_results(results_table(D, PL, from, to), file = f,
                    threshold = if (nzchar(threshold)) as.numeric(threshold))
  print(p)
}

#* Key forwards and main goalies
#* @param team Team code (optional, e.g. BOS)
#* @get /key_players
function(team = "") {
  k <- key_players(PL)
  if (nzchar(team)) k <- k[k$team == toupper(team), ]
  k
}

#* Per-team period table
#* @param venue all, home or away
#* @param last_n only the team's last N games (optional)
#* @get /teams
function(venue = "all", last_n = "") {
  team_period_summary(D, venue, if (nzchar(last_n)) as.integer(last_n))
}

#* One team, split all / home / away
#* @param abbrev Team code (e.g. BOS)
#* @get /team/<abbrev>
function(abbrev) {
  lapply(c(all = "all", home = "home", away = "away"), function(v)
    filter(team_period_summary(D, v), team == toupper(abbrev)))
}

#* Estimated P(goal) per period for a matchup
#* @param home Home team code
#* @param away Away team code
#* @param last_n only each team's last N games (optional)
#* @get /matchup
function(home, away, last_n = "") {
  out <- matchup(toupper(home), toupper(away), D, if (nzchar(last_n)) as.integer(last_n))
  list(periods = out,
       p_goal_all3_poisson = round(100 * prod(out$p_goal_poisson / 100), 1),
       p_goal_all3_empiric = round(100 * prod(out$p_goal_empiric / 100), 1))
}

#* Version of nhl_periods.R loaded by this API (restart the API after replacing the files)
#* @get /version
function() list(version = NHL_PERIODS_VERSION, games = length(unique(D$game_id)),
                last_game_date = max(D$date), today = nhl_today(),
                computer_time = format(Sys.time(), "%Y-%m-%d %H:%M %Z"))

#* League averages
#* @get /league
function() league_reference(D)

#* Download newly finished games, rebuild, fill Won/Lost and log tonight's predictions
#* @get /refresh
function() {
  update_data(); load_all(); n <- predict_tonight()
  list(finished_games = length(unique(D$game_id)), last_game_date = max(D$date),
       today = nhl_today(), games_tonight_logged = n)
}
