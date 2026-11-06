# scripts/daily.R — run every day by GitHub Actions (or by hand: Rscript scripts/daily.R)
# 1. downloads the games finished since last run
# 2. predicts tonight's games (+ injury notes) and adds them to nhl_predictions_log.csv
# 3. fills Won/Lost (and profit if you typed odds) in the log
# 4. exports everything the web page needs to site/data/*.json

if (!exists("NHL_SKIP_SOURCE")) source("nhl_periods.R")   # tests can pre-load and override
suppressPackageStartupMessages(library(jsonlite))

OUT <- "site/data"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
save_json <- function(x, name)
  write_json(x, file.path(OUT, name), dataframe = "rows", na = "null",
             auto_unbox = TRUE, digits = NA, pretty = FALSE)

update_data()
D  <- build_dataset()
PL <- build_players()
dt <- nhl_today()
message("NHL date: ", dt, " | finished games: ", length(unique(D$game_id)), " | last: ", max(D$date))

# ---- tonight ---------------------------------------------------------------
tonight <- tryCatch(predict_day(D, PL, dt), error = function(e) {
  warning("Tonight's predictions failed: ", conditionMessage(e)); tibble()
})
update_log_results(D)

# ---- season history: every finished game, predicted without look-ahead -----
season <- tryCatch({
  res <- results_table(D, PL, from = min(D$date), to = max(D$date))
  res <- add_form_columns(res, D, 10)
  res %>% mutate(across(starts_with("p_"), ~ round(100 * .x, 1)),
                 min_odds_pois = round(100 / p_pois_all3, 2),
                 min_odds_emp  = round(100 / p_emp_all3, 2),
                 period_goals = NA_character_)
}, error = function(e) { warning("Season history failed: ", conditionMessage(e)); tibble() })

# goals per period of each finished game (for the history table)
if (nrow(season)) {
  pg <- D %>% filter(venue == "home") %>%
    mutate(txt = paste0(gf, "-", ga)) %>%
    group_by(game_id) %>%
    summarise(period_goals = paste(paste0(period, " ", txt), collapse = " | "), .groups = "drop")
  season$period_goals <- pg$period_goals[match(season$game_id, pg$game_id)]
}

# ---- teams -----------------------------------------------------------------
teams <- team_period_summary(D) %>%
  left_join(last_n_counts(D, 10) %>% rename(last10_games = games, last10_P1 = P1,
                                           last10_P2 = P2, last10_P3 = P3, last10_all3 = all3),
            by = "team")
kp <- tryCatch(key_players(PL), error = function(e) NULL)

# ---- log (with your odds) ----------------------------------------------------
log <- if (file.exists(LOG_FILE)) read_log() else data.frame()

save_json(list(
  generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  generated_at_paris = format(Sys.time(), "%d/%m/%Y %H:%M", tz = "Europe/Paris"),
  nhl_date = dt, version = NHL_PERIODS_VERSION, seasons = SEASONS,
  finished_games = length(unique(D$game_id)), last_game_date = max(D$date),
  key_share = KEY_SHARE, goalie_share = GOALIE_SHARE,
  league = list(pct_goal_all3 = league_reference(D)$pct_goal_all3,
                pct_games_to_ot = league_reference(D)$pct_games_to_ot)
), "meta.json")
save_json(tonight, "tonight.json")
save_json(season,  "season.json")
save_json(teams,   "teams.json")
save_json(if (is.null(kp)) list() else kp, "key_players.json")
save_json(log,     "log.json")
message("Exported: tonight ", nrow(tonight), " | season ", nrow(season), " | log ", nrow(log))
