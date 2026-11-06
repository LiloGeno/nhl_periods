# =============================================================================
# nhl_periods.R — NHL goals by period (scored / conceded / OT) for betting
# Source: free public NHL API (api-web.nhle.com), no key required.
#
# Usage (in R / RStudio):
#   source("nhl_periods.R")
#   update_data()                 # downloads finished games (cached, incremental)
#   d   <- build_dataset()        # one row per team x game x period
#   tab <- team_period_summary(d) # per-team table
#   print_rankings(tab)
#   matchup("BOS", "TOR", d)      # home, away -> estimated P(goal) per period
#   plot_period_heatmap(tab)      # heatmap saved as PNG
#   l10 <- last_n_counts(d, 10)   # last 10 games: count with >= 1 goal per period
#   plot_last_n_heatmap(l10, 10)  # heatmap "x/10" saved as PNG
#   bt  <- run_backtest(d)        # replay the season: calibration, thresholds
#
#   today()                       # games of the day: probabilities + absence notes
#   results()                     # last 7 days: predictions vs bet won/lost + plot
#   bet_preview(D, PL, "2026-11-04")  # only games passing the rule (9/10 form + p > 70%)
#   P <- build_players(); key_players(P)   # key forwards & main goalies
#
# Packages: jsonlite, dplyr, tidyr, ggplot2
# =============================================================================

NHL_PERIODS_VERSION <- "2026-11-06a"
message("nhl_periods.R version ", NHL_PERIODS_VERSION)

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# ---------------------------------------------------------------- settings ---
# 20262027 = season in progress (Oct 2026 -> June 2027)
# 20252026 = previous season (Oct 2025 -> June 2026). Add it for a bigger sample.
SEASONS    <- strsplit(Sys.getenv("NHL_SEASONS", "20262027"), "[ ,;]+")[[1]]
GAME_TYPES <- c(2)            # 2 = regular season, 3 = playoffs
CACHE_DIR  <- "nhl_cache"     # finished games are stored here and never re-downloaded
API        <- "https://api-web.nhle.com/v1"

# 32 clubs (fallback if the standings endpoint is unavailable)
TEAMS_FALLBACK <- c("ANA","BOS","BUF","CAR","CBJ","CGY","CHI","COL","DAL","DET",
                    "EDM","FLA","LAK","MIN","MTL","NJD","NSH","NYI","NYR","OTT",
                    "PHI","PIT","SEA","SJS","STL","TBL","TOR","UTA","VAN","VGK",
                    "WPG","WSH")

# --------------------------------------------------------------- download ---
get_json <- function(url, tries = 3) {
  for (i in seq_len(tries)) {
    res <- tryCatch(fromJSON(url, simplifyVector = TRUE), error = function(e) e)
    if (!inherits(res, "error")) return(res)
    Sys.sleep(1.5 * i)
  }
  warning("Failed: ", url, " (", conditionMessage(res), ")")
  NULL
}

get_teams <- function() {
  st <- get_json(file.path(API, "standings/now"))
  ab <- tryCatch(st$standings$teamAbbrev$default, error = function(e) NULL)
  if (length(ab) >= 30) sort(unique(ab)) else TEAMS_FALLBACK
}

# IDs of finished games for one season
get_game_ids <- function(season, game_types = GAME_TYPES) {
  ids <- c()
  for (tm in get_teams()) {
    sc <- get_json(sprintf("%s/club-schedule-season/%s/%s", API, tm, season))
    g  <- sc$games
    if (is.null(g) || !nrow(g)) next
    ok <- g$gameType %in% game_types & g$gameState %in% c("OFF", "FINAL")
    ids <- c(ids, g$id[ok])
    Sys.sleep(0.2)
  }
  sort(unique(ids))
}

# Download play-by-play for every finished game not yet in the cache
update_data <- function(seasons = SEASONS, game_types = GAME_TYPES) {
  dir.create(CACHE_DIR, showWarnings = FALSE)
  for (s in seasons) {
    ids  <- get_game_ids(s, game_types)
    todo <- ids[!file.exists(file.path(CACHE_DIR, paste0(ids, ".json")))]
    message(s, ": ", length(ids), " finished games, ", length(todo), " to download")
    for (k in seq_along(todo)) {
      url <- sprintf("%s/gamecenter/%s/play-by-play", API, todo[k])
      txt <- tryCatch(paste(readLines(url, warn = FALSE, encoding = "UTF-8"),
                            collapse = ""), error = function(e) NULL)
      if (!is.null(txt)) writeLines(txt, file.path(CACHE_DIR, paste0(todo[k], ".json")))
      if (k %% 50 == 0) message("  ", k, "/", length(todo))
      Sys.sleep(0.2)
    }
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------ parsing ---
# One finished game -> rows team x period (P1, P2, P3, and OT if played).
# Shootout "goals" are ignored (they are not real goals and do not count in
# period bets).
parse_game <- function(file) {
  g <- fromJSON(file, simplifyVector = TRUE)
  home <- g$homeTeam$abbrev; away <- g$awayTeam$abbrev
  hid  <- g$homeTeam$id;     aid  <- g$awayTeam$id
  pl   <- g$plays

  goals <- data.frame(period = character(), team_id = integer())
  if (!is.null(pl) && nrow(pl)) {
    ptype <- pl$periodDescriptor$periodType
    pnum  <- pl$periodDescriptor$number
    isg   <- pl$typeDescKey == "goal" & ptype %in% c("REG", "OT")
    if (any(isg)) {
      goals <- data.frame(
        period  = ifelse(ptype[isg] == "OT", "OT", paste0("P", pnum[isg])),
        team_id = pl$details$eventOwnerTeamId[isg])
    }
  }

  last <- g$gameOutcome$lastPeriodType
  if (is.null(last)) last <- if (!is.null(pl) && max(pl$periodDescriptor$number) > 3) "OT" else "REG"
  periods <- c("P1", "P2", "P3", if (last %in% c("OT", "SO")) "OT")

  rows <- expand.grid(period = periods, venue = c("home", "away"),
                      stringsAsFactors = FALSE)
  rows$team     <- ifelse(rows$venue == "home", home, away)
  rows$opponent <- ifelse(rows$venue == "home", away, home)
  my_id  <- ifelse(rows$venue == "home", hid, aid)
  opp_id <- ifelse(rows$venue == "home", aid, hid)
  rows$gf <- mapply(function(p, t) sum(goals$period == p & goals$team_id == t), rows$period, my_id)
  rows$ga <- mapply(function(p, t) sum(goals$period == p & goals$team_id == t), rows$period, opp_id)

  id <- as.character(g$id)
  cbind(game_id = id,
        date    = as.character(g$gameDate),
        season  = paste0(substr(id, 1, 4), as.integer(substr(id, 1, 4)) + 1),
        went_ot = last %in% c("OT", "SO"),
        shootout = last == "SO",
        rows, stringsAsFactors = FALSE)
}

# Every cached game of the chosen seasons -> one long table
cached_files <- function(seasons = SEASONS, game_types = GAME_TYPES) {
  files <- list.files(CACHE_DIR, pattern = "^\\d{10}\\.json$", full.names = TRUE)
  ids   <- sub("\\.json$", "", basename(files))
  keep  <- paste0(substr(ids, 1, 4), as.integer(substr(ids, 1, 4)) + 1) %in% seasons &
           as.integer(substr(ids, 5, 6)) %in% game_types
  if (!any(keep)) stop("No cached game for these seasons: run update_data() first.")
  files[keep]
}

build_dataset <- function(seasons = SEASONS, game_types = GAME_TYPES) {
  d <- bind_rows(lapply(cached_files(seasons, game_types), parse_game))
  d$period <- factor(d$period, levels = c("P1", "P2", "P3", "OT"))
  d$total  <- d$gf + d$ga
  as_tibble(d)
}

# ------------------------------------------------------------------ summary ---
# Per team: for each regulation period, average goals for/against and the %
# of games in which the team scored / conceded / the game had >= 1 goal.
# venue = "all", "home" or "away"; last_n = only the team's last N games.
team_period_summary <- function(d, venue = c("all", "home", "away"), last_n = NULL) {
  venue <- match.arg(venue)
  x <- if (venue == "all") d else d[d$venue == venue, ]
  if (!is.null(last_n)) {
    recent <- x %>% distinct(team, game_id, date) %>% group_by(team) %>%
      slice_max(date, n = last_n, with_ties = FALSE) %>% ungroup()
    x <- semi_join(x, recent, by = c("team", "game_id"))
  }
  reg <- x %>% filter(period != "OT")

  per <- reg %>% group_by(team, period) %>%
    summarise(gf_avg       = mean(gf),
              ga_avg       = mean(ga),
              pct_scored   = 100 * mean(gf > 0),
              pct_conceded = 100 * mean(ga > 0),
              pct_goal     = 100 * mean(total > 0),
              .groups = "drop") %>%
    pivot_wider(names_from = period,
                values_from = c(gf_avg, ga_avg, pct_scored, pct_conceded, pct_goal),
                names_glue = "{period}_{.value}")

  # The bet: >= 1 goal in EVERY regulation period (P1 AND P2 AND P3)
  allp <- reg %>% group_by(team, game_id) %>%
    summarise(all3 = all(total > 0), n0 = sum(total == 0), .groups = "drop") %>%
    group_by(team) %>%
    summarise(gp = n(), pct_goal_all3 = 100 * mean(all3), .groups = "drop")

  ot <- x %>% filter(period == "OT") %>% group_by(team) %>%
    summarise(ot_games = n(), ot_gf = sum(gf), ot_ga = sum(ga), .groups = "drop")

  allp %>% left_join(per, by = "team") %>% left_join(ot, by = "team") %>%
    mutate(across(c(ot_games, ot_gf, ot_ga), ~ coalesce(.x, 0L))) %>%
    mutate(across(where(is.double), ~ round(.x, 2))) %>%
    arrange(desc(pct_goal_all3))
}

league_reference <- function(d) {
  reg <- d %>% filter(period != "OT", venue == "home")   # 1 row per game x period
  list(
    pct_goal_by_period = reg %>% group_by(period) %>%
      summarise(pct_goal = round(100 * mean(total > 0), 1), .groups = "drop"),
    pct_goal_all3 = round(100 * mean(tapply(reg$total > 0, reg$game_id, all)), 1),
    pct_games_to_ot = round(100 * mean(tapply(d$went_ot, d$game_id, any)), 1))
}

print_rankings <- function(tab, n = 8) {
  show <- function(title, df) { cat("\n==", title, "==\n"); print(as.data.frame(df), row.names = FALSE) }
  show("Most often >= 1 goal in all 3 periods",
       head(tab[, c("team", "gp", "pct_goal_all3")], n))
  show("Least often >= 1 goal in all 3 periods",
       tail(tab[, c("team", "gp", "pct_goal_all3")], n))
  for (p in c("P1", "P2", "P3")) {
    cols <- paste0(p, c("_pct_scored", "_pct_conceded", "_pct_goal"))
    o <- tab[order(-tab[[cols[3]]]), c("team", cols)]
    show(paste(p, "- highest % of games with a goal"), head(o, 5))
    show(paste(p, "- lowest % of games with a goal"),  tail(o, 5))
  }
}

# ------------------------------------------------------------------ matchup ---
# Estimate for HOME vs AWAY, per regulation period:
#  - "empirical": average of the two teams' observed % of games with a goal
#    (home team's home games, away team's away games)
#  - "poisson": expected goals = (home GF + away GA)/2 + (away GF + home GA)/2,
#    P(>= 1 goal) = 1 - exp(-lambda)
# P(goal in all 3 periods) assumes the periods are independent (approximation).
matchup <- function(home, away, d, last_n = NULL) {
  h <- team_period_summary(d, "home", last_n) %>% filter(team == home)
  a <- team_period_summary(d, "away", last_n) %>% filter(team == away)
  if (!nrow(h) || !nrow(a)) stop("Unknown team or no games: ", home, " / ", away)

  out <- lapply(c("P1", "P2", "P3"), function(p) {
    g <- function(df, v) df[[paste0(p, "_", v)]]
    lam <- (g(h, "gf_avg") + g(a, "ga_avg")) / 2 + (g(a, "gf_avg") + g(h, "ga_avg")) / 2
    data.frame(period = p,
               exp_goals       = round(lam, 2),
               p_goal_poisson  = round(100 * (1 - exp(-lam)), 1),
               p_goal_empiric  = round((g(h, "pct_goal") + g(a, "pct_goal")) / 2, 1),
               home_pct_scored = g(h, "pct_scored"),
               away_pct_scored = g(a, "pct_scored"))
  })
  out <- bind_rows(out)
  cat(sprintf("\n%s (home, %d games) vs %s (away, %d games)\n", home, h$gp, away, a$gp))
  print(out, row.names = FALSE)
  cat(sprintf("P(goal in all 3 periods): Poisson %.1f%% | empirical %.1f%% (independence assumed)\n",
              100 * prod(out$p_goal_poisson / 100), 100 * prod(out$p_goal_empiric / 100)))
  cat(sprintf("Observed 'goal in all 3' rate: %s %.1f%% at home, %s %.1f%% away\n",
              home, h$pct_goal_all3, away, a$pct_goal_all3))
  invisible(out)
}

# --------------------------------------------------------------------- plot ---
plot_period_heatmap <- function(tab, file = "nhl_goal_by_period.png") {
  long <- tab %>%
    select(team, gp, P1_pct_goal, P2_pct_goal, P3_pct_goal, pct_goal_all3) %>%
    pivot_longer(-c(team, gp), names_to = "metric", values_to = "pct") %>%
    mutate(metric = factor(metric,
             levels = c("P1_pct_goal", "P2_pct_goal", "P3_pct_goal", "pct_goal_all3"),
             labels = c("Period 1", "Period 2", "Period 3", "All 3 periods")),
           team = factor(team, levels = rev(tab$team[order(-tab$pct_goal_all3)])))
  p <- ggplot(long, aes(metric, team, fill = pct)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = round(pct)), size = 3) +
    scale_fill_gradient(low = "#f8fafc", high = "#1d4ed8", name = "% of games") +
    labs(title = "Games with at least one goal, by period",
         subtitle = paste("Regular season, games played:", min(tab$gp), "-", max(tab$gp), "per team"),
         x = NULL, y = "Team") +
    theme_minimal(base_size = 11) + theme(panel.grid = element_blank())
  ggsave(file, p, width = 7, height = 10, dpi = 150, bg = "white")
  message("Saved: ", file)
  invisible(p)
}

# ------------------------------------------------- last N games (form) ---
# For each team's last N games: how many had >= 1 goal (either team) in
# P1, P2, P3, and in all 3 periods. venue = "all", "home" or "away".
last_n_counts <- function(d, n = 10, venue = c("all", "home", "away")) {
  venue <- match.arg(venue)
  x <- if (venue == "all") d else d[d$venue == venue, ]
  recent <- x %>% distinct(team, game_id, date) %>% group_by(team) %>%
    slice_max(date, n = n, with_ties = FALSE) %>% ungroup()
  x %>% semi_join(recent, by = c("team", "game_id")) %>%
    filter(period != "OT") %>%
    group_by(team, game_id) %>%
    summarise(P1 = any(period == "P1" & total > 0),
              P2 = any(period == "P2" & total > 0),
              P3 = any(period == "P3" & total > 0),
              .groups = "drop") %>%
    group_by(team) %>%
    summarise(games = n(),
              all3 = sum(P1 & P2 & P3),          # computed before P1..P3 become counts
              P1 = sum(P1), P2 = sum(P2), P3 = sum(P3),
              .groups = "drop") %>%
    relocate(all3, .after = P3) %>%
    arrange(desc(all3), desc(P1 + P2 + P3))
}

plot_last_n_heatmap <- function(cnt, n = 10, file = sprintf("nhl_last%d_by_period.png", n)) {
  long <- cnt %>%
    pivot_longer(c(P1, P2, P3, all3), names_to = "metric", values_to = "count") %>%
    mutate(metric = factor(metric, levels = c("P1", "P2", "P3", "all3"),
                           labels = c("Period 1", "Period 2", "Period 3", "All 3 periods")),
           team = factor(team, levels = rev(cnt$team)),
           label = paste0(count, "/", games))
  p <- ggplot(long, aes(metric, team, fill = count / games)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = label), size = 3) +
    scale_fill_gradient(low = "#f8fafc", high = "#1d4ed8",
                        labels = scales::percent, name = "Share of games") +
    labs(title = sprintf("Last %d games: games with at least one goal, by period", n),
         subtitle = "Goals by either team, overtime excluded",
         x = NULL, y = "Team") +
    theme_minimal(base_size = 11) + theme(panel.grid = element_blank())
  ggsave(file, p, width = 7, height = 10, dpi = 150, bg = "white")
  message("Saved: ", file)
  invisible(p)
}

# ----------------------------------------------------------------- backtest ---
# Replays the season: for each game, the model only sees games played on
# EARLIER dates (no look-ahead), predicts P(goal) per period and for all 3
# periods, then compares with what actually happened.
#   min_games : each team needs at least this many prior games at that venue
#   last_n    : use only each team's last N games at that venue (form window)
# Returns one row per game: predictions (p_*) and outcomes (y_*), 0-1 scale.
backtest <- function(d, min_games = 5, last_n = NULL) {
  games <- game_outcomes(d)

  # league baseline per game date (games before that date only)
  lg <- games %>% arrange(date) %>% group_by(date) %>%
    summarise(n = n(), k = sum(y_all3), .groups = "drop") %>%
    mutate(p_league_all3 = lag(cumsum(k) / cumsum(n)))

  dates <- sort(unique(games$date))
  out <- lapply(dates, function(dt)
    predict_games(games[games$date == dt, ], d[d$date < dt, ], min_games, last_n))
  bind_rows(out) %>%
    filter(!is.na(p_pois_P1)) %>%
    left_join(select(lg, date, p_league_all3), by = "date") %>%
    arrange(date)
}

# One row per finished game: did each regulation period have >= 1 goal?
game_outcomes <- function(d) {
  d %>% filter(venue == "home", period != "OT") %>%
    group_by(game_id, date, home = team, away = opponent) %>%
    summarise(y_P1 = any(period == "P1" & total > 0),
              y_P2 = any(period == "P2" & total > 0),
              y_P3 = any(period == "P3" & total > 0), .groups = "drop") %>%
    mutate(y_all3 = y_P1 & y_P2 & y_P3)
}

# Core model, shared by backtest(), predict_day() and results_table().
# g: games with columns home, away. prior: games played BEFORE these games.
# Each team uses its stats at that venue (home team: home games, away team:
# away games). If it has < min_games at that venue, it falls back to ALL its
# games (column basis = "all games"). Only teams with < min_games in total
# get NA probabilities (basis = "too few games").
predict_games <- function(g, prior, min_games = 5, last_n = NULL) {
  g$home_gp <- NA_integer_; g$away_gp <- NA_integer_
  g$basis <- "too few games"
  for (p in c("P1", "P2", "P3")) {
    g[[paste0("p_pois_", p)]] <- NA_real_; g[[paste0("p_emp_", p)]] <- NA_real_
  }
  if (nrow(prior) && nrow(g)) {
    hs <- team_period_summary(prior, "home", last_n)
    aw <- team_period_summary(prior, "away", last_n)
    al <- team_period_summary(prior, "all",  last_n)
    pick <- function(venue_tab, teams) {
      x  <- venue_tab[match(teams, venue_tab$team), ]
      y  <- al[match(teams, al$team), ]
      fb <- is.na(x$gp) | x$gp < min_games
      x[fb, ] <- y[fb, ]
      list(tab = x, fallback = fb)
    }
    H <- pick(hs, g$home); A <- pick(aw, g$away)
    h <- H$tab; a <- A$tab
    g$home_gp <- h$gp; g$away_gp <- a$gp
    ok <- !is.na(h$gp) & !is.na(a$gp) & h$gp >= min_games & a$gp >= min_games
    g$basis <- ifelse(!ok, "too few games",
                      ifelse(H$fallback | A$fallback, "all games (few at venue)", "home/away games"))
    for (p in c("P1", "P2", "P3")) {
      v <- function(df, x) df[[paste0(p, "_", x)]]
      lam <- (v(h, "gf_avg") + v(a, "ga_avg")) / 2 + (v(a, "gf_avg") + v(h, "ga_avg")) / 2
      g[[paste0("p_pois_", p)]] <- ifelse(ok, 1 - exp(-lam), NA)
      g[[paste0("p_emp_",  p)]] <- ifelse(ok, (v(h, "pct_goal") + v(a, "pct_goal")) / 200, NA)
    }
  }
  g %>% mutate(p_pois_all3 = p_pois_P1 * p_pois_P2 * p_pois_P3,
               p_emp_all3  = p_emp_P1  * p_emp_P2  * p_emp_P3)
}

# Accuracy scores (lower = better). The "league" row is the naive baseline:
# always predict the league-wide rate so far. A useful model must beat it.
backtest_scores <- function(bt, target = "all3") {
  y <- bt[[paste0("y_", target)]]
  models <- c(poisson = paste0("p_pois_", target), empirical = paste0("p_emp_", target))
  if (target == "all3") models <- c(models, league = "p_league_all3")
  bind_rows(lapply(names(models), function(m) {
    p <- pmin(pmax(bt[[models[m]]], 0.01), 0.99); k <- !is.na(p)
    data.frame(model = m, n = sum(k),
               mean_pred = round(mean(p[k]), 3), observed = round(mean(y[k]), 3),
               brier = round(mean((p[k] - y[k])^2), 4),
               logloss = round(-mean(y[k] * log(p[k]) + (1 - y[k]) * log(1 - p[k])), 4))
  }))
}

# Calibration: games grouped by predicted probability. Well calibrated =
# "observed" close to "mean_pred" in every bin.
calibration_table <- function(bt, model = c("pois", "emp"), target = "all3",
                              breaks = seq(0, 1, 0.05)) {
  model <- match.arg(model)
  p <- bt[[paste0("p_", model, "_", target)]]; y <- bt[[paste0("y_", target)]]
  data.frame(p, y) %>% filter(!is.na(p)) %>%
    mutate(bin = cut(p, breaks, include.lowest = TRUE)) %>%
    group_by(bin) %>%
    summarise(n = n(), mean_pred = round(mean(p), 3), observed = round(mean(y), 3),
              .groups = "drop")
}

# "If I had bet every game where the model said >= threshold":
# how many bets, hit rate, and the minimum odds needed to break even.
# If bt has a column odds_<target> (your bookmaker odds, joined by game_id),
# flat-stake profit and ROI are added, and "edge" bets (model prob >
# 1/odds + margin) are evaluated too.
threshold_table <- function(bt, model = c("pois", "emp"), target = "all3",
                            thresholds = seq(0.50, 0.95, 0.05), margin = 0.05) {
  model <- match.arg(model)
  p <- bt[[paste0("p_", model, "_", target)]]; y <- bt[[paste0("y_", target)]]
  odds <- bt[[paste0("odds_", target)]]
  res <- bind_rows(lapply(thresholds, function(t) {
    k <- !is.na(p) & p >= t
    r <- data.frame(rule = sprintf("p >= %.0f%%", 100 * t), bets = sum(k),
                    hit_rate = round(mean(y[k]), 3),
                    breakeven_odds = round(1 / mean(y[k]), 2))
    if (!is.null(odds)) {
      k2 <- k & !is.na(odds)
      r$profit <- round(sum(ifelse(y[k2], odds[k2] - 1, -1)), 2)
      r$roi_pct <- round(100 * r$profit / sum(k2), 1)
    }
    r
  }))
  if (!is.null(odds)) {
    k <- !is.na(p) & !is.na(odds) & p > 1 / odds + margin
    res <- bind_rows(res, data.frame(
      rule = sprintf("edge > %.0f pts", 100 * margin), bets = sum(k),
      hit_rate = round(mean(y[k]), 3), breakeven_odds = round(1 / mean(y[k]), 2),
      profit = round(sum(ifelse(y[k], odds[k] - 1, -1)), 2),
      roi_pct = round(100 * sum(ifelse(y[k], odds[k] - 1, -1)) / sum(k), 1)))
  }
  res[res$bets > 0, ]
}

plot_calibration <- function(bt, target = "all3", file = sprintf("nhl_calibration_%s.png", target)) {
  cal <- bind_rows(
    mutate(calibration_table(bt, "pois", target), model = "Poisson"),
    mutate(calibration_table(bt, "emp",  target), model = "Empirical")) %>%
    filter(n >= 10)                       # bins with < 10 games are too noisy to plot
  lab <- if (target == "all3") "goal in all 3 periods" else paste("goal in", target)
  p <- ggplot(cal, aes(mean_pred, observed, colour = model)) +
    geom_abline(linetype = "dashed", colour = "grey50") +
    geom_line() + geom_point(aes(size = n)) +
    scale_x_continuous(labels = scales::percent, limits = c(NA, 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(NA, 1)) +
    scale_colour_manual(values = c(Poisson = "#1d4ed8", Empirical = "#ea580c")) +
    labs(title = paste("Backtest calibration:", lab),
         subtitle = paste(nrow(bt), "games, predictions made with prior games only.",
                          "Dashed line = perfect calibration (bins with < 10 games hidden)"),
         x = "Predicted probability", y = "Observed frequency",
         colour = "Model", size = "Games") +
    theme_minimal(base_size = 11)
  ggsave(file, p, width = 7, height = 6, dpi = 150, bg = "white")
  message("Saved: ", file)
  invisible(p)
}

# One call that prints everything
run_backtest <- function(d, min_games = 5, last_n = NULL) {
  bt <- backtest(d, min_games, last_n)
  cat("\n== Backtest:", nrow(bt), "games ==\n")
  cat("\n-- Scores, goal in all 3 periods (lower brier/logloss = better) --\n")
  print(backtest_scores(bt, "all3"), row.names = FALSE)
  for (m in c("pois", "emp")) {
    cat(sprintf("\n-- Calibration (%s) --\n", m)); print(as.data.frame(calibration_table(bt, m)), row.names = FALSE)
    cat(sprintf("\n-- Thresholds (%s) --\n", m));  print(threshold_table(bt, m), row.names = FALSE)
  }
  plot_calibration(bt)
  write.csv(bt, "nhl_backtest.csv", row.names = FALSE)
  invisible(bt)
}

# =============================================================================
# PLAYERS: key forwards & main goalie, computed from the cached play-by-play
# (rosterSpots = players dressed for the game; goal plays give scorer and
# assists; goalieInNetId gives the goalie in net -> the first one seen for a
# team is its starter).
# =============================================================================
KEY_SHARE    <- 0.25   # key forward: involved (goal or assist) in >= 25% of team goals
GOALIE_SHARE <- 0.50   # main goalie: started >= 50% of the team's games
INJURY_URL   <- "https://site.api.espn.com/apis/site/v2/sports/hockey/nhl/injuries"

`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

parse_players <- function(file) {
  g  <- fromJSON(file, simplifyVector = TRUE)
  id <- as.character(g$id); dt <- as.character(g$gameDate)
  tm <- setNames(c(g$homeTeam$abbrev, g$awayTeam$abbrev),
                 as.character(c(g$homeTeam$id, g$awayTeam$id)))
  rs <- g$rosterSpots
  roster <- if (NROW(rs)) data.frame(
    game_id = id, date = dt, team = unname(tm[as.character(rs$teamId)]),
    player_id = rs$playerId, name = paste(rs$firstName$default, rs$lastName$default),
    pos = rs$positionCode, stringsAsFactors = FALSE)
  pl <- g$plays
  if (!NROW(pl)) return(list(roster = roster))
  det   <- pl$details
  col   <- function(x) det[[x]] %||% rep(NA_integer_, nrow(pl))
  ptype <- pl$periodDescriptor$periodType
  isg   <- pl$typeDescKey == "goal" & ptype %in% c("REG", "OT")
  ig    <- which(isg)                       # empty for a game with no goal (e.g. 0-0 then shootout)
  goals <- if (length(ig)) data.frame(
    game_id = id, team = unname(tm[as.character(col("eventOwnerTeamId")[ig])]),
    scorer = col("scoringPlayerId")[ig],
    a1 = col("assist1PlayerId")[ig], a2 = col("assist2PlayerId")[ig])
  gin <- col("goalieInNetId"); k <- !is.na(gin) & ptype %in% c("REG", "OT")
  starters <- NULL
  if (any(k) && !is.null(roster)) {
    s <- data.frame(player_id = gin[k])
    s$team <- roster$team[match(s$player_id, roster$player_id)]
    s <- s[!is.na(s$team) & !duplicated(s$team), ]
    if (nrow(s)) starters <- data.frame(game_id = id, date = dt, team = s$team, player_id = s$player_id)
  }
  list(roster = roster, goals = goals, starters = starters)
}

build_players <- function(seasons = SEASONS, game_types = GAME_TYPES) {
  ps <- lapply(cached_files(seasons, game_types), function(f)
    tryCatch(parse_players(f), error = function(e) {
      warning("Skipped players of ", basename(f), ": ", conditionMessage(e), call. = FALSE)
      list()
    }))
  grab <- function(x) bind_rows(lapply(ps, `[[`, x))
  P <- list(roster = grab("roster"), goals = grab("goals"), starters = grab("starters"))
  P$roster$season <- substr(P$roster$game_id, 1, 4)
  P
}

check_players <- function(P) {
  if (is.null(P) || !NROW(P$roster))
    stop("Player data is empty: run  P <- build_players()  (or  PL <- build_players()  in the API) first.",
         call. = FALSE)
}

# Key forwards and main goalie of every team, using only games before `before`
# (same season as the last game before that date).
key_players <- function(P, before = NULL, key_share = KEY_SHARE,
                        goalie_share = GOALIE_SHARE, min_team_games = 5) {
  check_players(P)
  r <- P$roster
  if (!is.null(before)) r <- r[r$date < before, ]
  if (!NROW(r)) return(NULL)
  r  <- r[r$season == r$season[r$date == max(r$date)][1], ]
  gl <- P$goals[P$goals$game_id %in% r$game_id, ]
  st <- P$starters[P$starters$game_id %in% r$game_id, ]

  team_gp    <- r %>% distinct(team, game_id) %>% count(team, name = "team_gp")
  team_goals <- gl %>% count(team, name = "team_goals")
  info <- r %>% arrange(desc(date)) %>% distinct(player_id, .keep_all = TRUE) %>%
    select(player_id, name, pos)

  fwd <- bind_rows(transmute(gl, team, player_id = scorer, g = 1L, a = 0L),
                   transmute(gl, team, player_id = a1,     g = 0L, a = 1L),
                   transmute(gl, team, player_id = a2,     g = 0L, a = 1L)) %>%
    filter(!is.na(player_id)) %>%
    group_by(team, player_id) %>% summarise(goals = sum(g), assists = sum(a), .groups = "drop") %>%
    left_join(team_goals, by = "team") %>% left_join(team_gp, by = "team") %>%
    left_join(info, by = "player_id") %>%
    mutate(share = (goals + assists) / team_goals) %>%
    filter(pos %in% c("C", "L", "R"), share >= key_share, team_gp >= min_team_games) %>%
    mutate(role = "key forward")

  gk <- st %>% count(team, player_id, name = "starts") %>%
    left_join(team_gp, by = "team") %>% left_join(info, by = "player_id") %>%
    mutate(share = starts / team_gp) %>%
    filter(share >= goalie_share, team_gp >= min_team_games) %>%
    mutate(role = "main goalie")

  bind_rows(fwd, gk) %>% select(team, role, player_id, name, pos, share, goals, assists, starts, team_gp)
}

# ---------------------------------------------------------------- injuries ---
ACCENT_FROM <- utf8ToInt("\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u00e7\u00e8\u00e9\u00ea\u00eb\u00ec\u00ed\u00ee\u00ef\u00f1\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8\u00f9\u00fa\u00fb\u00fc\u00fd\u00ff\u0161\u017e\u010d\u0159\u0107\u0142\u0144\u015b\u017a\u017c\u0151\u0171\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5\u00c7\u00c8\u00c9\u00ca\u00cb\u00cc\u00cd\u00ce\u00cf\u00d1\u00d2\u00d3\u00d4\u00d5\u00d6\u00d8\u00d9\u00da\u00db\u00dc\u00dd\u0160\u017d\u010c\u0158\u0106\u0141\u0143\u015a\u0179\u017b\u0150\u0170")
ACCENT_TO   <- utf8ToInt("aaaaaaceeeeiiiinoooooouuuuyyszcrclnszzouAAAAAACEEEEIIIINOOOOOOUUUUYSZCRCLNSZZOU")
norm_name <- function(x) {             # accented names -> plain lowercase ASCII (any locale)
  x <- enc2utf8(as.character(x)); x[is.na(x)] <- ""
  x <- vapply(x, function(s) {
    cp <- utf8ToInt(s); if (anyNA(cp)) return(s)
    k <- match(cp, ACCENT_FROM); cp[!is.na(k)] <- ACCENT_TO[k[!is.na(k)]]
    intToUtf8(cp)
  }, "", USE.NAMES = FALSE)
  x <- gsub("[^a-z ]", "", tolower(x))
  trimws(gsub(" +", " ", x))
}
short_key <- function(x) {             # "auston matthews" -> "a matthews"
  w <- strsplit(norm_name(x), " +")
  vapply(w, function(v) if (length(v) < 2) paste(v, collapse = "")
         else paste(substr(v[1], 1, 1), paste(v[-1], collapse = "")), "")
}

# League-wide injury report (ESPN public endpoint, unofficial, may change).
get_injuries <- function(url = INJURY_URL) {
  j <- tryCatch(fromJSON(url, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(j$injuries)) { warning("Injury report unavailable"); return(NULL) }
  rows <- list()
  for (tm in j$injuries) for (inj in tm$injuries) {
    nm <- inj$athlete$displayName %||% NA
    if (is.na(nm)) next
    st <- inj$status %||% inj$type$description %||% "listed"
    if (is.list(st)) st <- st$name %||% "listed"
    rows[[length(rows) + 1]] <- data.frame(
      name = nm, status = as.character(st),
      comment = as.character(inj$shortComment %||% ""), stringsAsFactors = FALSE)
  }
  bind_rows(rows)
}

match_injury <- function(names, inj) {
  if (is.null(inj) || !nrow(inj)) return(rep(NA_character_, length(names)))
  i <- match(norm_name(names), norm_name(inj$name))
  j <- match(short_key(names), short_key(inj$name))
  inj$status[ifelse(is.na(i), j, i)]
}

abbr <- function(name) sub("^(\\S)\\S*\\s+", "\\1. ", name)   # "Auston Matthews" -> "A. Matthews"

# Notes for UPCOMING games: key player on the injury report, or not dressed
# for his team's last game.
upcoming_notes <- function(games, P, date, injuries = NULL) {
  kp <- key_players(P, before = date)
  notes <- character(nrow(games)); n_abs <- integer(nrow(games))
  if (is.null(kp) || !nrow(kp)) return(data.frame(notes = notes, n_absent = n_abs))
  r <- P$roster[P$roster$date < date, ]
  for (i in seq_len(nrow(games))) {
    msg <- c()
    for (tm in c(games$home[i], games$away[i])) {
      k <- kp[kp$team == tm, ]
      if (!nrow(k)) next
      rt <- r[r$team == tm, ]; last_id <- rt$game_id[rt$date == max(rt$date)][1]
      dressed <- k$player_id %in% r$player_id[r$game_id %in% last_id]
      inj     <- match_injury(k$name, injuries)
      for (j in seq_len(nrow(k))) {
        why <- c(if (!is.na(inj[j])) paste0("injury report: ", inj[j]),
                 if (!dressed[j]) "did not play last game")
        if (!length(why)) next
        n_abs[i] <- n_abs[i] + 1
        what <- if (k$role[j] == "main goalie") sprintf("main G %s (%.0f%% starts)", abbr(k$name[j]), 100 * k$share[j])
                else sprintf("key F %s (%.0f%% of goals)", abbr(k$name[j]), 100 * k$share[j])
        msg <- c(msg, sprintf("%s %s: %s", tm, what, paste(why, collapse = ", ")))
      }
    }
    notes[i] <- paste(msg, collapse = " | ")
  }
  data.frame(notes = notes, n_absent = n_abs, stringsAsFactors = FALSE)
}

# Notes for PLAYED games: who was actually missing (not dressed), and whether
# the main goalie did not start.
played_notes <- function(games, P) {
  notes <- character(nrow(games)); n_abs <- integer(nrow(games))
  for (dt in unique(games$date)) {
    kp <- key_players(P, before = dt)
    if (is.null(kp) || !nrow(kp)) next
    for (i in which(games$date == dt)) {
      gid <- games$game_id[i]; msg <- c()
      dressed <- P$roster$player_id[P$roster$game_id == gid]
      started <- P$starters$player_id[P$starters$game_id == gid]
      for (tm in c(games$home[i], games$away[i])) {
        k <- kp[kp$team == tm, ]
        for (j in seq_len(nrow(k))) {
          pid <- k$player_id[j]; m <- NULL
          if (k$role[j] == "main goalie" && !pid %in% started)
            m <- sprintf("%s main G %s %s", tm, abbr(k$name[j]),
                         if (pid %in% dressed) "on bench (backup started)" else "absent")
          if (k$role[j] == "key forward" && !pid %in% dressed)
            m <- sprintf("%s key F %s absent", tm, abbr(k$name[j]))
          if (!is.null(m)) { msg <- c(msg, m); n_abs[i] <- n_abs[i] + 1 }
        }
      }
      notes[i] <- paste(msg, collapse = " | ")
    }
  }
  data.frame(notes = notes, n_absent = n_abs, stringsAsFactors = FALSE)
}

# =============================================================================
# GAMES OF THE DAY
# =============================================================================
LOG_FILE <- "nhl_predictions_log.csv"

nhl_today <- function() format(Sys.time(), "%Y-%m-%d", tz = "America/New_York")

# Schedule for one date (NHL dates are North-American: games of "2026-11-03"
# start in the evening ET, i.e. at night in France).
schedule_day <- function(date = nhl_today()) {
  sc <- get_json(sprintf("%s/schedule/%s", API, date))
  gw <- sc$gameWeek
  i  <- which(gw$date == date)
  if (!length(i) || !NROW(gw$games[[i]])) return(tibble())
  g <- gw$games[[i]]
  tibble(game_id = as.character(g$id), date = date, game_type = g$gameType,
         start = format(as.POSIXct(g$startTimeUTC, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                        "%d/%m %H:%M", tz = "Europe/Paris"),
         home = g$homeTeam$abbrev, away = g$awayTeam$abbrev) %>%
    filter(game_type %in% GAME_TYPES) %>% select(-game_type)
}

# Predictions for the games of `date`. Probabilities in %, min_odds = the odds
# below which the "Yes, goal in all 3 periods" bet loses money on average.
# Also writes/updates LOG_FILE (keeps any odds you typed in column odds_all3).
predict_day <- function(d, P, date = nhl_today(), min_games = 5, last_n = NULL,
                        games = NULL, injuries = get_injuries(), log = TRUE, form_n = 10) {
  if (is.null(games)) games <- schedule_day(date)
  if (!nrow(games)) { message("No game on ", date); return(tibble()) }
  prior <- d[d$date < date, ]
  pr <- predict_games(games, prior, min_games, last_n)
  nt <- upcoming_notes(games, P, date, injuries)
  # recent form: games with a goal in all 3 periods among each team's last form_n games
  form <- last_n_counts(prior, form_n) %>% select(team, games, all3)
  out <- pr %>% bind_cols(nt) %>%
    left_join(rename(form, home_games = games, home_hits = all3), by = c("home" = "team")) %>%
    left_join(rename(form, away_games = games, away_hits = all3), by = c("away" = "team")) %>%
    mutate(min_odds_pois = round(1 / p_pois_all3, 2),
           min_odds_emp  = round(1 / p_emp_all3, 2),
           across(starts_with("p_"), ~ round(100 * .x, 1)))
  # explicit names, e.g. game_1gpp_last10_home = games with >= 1 goal per period
  # among the home team's last 10 games; games_last10_home = games counted (< 10 early season)
  out <- out %>% rename(!!paste0("game_1gpp_last", form_n, "_home") := home_hits,
                        !!paste0("game_1gpp_last", form_n, "_away") := away_hits,
                        !!paste0("games_last", form_n, "_home")     := home_games,
                        !!paste0("games_last", form_n, "_away")     := away_games)
  if (log) {
    new <- out %>% mutate(predicted_at = format(Sys.time(), "%Y-%m-%d %H:%M"), odds_all3 = NA_real_)
    if (file.exists(LOG_FILE)) {
      old <- read_log()
      new$odds_all3 <- old$odds_all3[match(new$game_id, old$game_id)]
      new <- bind_rows(old[!old$game_id %in% new$game_id, ], new)
    }
    write_log(new)
    update_log_results(d)
  }
  out
}

# The log is written with ";" and decimal "," so that Excel (French settings)
# opens it directly in columns. read_log() also accepts "," files and dates
# rewritten by Excel (02/11/2026), so you can type your odds and save in Excel.
read_log <- function(file = LOG_FILE) {
  first <- readLines(file, n = 1, warn = FALSE, encoding = "UTF-8")
  if (!length(first)) return(data.frame())
  n_semi  <- lengths(regmatches(first, gregexpr(";", first, fixed = TRUE)))
  n_comma <- lengths(regmatches(first, gregexpr(",", first, fixed = TRUE)))
  lg <- utils::read.table(file, header = TRUE, sep = if (n_semi > n_comma) ";" else ",",
                          quote = "\"", colClasses = "character", na.strings = c("", "NA"),
                          fileEncoding = "UTF-8-BOM", check.names = FALSE, comment.char = "",
                          fill = TRUE)
  names(lg) <- trimws(sub("^\ufeff", "", names(lg)))
  if (!is.null(lg$date)) lg$date <- norm_date(lg$date)
  keep_text <- c("game_id", "date", "start", "home", "away", "result", "notes",
                 "notes_pregame", "basis", "predicted_at")
  for (cl in setdiff(names(lg), keep_text)) {
    v <- suppressWarnings(as.numeric(sub(",", ".", lg[[cl]], fixed = TRUE)))
    if (all(is.na(v) == is.na(lg[[cl]]))) lg[[cl]] <- v
  }
  if (!is.null(lg$game_id)) lg$game_id <- sub("\\.0+$", "", trimws(lg$game_id))
  lg
}

norm_date <- function(x) {
  x <- trimws(as.character(x))
  out <- as.Date(x, format = "%Y-%m-%d", optional = TRUE)
  i <- is.na(out) & !is.na(x); out[i] <- as.Date(x[i], format = "%d/%m/%Y", optional = TRUE)
  i <- is.na(out) & !is.na(x); out[i] <- as.Date(x[i], format = "%d/%m/%y", optional = TRUE)
  as.character(out)
}

LOG_COLS <- c("game_id", "date", "start", "home", "away",
              "game_1gpp_last10_home", "game_1gpp_last10_away", "games_last10_home", "games_last10_away",
              "p_pois_all3", "p_emp_all3", "min_odds_pois", "min_odds_emp",
              "odds_all3", "result", "profit", "notes", "basis", "predicted_at")
write_log <- function(lg) {
  lg <- as.data.frame(lg)
  first <- intersect(LOG_COLS, names(lg))
  lg <- lg[order(lg$date, lg$game_id), c(first, setdiff(names(lg), first))]
  ok <- tryCatch({ utils::write.csv2(lg, LOG_FILE, row.names = FALSE, na = "", fileEncoding = "UTF-8"); TRUE },
                 error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) warning("Could not write ", LOG_FILE, " (is it open in Excel? close it and retry)", call. = FALSE)
}

# Adds game_1gpp_last<n>_home/_away (games with >= 1 goal in every period among
# each team's last n games) and games_last<n>_home/_away, using only games
# played BEFORE each row's date. df needs columns date, home, away.
add_form_columns <- function(df, d, n = 10) {
  df$date <- as.character(df$date)
  nm <- paste0(c("game_1gpp_last", "game_1gpp_last", "games_last", "games_last"),
               n, c("_home", "_away", "_home", "_away"))
  for (cl in nm) if (is.null(df[[cl]])) df[[cl]] <- NA_integer_
  for (dt in unique(df$date)) {
    prior <- d[d$date < dt, ]
    if (!nrow(prior)) next
    f <- last_n_counts(prior, n)
    i <- which(df$date == dt)
    df[[nm[1]]][i] <- f$all3[match(df$home[i], f$team)]
    df[[nm[2]]][i] <- f$all3[match(df$away[i], f$team)]
    df[[nm[3]]][i] <- f$games[match(df$home[i], f$team)]
    df[[nm[4]]][i] <- f$games[match(df$away[i], f$team)]
  }
  df
}

# Fills column `result` of the log for every game now finished:
# "Won" = at least one goal in each of the 3 regulation periods, otherwise "Lost".
# If you typed the odds in odds_all3, `profit` = result of a 1-unit bet.
update_log_results <- function(d) {
  if (!file.exists(LOG_FILE)) return(invisible(NULL))
  lg <- read_log()
  if (!nrow(lg)) return(invisible(lg))
  if (is.null(lg$home) || is.null(lg$away) || is.null(lg$date)) {
    warning(LOG_FILE, " has no date/home/away columns: results not filled. ",
            "Rename or delete the file, it will be recreated by /today.", call. = FALSE)
    return(invisible(lg))
  }
  # game_id lost (e.g. column deleted in Excel): rebuild it from date + teams
  if (is.null(lg$game_id)) lg$game_id <- NA_character_
  o  <- game_outcomes(d)
  miss <- is.na(lg$game_id) | lg$game_id == ""
  lg$game_id[miss] <- o$game_id[match(paste(lg$date, lg$home, lg$away)[miss],
                                      paste(o$date, o$home, o$away))]
  # recent form of both teams, recomputed for every row with games BEFORE its date
  lg <- add_form_columns(lg, d, 10)
  k  <- match(lg$game_id, o$game_id)
  lg$result <- ifelse(!is.na(k), ifelse(o$y_all3[k], "Won", "Lost"), "not played yet")
  odds <- suppressWarnings(as.numeric(lg$odds_all3 %||% NA))
  lg$profit <- ifelse(lg$result == "Won" & !is.na(odds), round(odds - 1, 2),
                      ifelse(lg$result == "Lost" & !is.na(odds), -1, NA))
  write_log(lg)
  invisible(lg)
}

# One call: refresh the data, then predict today's games.
today <- function(date = nhl_today(), ...) {
  update_data()
  d <- build_dataset(); P <- build_players()
  out <- predict_day(d, P, date, ...)
  print(as.data.frame(out %>% select(start, home, away, game_1gpp_last10_home, game_1gpp_last10_away, p_pois_P1, p_pois_P2, p_pois_P3,
                                     p_pois_all3, p_emp_all3, min_odds_pois, notes)), row.names = FALSE)
  invisible(out)
}

# =============================================================================
# RESULTS OF PLAYED GAMES: pre-game probabilities vs what happened
# =============================================================================
# Probabilities are recomputed with games BEFORE each date only (identical to
# what predict_day() gave that day). Notes = absences that actually happened.
# Odds you entered in LOG_FILE are joined to compute profit (1 unit per bet).
results_table <- function(d, P, from = as.character(as.Date(nhl_today()) - 7),
                          to = as.character(as.Date(nhl_today()) - 1),
                          min_games = 5, last_n = NULL) {
  games <- game_outcomes(d) %>% filter(date >= from, date <= to)
  if (!nrow(games)) stop("No finished game between ", from, " and ", to)
  res <- bind_rows(lapply(sort(unique(games$date)), function(dt)
    predict_games(games[games$date == dt, ], d[d$date < dt, ], min_games, last_n))) %>%
    arrange(date, game_id)
  res <- bind_cols(res, played_notes(res, P)) %>%
    mutate(bet = ifelse(y_all3, "Won", "Lost"))
  if (file.exists(LOG_FILE)) {
    lg <- tryCatch(read_log(), error = function(e) data.frame())
    if (!is.null(lg$game_id) && !is.null(lg$odds_all3)) {
      m <- match(res$game_id, lg$game_id)
      res$odds_all3 <- as.numeric(lg$odds_all3[m])
      res$notes_pregame <- if (!is.null(lg$notes)) lg$notes[m] else NA
      res$profit <- ifelse(is.na(res$odds_all3), NA, ifelse(res$y_all3, res$odds_all3 - 1, -1))
    }
  }
  res
}

results_summary <- function(res) {
  r <- res[!is.na(res$p_pois_all3), ]
  out <- data.frame(
    group = c("All games", "With a key absence", "No key absence"),
    games = c(nrow(r), sum(r$n_absent > 0), sum(r$n_absent == 0)),
    won_pct = round(100 * c(mean(r$y_all3), mean(r$y_all3[r$n_absent > 0]), mean(r$y_all3[r$n_absent == 0])), 1),
    mean_poisson = round(100 * c(mean(r$p_pois_all3), mean(r$p_pois_all3[r$n_absent > 0]), mean(r$p_pois_all3[r$n_absent == 0])), 1),
    mean_empirical = round(100 * c(mean(r$p_emp_all3), mean(r$p_emp_all3[r$n_absent > 0]), mean(r$p_emp_all3[r$n_absent == 0])), 1))
  if (!is.null(r$profit) && any(!is.na(r$profit)))
    out$profit_units <- c(sum(r$profit, na.rm = TRUE), sum(r$profit[r$n_absent > 0], na.rm = TRUE),
                          sum(r$profit[r$n_absent == 0], na.rm = TRUE))
  out
}

# Compact version of the notes for the plot (full text stays in the table)
short_notes <- function(x) {
  x <- gsub("(\\w+) main G \\S+ \\S+ on bench \\(backup started\\)", "\\1 backup G", x)
  x <- gsub("(\\w+) main G (\\S+ \\S+) absent", "\\1 G \\2 out", x)
  x <- gsub("(\\w+) key F (\\S+ \\S+) absent", "\\1 F \\2 out", x)
  gsub(" \\| ", "; ", x)
}

# Dot plot, one row per game: Poisson and empirical pre-game probability of
# "goal in all 3 periods", the bet result, and the absence notes.
# threshold: optional vertical line (e.g. 0.70 for your betting rule).
plot_results <- function(res, file = "nhl_results.png", threshold = NULL) {
  r <- res %>% filter(!is.na(p_pois_all3)) %>% arrange(date, game_id) %>%
    mutate(row = factor(sprintf("%s  %s vs %s", format(as.Date(date), "%b %d"), home, away),
                        levels = rev(unique(sprintf("%s  %s vs %s", format(as.Date(date), "%b %d"), home, away)))),
           note = ifelse(n_absent > 0, short_notes(notes), ""))
  if (!nrow(r)) stop("No game with predictions in this range")
  miss <- res %>% filter(is.na(p_pois_all3))
  if (nrow(miss)) message("Not plotted (a team has < min_games games): ",
                          paste(sprintf("%s %s-%s", miss$date, miss$home, miss$away), collapse = ", "))
  long <- r %>% select(row, Poisson = p_pois_all3, Empirical = p_emp_all3) %>%
    pivot_longer(-row, names_to = "model", values_to = "p")
  lo <- max(0, floor(min(long$p) * 10) / 10 - 0.05)
  p <- ggplot() +
    geom_segment(data = r, aes(x = p_emp_all3, xend = p_pois_all3, y = row, yend = row),
                 colour = "#c9c8c4", linewidth = 0.6) +
    { if (!is.null(threshold)) geom_vline(xintercept = threshold, linetype = "dashed", colour = "#52514e") } +
    geom_point(data = long, aes(p, row, colour = model, shape = model), size = 2.8) +
    geom_text(data = r, aes(x = 1.02, y = row, label = bet,
                            colour = ifelse(y_all3, "Bet won", "Bet lost")),
              hjust = 0, fontface = "bold", size = 3.3, show.legend = FALSE) +
    geom_text(data = r, aes(x = 1.10, y = row, label = note), hjust = 0, size = 2.6, colour = "#52514e") +
    scale_colour_manual(values = c(Poisson = "#2a78d6", Empirical = "#eb6f14",
                                   "Bet won" = "#006300", "Bet lost" = "#d03b3b"),
                        breaks = c("Poisson", "Empirical"), name = "Model") +
    scale_shape_manual(values = c(Poisson = 16, Empirical = 17),
                       breaks = c("Poisson", "Empirical"), name = "Model") +
    scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
    coord_cartesian(xlim = c(lo, 1), clip = "off") +
    labs(title = "Pre-game probability of a goal in all 3 periods vs bet result",
         subtitle = sprintf("%d games, %d won (%.0f%%). Mean predicted: Poisson %.0f%%, empirical %.0f%%",
                            nrow(r), sum(r$y_all3), 100 * mean(r$y_all3),
                            100 * mean(r$p_pois_all3), 100 * mean(r$p_emp_all3)),
         x = "Predicted probability (\"Yes\" in every period)", y = NULL,
         caption = paste0("Right: bet result, then key absences (forward involved in >= 25% of team goals, goalie starting >= 50% of games)",
                          if (nrow(miss)) paste0("\nNot shown, too few games played: ",
                                                 paste(sprintf("%s-%s", miss$home, miss$away), collapse = ", ")))) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top", legend.justification = "left",
          panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          plot.margin = margin(10, 230, 10, 10), plot.caption = element_text(colour = "#52514e", hjust = 0))
  ggsave(file, p, width = 11, height = 2 + 0.3 * nrow(r), dpi = 150, bg = "white", limitsize = FALSE)
  message("Saved: ", file)
  invisible(p)
}

# =============================================================================
# BET PREVIEW: only the games that pass your selection rule
# =============================================================================
# Keeps a game when BOTH teams had a goal in all 3 periods in at least
# `min_hits` of their last `n_last` games (all venues, before `date`) AND the
# model probability of "goal in all 3 periods" is > p_min.
#   model = "either" (Poisson OR empirical > p_min), "both", "poisson", "empirical"
# For a past date, the result (Won/Lost) is added when the game is in the cache.
bet_preview <- function(d, P, date = nhl_today(), n_last = 10, min_hits = 9,
                        p_min = 0.70, model = c("either", "both", "poisson", "empirical"),
                        games = NULL, injuries = get_injuries(), ...) {
  model <- match.arg(model)
  if (is.null(games)) games <- schedule_day(date)
  if (!nrow(games)) { message("No game on ", date); return(tibble()) }
  pr <- predict_day(d, P, date, games = games, injuries = injuries, log = FALSE,
                    form_n = n_last, ...)

  hh <- pr[[paste0("game_1gpp_last", n_last, "_home")]]
  ah <- pr[[paste0("game_1gpp_last", n_last, "_away")]]
  form_ok <- !is.na(hh) & !is.na(ah) & hh >= min_hits & ah >= min_hits
  pp <- pr$p_pois_all3 / 100; pe <- pr$p_emp_all3 / 100      # predict_day gives %
  prob_ok <- switch(model,
    either    = (!is.na(pp) & pp > p_min) | (!is.na(pe) & pe > p_min),
    both      = !is.na(pp) & !is.na(pe) & pp > p_min & pe > p_min,
    poisson   = !is.na(pp) & pp > p_min,
    empirical = !is.na(pe) & pe > p_min)

  out <- pr[form_ok & prob_ok, ] %>%
    select(game_id, date, start, home, away,
           all_of(paste0(c("game_1gpp_last", "game_1gpp_last", "games_last", "games_last"),
                         n_last, c("_home", "_away", "_home", "_away"))),
           p_pois_all3, p_emp_all3, min_odds_pois, min_odds_emp, basis, notes)
  played <- game_outcomes(d)
  out$result <- ifelse(out$game_id %in% played$game_id,
                       ifelse(played$y_all3[match(out$game_id, played$game_id)], "Won", "Lost"),
                       "not played yet")
  message(sprintf("%s: %d of %d games pass (both teams >= %d/%d games with a goal in every period, %s model > %.0f%%)",
                  date, nrow(out), nrow(pr), min_hits, n_last, model, 100 * p_min))
  out
}

# One call: refresh, then table + summary + plot for the last 7 days.
results <- function(from = as.character(as.Date(nhl_today()) - 7),
                    to = as.character(as.Date(nhl_today()) - 1), threshold = NULL, ...) {
  update_data()
  d <- build_dataset(); P <- build_players()
  update_log_results(d)
  res <- results_table(d, P, from, to, ...)
  print(results_summary(res), row.names = FALSE)
  plot_results(res, threshold = threshold)
  write.csv(res, "nhl_results.csv", row.names = FALSE, na = "")
  invisible(res)
}

# ------------------------------------------------------- run as a script ---
if (sys.nframe() == 0) {
  update_data()
  d   <- build_dataset()
  tab <- team_period_summary(d)
  write.csv(tab, "nhl_team_periods.csv", row.names = FALSE)
  write.csv(d,   "nhl_games_periods_long.csv", row.names = FALSE)
  print(league_reference(d))
  print_rankings(tab)
  plot_period_heatmap(tab)
  last10 <- last_n_counts(d, 10)
  write.csv(last10, "nhl_last10_by_period.csv", row.names = FALSE)
  plot_last_n_heatmap(last10, 10)
  run_backtest(d)
}
