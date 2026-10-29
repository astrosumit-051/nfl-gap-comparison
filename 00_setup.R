# 00_setup.R
# Install missing packages, check the raw data, and save a compact copy of the
# tracking data that holds only the clean designed runs.
#
# Output:
#   cache/plays_runs.rds        one row per clean designed run (plays.csv columns)
#   cache/players.rds           players.csv
#   cache/games.rds             games.csv
#   cache/tracking_runs_wkN.rds tracking rows for those runs, one file per week

# ---- Packages ---------------------------------------------------------------

needed <- c("data.table", "ggplot2", "gganimate", "av")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

source("R/config.R")

# ---- Check the raw data -----------------------------------------------------

required_files <- c(
  file.path(DATA_DIR, c("games.csv", "plays.csv", "players.csv")),
  vapply(WEEKS, tracking_file, character(1))
)
not_found <- required_files[!file.exists(required_files)]
if (length(not_found) > 0) {
  stop("These data files were not found. Check DATA_DIR in R/config.R or set ",
       "NFL_DATA_DIR:\n  ", paste(not_found, collapse = "\n  "))
}
message("Data folder: ", DATA_DIR)

# ---- Play, player and game tables -------------------------------------------

plays   <- fread(file.path(DATA_DIR, "plays.csv"), na.strings = c("", "NA"))
players <- fread(file.path(DATA_DIR, "players.csv"), na.strings = c("", "NA"))
games   <- fread(file.path(DATA_DIR, "games.csv"), na.strings = c("", "NA"))

# A designed run has no value in passResult (C = completion, R = QB scramble).
# Plays cancelled by a penalty are removed.
runs <- plays[is.na(passResult) & playNullifiedByPenalty == "N"]
runs <- merge(runs, games[, .(gameId, week)], by = "gameId")
message(sprintf("Clean designed runs: %d (weeks %d to %d)",
                nrow(runs), min(runs$week), max(runs$week)))

saveRDS(runs, file.path(CACHE_DIR, "plays_runs.rds"))
saveRDS(players, file.path(CACHE_DIR, "players.rds"))
saveRDS(games, file.path(CACHE_DIR, "games.rds"))

# ---- Tracking: keep only the run plays --------------------------------------

keep_cols <- c("gameId", "playId", "nflId", "frameId", "club", "playDirection",
               "x", "y", "s", "a", "dis", "o", "dir", "event")
run_keys <- runs[, .(gameId, playId)]

for (wk in WEEKS) {
  out_file <- file.path(CACHE_DIR, sprintf("tracking_runs_wk%d.rds", wk))
  if (file.exists(out_file)) {
    message("Week ", wk, ": cached, skipping")
    next
  }
  trk <- fread(tracking_file(wk), select = keep_cols, na.strings = c("", "NA"))
  trk <- trk[run_keys, on = .(gameId, playId), nomatch = 0L]
  saveRDS(trk, out_file)
  message(sprintf("Week %d: %d rows, %d runs", wk, nrow(trk),
                  uniqueN(trk, by = c("gameId", "playId"))))
  rm(trk); invisible(gc())
}

message("Setup done.")
