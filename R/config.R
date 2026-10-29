# Shared settings and helpers. Every numbered script sources this file first.
# Run scripts from the project root (open nfl-gap-comparison.Rproj in RStudio).

suppressPackageStartupMessages({
  library(data.table)
})

# ---- Paths ------------------------------------------------------------------

# Raw Big Data Bowl 2024 CSVs. Set NFL_DATA_DIR to override (for example in
# ~/.Renviron) if your copy lives somewhere else.
DEFAULT_DATA_DIR <- file.path(
  "/Users/sumitkumarsah/Downloads/Centre College/DSC 355",
  "nfl_data_bowl/nfl-big-data-bowl-2024"
)
DATA_DIR  <- Sys.getenv("NFL_DATA_DIR", unset = DEFAULT_DATA_DIR)
CACHE_DIR <- "cache"
OUT_DIR   <- "output"

WEEKS <- 1:9

tracking_file <- function(week) {
  file.path(DATA_DIR, sprintf("tracking_2024_week_%d.csv", week))
}

# ---- Field constants --------------------------------------------------------

FIELD_LENGTH <- 120
FIELD_WIDTH  <- 160 / 3   # 53.33 yards
FPS          <- 10        # tracking frames per second

# ---- Analysis settings (from 01_project_plan.md) ----------------------------

OL_POSITIONS     <- c("T", "G", "C")
SHORT_YARDAGE_TO <- 2     # 3rd/4th and this many yards or fewer
GOAL_LINE_CUTOFF <- 5     # drop runs that start inside the opponent's 5

# ---- Helpers ----------------------------------------------------------------

# Run success: 40% of yards to go on 1st down, 60% on 2nd, 100% on 3rd/4th.
run_success <- function(down, yards_to_go, yards_gained) {
  need <- fifelse(down == 1, 0.4, fifelse(down == 2, 0.6, 1.0)) * yards_to_go
  as.integer(yards_gained >= need)
}

# Flip a play that goes left so the offense always moves toward +x.
# After flipping, +y is the offense's left.
flip_tracking <- function(dt) {
  dt[playDirection == "left", `:=`(
    x   = FIELD_LENGTH - x,
    y   = FIELD_WIDTH - y,
    dir = (dir + 180) %% 360,
    o   = (o + 180) %% 360
  )]
  # Velocity components. NFL `dir` is degrees clockwise from +y.
  dt[, `:=`(
    vx = s * sin(dir * pi / 180),
    vy = s * cos(dir * pi / 180)
  )]
  invisible(dt)
}

dir.create(CACHE_DIR, showWarnings = FALSE)
dir.create(OUT_DIR, showWarnings = FALSE)
