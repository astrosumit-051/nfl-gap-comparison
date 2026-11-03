# nfl_gap_comparison.R
# Designed gap vs best gap vs gap taken. DSC 355 Project 3.
#
# On NFL designed runs from the Big Data Bowl 2024 tracking data (2022 season,
# weeks 1 to 9), compare three gaps: the gap the play was designed for, the
# best gap at the handoff, and the gap the runner took.
#
# How to run: open nfl-gap-comparison.Rproj in RStudio, open this file and
# click Source. The whole run takes about 15 minutes the first time, about half
# of it rendering 40 animations. RStudio's outline (the list icon at the top right of
# the editor) jumps between the steps. Each step reads what the earlier steps
# saved in cache/, so after one full run you can rerun any single step.
#
#   Settings and helpers
#   Step 0  Packages, data check, keep the clean designed runs only
#   Step 1  Flip plays, key frames, line of scrimmage, linemen      (plan 3.1)
#   Step 2  Blocking scheme and designed gap                         (plan 3.2)
#   Step 3  Every gap's width and unblocked defenders at the handoff (plan 3.3)
#   Step 4  Gap taken                                                (plan 3.4)
#   Step 5  Gap score, best gap, the four groups, runner read rates  (plan 3.3, 3.5)
#   Step 6  Checks and scheme animations                             (plan 3.6)
#   Step 7  Figures for the report and slides
#   Step 8  Stress tests of the assumptions and results
#
# Output: cache/ (intermediate tables) and output/ (figures, tables, checks).

# ==== SETTINGS ===================================================================

# Raw Big Data Bowl 2024 CSVs. Anyone whose copy lives somewhere else adds
#   NFL_DATA_DIR=/path/to/nfl-big-data-bowl-2024
# to ~/.Renviron and restarts R, or edits the path below.
DEFAULT_DATA_DIR <- file.path(
  "/Users/sumitkumarsah/Downloads/Centre College/DSC 355",
  "nfl_data_bowl/nfl-big-data-bowl-2024"
)
DATA_DIR <- Sys.getenv("NFL_DATA_DIR", unset = DEFAULT_DATA_DIR)

# Switches. Each can also be set in ~/.Renviron (for example MAKE_ANIMATIONS=FALSE).
MAKE_ANIMATIONS  <- as.logical(Sys.getenv("MAKE_ANIMATIONS", "TRUE"))   # step 6, about 7 minutes
RUN_STRESS_TESTS <- as.logical(Sys.getenv("RUN_STRESS_TESTS", "TRUE"))  # step 8
RUN_VARIANTS     <- as.logical(Sys.getenv("RUN_VARIANTS", "TRUE"))      # step 8 part 6, about a minute

# Work in the folder that holds this file when it is sourced, so cache/ and
# output/ land next to it. (Opening the .Rproj does the same.)
this_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) "")
if (basename(this_file) == "nfl_gap_comparison.R") setwd(dirname(this_file))

CACHE_DIR <- "cache"
OUT_DIR   <- "output"
dir.create(CACHE_DIR, showWarnings = FALSE)
dir.create(OUT_DIR, showWarnings = FALSE)

WEEKS <- 1:9

# ==== STEP 0: PACKAGES =============================================================

needed <- c("data.table", "ggplot2", "gganimate", "av")
missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

# ==== HELPERS ======================================================================

# ---- Field constants and plan settings ---------------------------------------------

FIELD_LENGTH <- 120
FIELD_WIDTH  <- 160 / 3   # 53.33 yards
FPS          <- 10        # tracking frames per second

OL_POSITIONS     <- c("T", "G", "C")
SHORT_YARDAGE_TO <- 2     # 3rd/4th and this many yards or fewer
GOAL_LINE_CUTOFF <- 5     # drop runs that start inside the opponent's 5

tracking_file <- function(week) {
  file.path(DATA_DIR, sprintf("tracking_2024_week_%d.csv", week))
}

# Run success: 40% of yards to go on 1st down, 60% on 2nd, 100% on 3rd/4th.
run_success <- function(down, yards_to_go, yards_gained) {
  need <- fifelse(down == 1, 0.4, fifelse(down == 2, 0.6, 1.0)) * yards_to_go
  as.integer(yards_gained >= need)
}

# Flip a play that goes left so the offense always moves toward +x.
# After flipping, +y is the offense's left. Use as: trk <- flip_tracking(trk)
flip_tracking <- function(dt) {
  dt <- setalloccol(dt)   # tables read with readRDS need this before := adds columns
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

# ---- Gaps ---------------------------------------------------------------------------
#
# Gaps are named by side (L or R, the offense's left or right) and letter:
#   A  between the center and the guard
#   B  between the guard and the tackle
#   C  between the tackle and the end man (on-line TE or extra lineman);
#      with no end man on that side, C is the edge outside the tackle
#   D  outside the end man (only when there is an end man)
# The outermost gap on each side is the edge gap.
#
# Players on each side are ordered by lateral distance from the center at the
# snap: the 1st is the guard, the 2nd the tackle, the 3rd the end man. Typical
# splits are about 1.5 yards per player.

GAP_LETTERS   <- c("A", "B", "C", "D")
DEFAULT_SPLIT <- 1.5   # yards, used when a side has fewer linemen than expected

# One row per run and side with the lateral distance of each edge from the center.
#   d1 = guard, d2 = tackle, d3 = end man (NA if none)
line_edges <- function(linemen) {
  ctr <- linemen[is_center == TRUE, .(gameId, playId, center_y = y, center_x = x)]
  l <- merge(linemen[is_center == FALSE], ctr, by = c("gameId", "playId"))
  l[, `:=`(side = fifelse(y > center_y, "L", "R"), u = abs(y - center_y))]
  setorder(l, gameId, playId, side, u)
  l[, k := seq_len(.N), by = .(gameId, playId, side)]
  e <- l[, .(d1 = u[k == 1][1], d2 = u[k == 2][1], d3 = u[k == 3][1],
             id1 = nflId[k == 1][1], id2 = nflId[k == 2][1], id3 = nflId[k == 3][1]),
         by = .(gameId, playId, side)]
  # Make sure both sides exist for every run
  grid <- ctr[, .(side = c("L", "R")), by = .(gameId, playId)]
  e <- merge(grid, e, by = c("gameId", "playId", "side"), all.x = TRUE)
  e[is.na(d1), d1 := DEFAULT_SPLIT]
  e[is.na(d2), d2 := d1 + DEFAULT_SPLIT]
  e <- merge(e, ctr, by = c("gameId", "playId"))
  e[]
}

# Gap letter for a lateral distance u from the center, given that side's edges.
gap_letter <- function(u, d1, d2, d3) {
  fcase(u < d1, "A",
        u < d2, "B",
        is.na(d3) | u < d3, "C",
        default = "D")
}

# Side and letter for points y on each run. `pts` needs gameId, playId, y.
gap_of_point <- function(pts, edges) {
  pts <- merge(pts, unique(edges[, .(gameId, playId, center_y)]), by = c("gameId", "playId"))
  pts[, side := fifelse(y > center_y, "L", "R")]
  pts <- merge(pts, edges[, .(gameId, playId, side, d1, d2, d3)],
               by = c("gameId", "playId", "side"))
  pts[, gap := paste0(side, gap_letter(abs(y - center_y), d1, d2, d3))]
  pts[, c("center_y", "d1", "d2", "d3") := NULL]
  pts[]
}

# Keep each side's edges in order out from the center (k = 0 is the center).
# By the handoff a lineman can cross inside his neighbour (a tackle blocking
# down past a puller's snap spot, say). He is then held at that neighbour's
# spot, so the gap between them is closed rather than inverted, and the best
# gap and the gap taken see the same gaps. `e` needs gameId, playId, side, k
# and y, and optionally vy.
order_edges <- function(e) {
  setorder(e, gameId, playId, side, k)
  e[, src := {
    reach <- if (side[1] == "L") cummax(y) else cummin(y)
    cummax(fifelse(y == reach, seq_len(.N), 0L))
  }, by = .(gameId, playId, side)]
  cols <- intersect(c("y", "vy"), names(e))
  e[, (cols) := lapply(.SD, function(v) v[src]), by = .(gameId, playId, side), .SDcols = cols]
  e[, src := NULL][]
}

# ---- Gap width and unblocked defenders (used by step 3 and the step 8 variants) ----
#
# Runner and defender arrival times use a simple motion model: start from the
# player's speed toward the gap, accelerate at 4 yd/s^2, top out at 8 yd/s.
# Gap edges are the linemen's lateral positions projected forward with their
# velocity at the handoff, at most 0.5 s ahead (plan rule).
# Width = lateral distance between the two edge players, minus 1 yard. An edge
# gap's outer side is 5 yd outside the end man (or the sideline if nearer).
# A defender is blocked when an offensive player is within 1.5 yd of him AND is
# between him and the gap (closer to the gap, on the same side of him).

GAP_SETTINGS <- list(
  max_projection = 0.5,          # s, furthest the edges are projected ahead
  accel          = 4,            # yd/s^2
  top_speed      = 8,            # yd/s
  edge_cap       = 5,            # yd, widest an edge gap can be
  block_radius   = 1.5,          # yd
  spacer         = 1,            # yd taken off every width (plan rule)
  runner_speed   = "toward_gap", # runner's starting speed: "toward_gap", "zero" or "full"
  snapshot_delay = 0L            # frames after the handoff for the picture of the field
)

# Time to cover distance d from speed v0 (toward the target) with the motion model
time_to_reach <- function(d, v0, accel, top_speed) {
  v0 <- pmin(pmax(v0, 0), top_speed)
  t1 <- (top_speed - v0) / accel
  d1 <- v0 * t1 + 0.5 * accel * t1^2
  fifelse(d <= d1, (-v0 + sqrt(v0^2 + 2 * accel * d)) / accel, t1 + (d - d1) / top_speed)
}

# Every player at the handoff (or `delay` frames after it), one row per player and run
handoff_snapshot <- function(runs, delay = 0L) {
  snap_list <- list()
  for (wk in WEEKS) {
    trk <- readRDS(file.path(CACHE_DIR, sprintf("tracking_prep_wk%d.rds", wk)))
    r <- runs[week == wk, .(gameId, playId, frameId = handoff_frame + delay)]
    snap_list[[wk]] <- trk[r, on = .(gameId, playId, frameId), nomatch = 0L,
                           .(gameId, playId, nflId, role, position, x, y, s, vx, vy)]
    rm(trk); invisible(gc())
  }
  ho <- rbindlist(snap_list)
  merge(ho, runs[, .(gameId, playId, los_x)], by = c("gameId", "playId"))
}

# One row per run and gap: projected width when the runner gets there and the
# unblocked defenders who get there first.
build_gaps <- function(runs, linemen, scheme, ho, cfg = GAP_SETTINGS) {
  edges <- line_edges(linemen)

  # Each side has up to four edge players: center (k0), guard (k1), tackle (k2),
  # end man (k3). Gap A = k0..k1, B = k1..k2, C = k2..k3 (or the edge), D = edge.
  edge_long <- rbind(
    edges[, .(gameId, playId, side, k = 0L, nflId = NA_integer_)],
    edges[, .(gameId, playId, side, k = 1L, nflId = id1)],
    edges[, .(gameId, playId, side, k = 2L, nflId = id2)],
    edges[, .(gameId, playId, side, k = 3L, nflId = id3)]
  )
  ctr_id <- linemen[is_center == TRUE, .(gameId, playId, center_id = nflId)]
  edge_long <- merge(edge_long, ctr_id, by = c("gameId", "playId"))
  edge_long[k == 0L, nflId := center_id][, center_id := NULL]
  edge_long <- edge_long[!is.na(nflId)]
  edge_long <- merge(edge_long, ho[, .(gameId, playId, nflId, ex = x, y, vy)],
                     by = c("gameId", "playId", "nflId"))
  # A pulling lineman has left his spot by the handoff. His gaps stay where he
  # lined up, so use his position at the snap, standing still.
  pullers <- scheme[!is.na(puller_id), .(gameId, playId, nflId = puller_id)]
  pullers <- merge(pullers, linemen[, .(gameId, playId, nflId, snap_y = y)],
                   by = c("gameId", "playId", "nflId"))
  edge_long[pullers, on = .(gameId, playId, nflId), `:=`(y = i.snap_y, vy = 0)]
  edge_long <- order_edges(edge_long)

  inner <- edge_long[k < 3L, .(gameId, playId, side, k_out = k + 1L, in_y = y, in_vy = vy, in_x = ex)]
  outer <- edge_long[k > 0L, .(gameId, playId, side, k_out = k, out_y = y, out_vy = vy, out_x = ex)]
  gaps <- merge(inner, outer, by = c("gameId", "playId", "side", "k_out"), all.x = TRUE)

  # Edge gaps: outside the outermost edge player on each side
  outermost <- edge_long[, .SD[which.max(k)], by = .(gameId, playId, side)]
  edge_gaps <- outermost[, .(gameId, playId, side, k_out = k + 1L, in_y = y, in_vy = vy, in_x = ex)]
  gaps <- rbind(gaps[!is.na(out_y)], edge_gaps, fill = TRUE)
  gaps[, letter := GAP_LETTERS[k_out]]
  gaps[, is_edge := is.na(out_y)]
  gaps[, gap := paste0(side, letter)]
  gaps[, sgn := fifelse(side == "L", 1, -1)]   # outward direction in y

  # Edge gap's outer side: 5 yd outside the end man, or the sideline if nearer.
  # Defenders on the edge count through the unblocked-defender rule, not the width.
  edge_cap <- cfg$edge_cap
  gaps[is_edge == TRUE, `:=`(
    out_y  = fifelse(sgn > 0, pmin(in_y + edge_cap, FIELD_WIDTH), pmax(in_y - edge_cap, 0)),
    out_vy = in_vy
  )]

  # Runner arrival and projected width
  carrier <- ho[role == "carrier", .(gameId, playId, rx = x, ry = y, rvx = vx, rvy = vy)]
  gaps <- merge(gaps, carrier, by = c("gameId", "playId"))
  gaps <- merge(gaps, runs[, .(gameId, playId, los_x)], by = c("gameId", "playId"))

  speed_rule <- cfg$runner_speed
  accel <- cfg$accel
  top_speed <- cfg$top_speed
  time_runner <- function(g) {
    g[, run_dist := sqrt((gx - rx)^2 + (gy - ry)^2)]
    if (speed_rule == "zero") {
      g[, run_v0 := 0]
    } else if (speed_rule == "full") {
      g[, run_v0 := sqrt(rvx^2 + rvy^2)]
    } else {
      g[, run_v0 := (rvx * (gx - rx) + rvy * (gy - ry)) / pmax(run_dist, 1e-6)]
    }
    g[, t_runner := time_to_reach(run_dist, run_v0, accel, top_speed)]
  }

  max_projection <- cfg$max_projection
  spacer <- cfg$spacer
  gaps[, `:=`(gx = los_x, gy = (in_y + out_y) / 2)]   # aim point: middle of the gap at the line
  time_runner(gaps)
  gaps[, t_proj := pmin(t_runner, max_projection)]
  gaps[, width := pmax(abs((out_y + out_vy * t_proj) - (in_y + in_vy * t_proj)) - spacer, 0)]
  # Aim point moves with the projected edges. Time the runner again to that
  # point, the same point the defenders are timed to below.
  gaps[, gy := ((in_y + in_vy * t_proj) + (out_y + out_vy * t_proj)) / 2]
  time_runner(gaps)

  # Unblocked defenders who arrive first
  defs <- ho[role == "defense"]
  blockers <- ho[role %in% c("OL", "TE_line", "offense") & position != "QB",
                 .(gameId, playId, bid = nflId, bx = x, by = y)]
  dg <- merge(gaps[, .(gameId, playId, gap, gx, gy, t_runner)],
              defs[, .(gameId, playId, did = nflId, dx = x, dy = y, dvx = vx, dvy = vy)],
              by = c("gameId", "playId"), allow.cartesian = TRUE)
  dg[, d_dist := sqrt((gx - dx)^2 + (gy - dy)^2)]
  dg[, d_v0 := (dvx * (gx - dx) + dvy * (gy - dy)) / pmax(d_dist, 1e-6)]
  dg[, t_def := time_to_reach(d_dist, d_v0, accel, top_speed)]
  dg <- dg[t_def <= t_runner]

  # Blocked: a blocker within 1.5 yd, closer to the gap than the defender, and on
  # the gap side of him (positive dot product).
  block_radius <- cfg$block_radius
  db <- merge(dg[, .(gameId, playId, gap, did, dx, dy, gx, gy, d_dist)],
              blockers, by = c("gameId", "playId"), allow.cartesian = TRUE)
  db <- db[sqrt((bx - dx)^2 + (by - dy)^2) <= block_radius &
             sqrt((gx - bx)^2 + (gy - by)^2) < d_dist &
             ((bx - dx) * (gx - dx) + (by - dy) * (gy - dy)) > 0]
  blocked <- unique(db[, .(gameId, playId, gap, did)])
  dg[, blocked := FALSE]
  dg[blocked, on = .(gameId, playId, gap, did), blocked := TRUE]

  counts <- dg[, .(n_first = .N, n_unblocked = sum(!blocked)), by = .(gameId, playId, gap)]
  gaps <- merge(gaps, counts, by = c("gameId", "playId", "gap"), all.x = TRUE)
  gaps[is.na(n_first), `:=`(n_first = 0L, n_unblocked = 0L)]

  gaps <- gaps[, .(gameId, playId, gap, side, letter, is_edge, width, t_runner,
                   run_dist, n_first, n_unblocked, gx, gy, in_y, out_y)]
  setorder(gaps, gameId, playId, side, letter)
  gaps[]
}

# ---- Gap score, best gap and the four groups (used by step 5 and the step 8 variants) ----
#
# Gap score = expected run success from a logistic regression of success on the
# taken gap's width and number of unblocked defenders (plan rule: fit only on
# the gaps the runners took). The best gap is the gap with the highest score.
# Yards left on the table = expected success of the best gap minus expected
# success of the gap taken (in success-probability points).

GAP_MODEL <- success ~ width + n_unblocked

score_gaps <- function(runs, scheme, gaps, taken, formula = GAP_MODEL) {
  gaps <- merge(gaps, runs[, .(gameId, playId, success, yards)], by = c("gameId", "playId"))
  gaps <- merge(gaps, taken[, .(gameId, playId, gap_taken)], by = c("gameId", "playId"))
  gaps[, is_taken := gap == gap_taken]

  model <- glm(formula, family = binomial, data = gaps[is_taken == TRUE])
  model$call$formula <- formula
  gaps[, score := predict(model, newdata = gaps, type = "response")]

  # Ties (same width and defenders) go to the gap the runner reaches soonest
  setorder(gaps, gameId, playId, -score, t_runner)
  best <- gaps[, .SD[1], by = .(gameId, playId)][, .(gameId, playId, best_gap = gap, best_score = score)]
  taken_score <- gaps[is_taken == TRUE, .(gameId, playId, taken_score = score)]

  cmp <- runs[, .(gameId, playId, week, ballCarrierId, ballCarrierDisplayName, carrier_pos,
                  success, yards, short_yardage)]
  cmp <- merge(cmp, scheme[, .(gameId, playId, scheme, design_side, design_gap)],
               by = c("gameId", "playId"))
  cmp <- merge(cmp, taken[, .(gameId, playId, gap_taken, taken_source)], by = c("gameId", "playId"))
  cmp <- merge(cmp, best, by = c("gameId", "playId"))
  cmp <- merge(cmp, taken_score, by = c("gameId", "playId"), all.x = TRUE)

  cmp[, took_best := gap_taken == best_gap]
  cmp[, left_on_table := best_score - taken_score]

  # The four groups (runs with a designed gap)
  cmp[!is.na(design_gap), group := fcase(
    design_gap == best_gap & gap_taken == best_gap,   "Worked as designed",
    design_gap != best_gap & gap_taken == best_gap,   "Good read",
    design_gap == best_gap & gap_taken != best_gap,   "Missed the open gap",
    design_gap != best_gap & gap_taken == design_gap, "Followed the call into a closed gap",
    default = "Took a third gap"
  )]
  list(model = model, gaps = gaps, cmp = cmp)
}

# ---- Play pictures: a still at the handoff and an animation ------------------------

# Colors: reference data-viz palette (categorical slots 1 and 2, blue ramp)
COL_OFFENSE <- "#2a78d6"
COL_DEFENSE <- "#eb6834"
COL_RUNNER  <- "#0b0b0b"
COL_INK     <- "#52514e"
COL_FIELD   <- "#fcfcfb"
COL_GRID    <- "#e4e3df"
SCORE_RAMP  <- c("#cde2fb", "#6da7ec", "#256abf", "#104281")

SCHEME_LABELS <- c(gap = "gap run (pulling lineman)", inside_zone = "inside zone",
                   outside_zone = "outside zone", other = "man / duo / other")

load_play <- function(game_id, play_id) {
  cmp  <- readRDS(file.path(CACHE_DIR, "compare.rds"))[gameId == game_id & playId == play_id]
  runs <- readRDS(file.path(CACHE_DIR, "runs.rds"))[gameId == game_id & playId == play_id]
  gaps <- readRDS(file.path(CACHE_DIR, "gaps_scored.rds"))[gameId == game_id & playId == play_id]
  trk  <- readRDS(file.path(CACHE_DIR, sprintf("tracking_prep_wk%d.rds", runs$week)))
  trk  <- trk[gameId == game_id & playId == play_id]
  list(cmp = cmp, run = runs, gaps = gaps, trk = trk)
}

field_theme <- function() {
  theme_void(base_size = 11) +
    theme(plot.background = element_rect(fill = COL_FIELD, colour = NA),
          plot.title = element_text(colour = "#0b0b0b", face = "bold", margin = margin(b = 4)),
          plot.subtitle = element_text(colour = COL_INK, margin = margin(b = 8)),
          plot.caption = element_text(colour = COL_INK, hjust = 0),
          legend.position = "bottom",
          legend.title = element_text(colour = COL_INK, size = 9),
          legend.text = element_text(colour = COL_INK, size = 8),
          plot.margin = margin(12, 12, 12, 12))
}

# Still picture at the handoff: gaps shaded by score, designed and best gap
# marked, and the runner's path from the handoff to the end of the play.
plot_play_handoff <- function(game_id, play_id) {
  p <- load_play(game_id, play_id)
  run <- p$run; cmp <- p$cmp; g <- p$gaps; trk <- p$trk
  los <- run$los_x
  at <- trk[frameId == run$handoff_frame & role != "football"]
  # Runner's path from the handoff to 1.5 s after he reaches the line
  path_end <- fcoalesce(run$cross_frame, run$contact_frame, run$end_frame) + 15L
  path <- trk[role == "carrier" & frameId >= run$handoff_frame & frameId <= path_end]

  g[, `:=`(ymin = pmin(in_y, out_y), ymax = pmax(in_y, out_y))]

  xlim <- c(los - 10, max(los + 12, max(path$x) + 2))
  near <- at[x >= xlim[1] & x <= xlim[2]]
  ylim <- range(c(g$ymin, g$ymax, path$y, near$y)) + c(-1.5, 1.5)
  ylim <- c(max(ylim[1], 0), min(ylim[2], FIELD_WIDTH))

  # Edge gaps are open-ended: draw them out to the edge of the picture
  g[is_edge == TRUE & side == "L", ymax := ylim[2]]
  g[is_edge == TRUE & side == "R", ymin := ylim[1]]
  # Label gaps tall enough to hold text; dark fills get light text
  g[, label := fifelse(ymax - ymin >= 0.7, gap, "")]
  g[, dark := score >= median(score)]
  design <- g[gap == cmp$design_gap]
  best <- g[gap == cmp$best_gap]

  at[, side := fifelse(role == "defense", "Defense", fifelse(role == "carrier", "Runner", "Offense"))]

  ggplot() +
    geom_vline(xintercept = seq(10, 110, 5), colour = COL_GRID, linewidth = 0.4) +
    geom_vline(xintercept = los, colour = COL_INK, linewidth = 0.6, linetype = "dashed") +
    geom_rect(data = g, aes(xmin = los - 0.8, xmax = los + 0.8, ymin = ymin, ymax = ymax,
                            fill = score), colour = COL_FIELD, linewidth = 0.6) +
    geom_text(data = g[dark == TRUE], aes(x = los, y = (ymin + ymax) / 2, label = label),
              colour = "#ffffff", size = 2.6, fontface = "bold") +
    geom_text(data = g[dark == FALSE], aes(x = los, y = (ymin + ymax) / 2, label = label),
              colour = "#0b0b0b", size = 2.6, fontface = "bold") +
    { if (nrow(design) > 0)
        geom_rect(data = design, aes(xmin = los - 1, xmax = los + 1, ymin = ymin, ymax = ymax),
                  fill = NA, colour = "#0b0b0b", linewidth = 0.7, linetype = "22") } +
    geom_rect(data = best, aes(xmin = los - 1.1, xmax = los + 1.1, ymin = ymin, ymax = ymax),
              fill = NA, colour = "#0b0b0b", linewidth = 1.1) +
    geom_path(data = path, aes(x, y), colour = COL_RUNNER, linewidth = 0.7) +
    geom_point(data = at, aes(x, y, colour = side), size = 3.2, stroke = 0.6) +
    scale_colour_manual(values = c(Offense = COL_OFFENSE, Defense = COL_DEFENSE, Runner = COL_RUNNER),
                        name = NULL) +
    scale_fill_gradientn(colours = SCORE_RAMP, name = "Gap score (expected success)",
                         breaks = scales::breaks_pretty(3),
                         labels = scales::label_percent(accuracy = 1)) +
    annotate("text", x = los + 1.5, y = best$ymax[1] + 0.4, label = "Best gap",
             hjust = 0, size = 3.2, colour = "#0b0b0b", fontface = "bold") +
    { if (nrow(design) > 0 && cmp$design_gap != cmp$best_gap)
        annotate("text", x = los + 1.5, y = design$ymin[1] - 0.4, label = "Designed gap",
                 hjust = 0, size = 3.2, colour = "#0b0b0b") } +
    coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = sprintf("%s, %s", cmp$ballCarrierDisplayName, SCHEME_LABELS[cmp$scheme]),
         subtitle = sprintf("Designed %s, best %s, taken %s. Result: %d yard%s. Offense moves left to right.",
                            fifelse(is.na(cmp$design_gap), "none", cmp$design_gap),
                            cmp$best_gap, cmp$gap_taken, run$yards,
                            fifelse(abs(run$yards) == 1, "", "s")),
         caption = sprintf("Game %s, play %s. Field at the handoff; line is the runner's path after it.",
                           game_id, play_id)) +
    field_theme()
}

# Animation from the snap to the end of the play, for checking scheme labels.
animate_play <- function(game_id, play_id, file, fps = 10) {
  p <- load_play(game_id, play_id)
  run <- p$run; cmp <- p$cmp; trk <- copy(p$trk)
  last <- fcoalesce(run$cross_frame, run$contact_frame, run$end_frame) + 15L
  trk <- trk[frameId >= run$snap_frame - 5 & frameId <= last]
  trk[, side := fcase(role == "football", "Ball", role == "defense", "Defense",
                      role == "carrier", "Runner", role %in% c("OL", "TE_line"), "Line",
                      default = "Offense")]
  los <- run$los_x
  anim <- ggplot(trk, aes(x, y, colour = side, group = nflId)) +
    geom_vline(xintercept = los, colour = COL_INK, linetype = "dashed") +
    geom_point(aes(size = side == "Ball")) +
    scale_size_manual(values = c(`FALSE` = 3, `TRUE` = 1.5), guide = "none") +
    scale_colour_manual(values = c(Line = "#104281", Offense = COL_OFFENSE, Defense = COL_DEFENSE,
                                   Runner = COL_RUNNER, Ball = "#8a5a00"), name = NULL) +
    coord_fixed(xlim = c(los - 12, los + 18), ylim = range(trk$y) + c(-1, 1)) +
    labs(title = sprintf("%s: %s, designed %s, taken %s",
                         cmp$ballCarrierDisplayName, SCHEME_LABELS[cmp$scheme],
                         fifelse(is.na(cmp$design_gap), "none", cmp$design_gap), cmp$gap_taken),
         subtitle = "{round(frame_time, 1)} s after the snap") +
    field_theme() +
    gganimate::transition_time(t)
  n <- uniqueN(trk$frameId)
  # MP4 through the av package is fast; GIF through gifski if the file name asks for it
  renderer <- if (grepl("\\.gif$", file)) gganimate::gifski_renderer() else gganimate::av_renderer()
  gganimate::anim_save(file, gganimate::animate(anim, nframes = n, fps = fps, width = 640,
                                                height = 400, renderer = renderer))
}

# ==== STEP 0: CHECK THE DATA AND KEEP THE CLEAN DESIGNED RUNS ======================
#
# Output:
#   cache/plays_runs.rds        one row per clean designed run (plays.csv columns)
#   cache/players.rds           players.csv
#   cache/games.rds             games.csv
#   cache/tracking_runs_wkN.rds tracking rows for those runs, one file per week

message("\n==== Step 0: data check ====")

required_files <- c(
  file.path(DATA_DIR, c("games.csv", "plays.csv", "players.csv")),
  vapply(WEEKS, tracking_file, character(1))
)
not_found <- required_files[!file.exists(required_files)]
if (length(not_found) > 0) {
  stop("These data files were not found. Check DEFAULT_DATA_DIR at the top of this file or set ",
       "NFL_DATA_DIR:\n  ", paste(not_found, collapse = "\n  "))
}
message("Data folder: ", DATA_DIR)

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

# Tracking: keep only the run plays. Delete cache/ to rebuild these files.
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
rm(plays, games); invisible(gc())

# ==== STEP 1: PREPARE THE PLAYS (plan 3.1) =========================================
#
# Flip plays, find the key frames, the line of scrimmage, the offensive line,
# the tight ends on the line and the point where the runner crosses the line.
#
# Output:
#   cache/runs.rds              one row per run, with flags and key frames
#   cache/linemen.rds           one row per OL / on-line TE per run, at the snap
#   cache/tracking_prep_wkN.rds flipped tracking with a `role` column

message("\n==== Step 1: prepare ====")

runs    <- readRDS(file.path(CACHE_DIR, "plays_runs.rds"))
players <- readRDS(file.path(CACHE_DIR, "players.rds"))

# ---- Play-level flags ---------------------------------------------------------------

# Yards the run itself gained. playResult also counts penalty yards (a holding
# call after a 6-yard run shows as -4), which say nothing about the runner's gap.
runs[, yards := prePenaltyPlayResult]
runs[, `:=`(
  success       = run_success(down, yardsToGo, yards),
  short_yardage = down %in% 3:4 & yardsToGo <= SHORT_YARDAGE_TO,
  inside_opp5   = yardlineSide == defensiveTeam & yardlineNumber <= GOAL_LINE_CUTOFF
)]
runs <- merge(runs, players[, .(ballCarrierId = nflId, carrier_pos = position)],
              by = "ballCarrierId", all.x = TRUE)

# Side and gap named in the play-by-play text, for validation (plan step 3.6).
runs[, pbp_dir := regmatches(playDescription,
                             regexpr("(left|right) (end|tackle|guard)|up the middle",
                                     playDescription))[1], by = .(gameId, playId)]
runs[, pbp_side := fifelse(grepl("^left", pbp_dir), "left",
                    fifelse(grepl("^right", pbp_dir), "right",
                    fifelse(pbp_dir == "up the middle", "middle", NA_character_)))]

SNAP_EVENTS <- c("ball_snap", "snap_direct", "autoevent_ballsnap")

# Pitches and shovels are sometimes tagged as a lateral or a forward pass
PITCH_EVENTS <- c("lateral", "pass_forward", "autoevent_passforward")

first_frame <- function(f) if (length(f) > 0) as.integer(min(f)) else NA_integer_

# ---- Per-week processing ------------------------------------------------------------

play_info <- list()
line_info <- list()

for (wk in WEEKS) {
  trk <- readRDS(file.path(CACHE_DIR, sprintf("tracking_runs_wk%d.rds", wk)))
  trk <- flip_tracking(trk)

  # Key frames from the event column
  ev <- unique(trk[!is.na(event), .(gameId, playId, frameId, event)])
  frames <- ev[, .(
    snap_frame    = first_frame(frameId[event %in% SNAP_EVENTS]),
    handoff_frame = first_frame(frameId[event == "handoff"]),
    pitch_frame   = first_frame(frameId[event %in% PITCH_EVENTS]),
    contact_frame = first_frame(frameId[event == "first_contact"])
  ), by = .(gameId, playId)]
  last <- trk[, .(end_frame = max(frameId)), by = .(gameId, playId)]
  frames <- merge(last, frames, by = c("gameId", "playId"), all.x = TRUE)

  # Line of scrimmage = football x at the snap
  ball <- trk[club == "football"][frames, on = .(gameId, playId, frameId = snap_frame),
                                  nomatch = 0L, .(gameId, playId, los_x = x, ball_y = y)]
  frames <- merge(frames, ball, by = c("gameId", "playId"), all.x = TRUE)

  # Player roles
  wk_runs <- runs[week == wk, .(gameId, playId, possessionTeam, ballCarrierId)]
  trk <- merge(trk, wk_runs, by = c("gameId", "playId"))
  trk <- merge(trk, players[, .(nflId, position)], by = "nflId", all.x = TRUE)
  trk[, role := fifelse(club == "football", "football",
                fifelse(club != possessionTeam, "defense",
                fifelse(nflId == ballCarrierId, "carrier",
                fifelse(position %in% OL_POSITIONS, "OL", "offense"))))]

  # Positions at the snap for the offense
  at_snap <- trk[frames, on = .(gameId, playId, frameId = snap_frame), nomatch = 0L]
  at_snap <- at_snap[role %in% c("OL", "offense", "carrier")]

  ol <- at_snap[role == "OL"]
  ol_box <- ol[, .(ol_x = mean(x), ol_ymin = min(y), ol_ymax = max(y), n_ol = .N),
               by = .(gameId, playId)]

  # A tight end is on the line if he is level with the linemen (within 1 yard
  # of their mean depth) and within 3 yards of the end lineman. A TE who is the
  # ball carrier keeps the carrier role.
  te <- at_snap[position == "TE" & role != "carrier"][ol_box, on = .(gameId, playId), nomatch = 0L]
  te <- te[abs(x - ol_x) <= 1 & y >= ol_ymin - 3 & y <= ol_ymax + 3 &
             (y < ol_ymin | y > ol_ymax)]
  trk[te, on = .(gameId, playId, nflId), role := "TE_line"]

  line <- rbind(ol[, .(gameId, playId, nflId, position, role = "OL", x, y)],
                te[, .(gameId, playId, nflId, position, role = "TE_line", x, y)])
  # Slot 1 is the right-most player (smallest y; +y is the offense's left).
  setorder(line, gameId, playId, y)
  line[, slot := seq_len(.N), by = .(gameId, playId)]

  # Center: listed C, else the lineman closest to the ball
  line <- merge(line, frames[, .(gameId, playId, ball_y)], by = c("gameId", "playId"))
  line[, is_center := {
    c_idx <- which(position == "C" & role == "OL")
    if (length(c_idx) == 0) c_idx <- which(role == "OL")[which.min(abs(y - ball_y)[role == "OL"])]
    seq_len(.N) == c_idx[which.min(abs(y[c_idx] - ball_y[1]))]
  }, by = .(gameId, playId)]
  line[, ball_y := NULL]

  # Handoff frame for non-QB runs with no `handoff` event: use the pitch event,
  # else the first frame after the snap with the ball within 1 yard of the
  # runner. Designed QB runs keep no handoff and stay out of the main analysis.
  ball_path <- trk[role == "football", .(gameId, playId, frameId, bx = x, by = y)]
  reach <- trk[role == "carrier" & position != "QB", .(gameId, playId, frameId, x, y)]
  reach <- merge(reach, ball_path, by = c("gameId", "playId", "frameId"))
  reach <- merge(reach, frames[is.na(handoff_frame), .(gameId, playId, snap_frame)],
                 by = c("gameId", "playId"))
  reach <- reach[frameId > snap_frame & sqrt((x - bx)^2 + (y - by)^2) < 1,
                 .(reach_frame = min(frameId)), by = .(gameId, playId)]
  frames <- merge(frames, reach, by = c("gameId", "playId"), all.x = TRUE)
  non_qb <- runs[week == wk & carrier_pos != "QB", .(gameId, playId)]
  frames[, handoff_source := fifelse(!is.na(handoff_frame), "event", NA_character_)]
  frames[non_qb, on = .(gameId, playId), `:=`(
    handoff_source = fcase(!is.na(handoff_frame), "event",
                           !is.na(pitch_frame), "pitch_event",
                           !is.na(reach_frame), "ball_reaches_runner",
                           default = NA_character_),
    handoff_frame = fcoalesce(handoff_frame, pitch_frame, reach_frame)
  )]
  frames[, c("pitch_frame", "reach_frame") := NULL]

  # Where the runner crosses the line of scrimmage: first frame at or after
  # the handoff (or the snap, if there is no handoff) with x >= LOS.
  carrier <- trk[role == "carrier", .(gameId, playId, frameId, x, y)]
  carrier <- merge(carrier, frames[, .(gameId, playId, los_x,
                                       start = fcoalesce(handoff_frame, snap_frame))],
                   by = c("gameId", "playId"))
  cross <- carrier[frameId >= start & x >= los_x,
                   .SD[which.min(frameId)], by = .(gameId, playId)]
  frames <- merge(frames, cross[, .(gameId, playId, cross_frame = frameId, cross_y = y)],
                  by = c("gameId", "playId"), all.x = TRUE)

  frames <- merge(frames, ol_box[, .(gameId, playId, n_ol)], by = c("gameId", "playId"),
                  all.x = TRUE)
  frames <- merge(frames, line[, .(n_te_line = sum(role == "TE_line")), by = .(gameId, playId)],
                  by = c("gameId", "playId"), all.x = TRUE)

  # Frames relative to the snap, in seconds
  trk <- merge(trk, frames[, .(gameId, playId, snap_frame)], by = c("gameId", "playId"))
  trk[, t := (frameId - snap_frame) / FPS]
  trk[, c("possessionTeam", "ballCarrierId", "snap_frame", "playDirection") := NULL]
  setkey(trk, gameId, playId, frameId, nflId)

  saveRDS(trk, file.path(CACHE_DIR, sprintf("tracking_prep_wk%d.rds", wk)))
  play_info[[wk]] <- frames
  line_info[[wk]] <- line
  message(sprintf("Week %d: %d runs prepared", wk, nrow(frames)))
  rm(trk); invisible(gc())
}

frames  <- rbindlist(play_info)
linemen <- rbindlist(line_info)
runs <- merge(runs, frames, by = c("gameId", "playId"), all.x = TRUE)

# ---- Runner side and the check against the play-by-play ------------------------------

# After the flip, +y is the offense's left.
runs[, side_track := fifelse(cross_y > ball_y, "left", "right")]
runs[, pbp_match := fifelse(pbp_side %in% c("left", "right"), side_track == pbp_side, NA)]

# Runs used in the main analysis: a snap, a handoff, not inside the opponent's 5
runs[, has_handoff := !is.na(handoff_frame)]
runs[, in_analysis := !inside_opp5 & !is.na(snap_frame) & has_handoff]

saveRDS(runs, file.path(CACHE_DIR, "runs.rds"))
saveRDS(linemen, file.path(CACHE_DIR, "linemen.rds"))

# ---- Summary --------------------------------------------------------------------------

cat("\nRuns:", nrow(runs), "\n")
cat("  inside opponent's 5:", sum(runs$inside_opp5), "\n")
cat("  short yardage (flagged, kept):", sum(runs$short_yardage), "\n")
cat("  no handoff (mostly QB runs):", sum(!runs$has_handoff), "\n")
cat("  handoff source:\n"); print(table(runs$handoff_source, useNA = "ifany"))
cat("  in main analysis:", sum(runs$in_analysis), "\n")
cat("  runner never crossed the LOS:", sum(runs$in_analysis & is.na(runs$cross_frame)), "\n")
cat("\nOffensive linemen per run:\n"); print(table(runs$n_ol, useNA = "ifany"))
cat("\nTight ends on the line per run:\n"); print(table(runs$n_te_line, useNA = "ifany"))
cat("\nSnap to handoff (s), median:",
    median((runs$handoff_frame - runs$snap_frame) / FPS, na.rm = TRUE), "\n")
cat(sprintf("\nSide vs play-by-play agreement: %.1f%% of %d runs\n",
            100 * mean(runs$pbp_match, na.rm = TRUE), sum(!is.na(runs$pbp_match))))
print(runs[in_analysis == TRUE & !is.na(pbp_match),
           .(agree = round(mean(pbp_match), 3), n = .N), by = week][order(week)])

# ==== STEP 2: BLOCKING SCHEME AND DESIGNED GAP (plan 3.2) ==========================
#
# Label each run's blocking scheme from how the offensive line moves, and set
# the designed gap.
#
# Lineman lateral movement is measured from the snap to the end of the scheme
# window: min(snap + 1.2 s, handoff + 0.2 s).
#   Gap (pull):  a guard or tackle moves 2.5+ yd sideways AND crosses the
#                center's starting y, or moves against the median of the line.
#                Designed side = the puller's direction.
#   Zone:        4+ linemen move 1.0+ yd the same way. Designed side = that way.
#                Inside vs outside: split on the line's mean movement toward the
#                play side, with the cut point set by 2-means on week 1.
#   Other:       everything else (man, duo, ...). No designed gap.
# Designed gap: gap play = the gap where the puller turns upfield; inside zone
# = B gap (outside leg of the play-side guard); outside zone = C gap (outside
# the play-side tackle).
#
# Output: cache/scheme.rds (one row per run in the analysis)

message("\n==== Step 2: scheme ====")

WINDOW_SNAP    <- 1.2   # s after the snap
WINDOW_HANDOFF <- 0.2   # s after the handoff
PULL_YARDS     <- 2.5
ZONE_YARDS     <- 1.0
ZONE_MIN_OL    <- 4
TURN_WINDOW    <- 1.0   # s after the handoff to look for the puller's turn upfield

runs    <- readRDS(file.path(CACHE_DIR, "runs.rds"))[in_analysis == TRUE]
linemen <- readRDS(file.path(CACHE_DIR, "linemen.rds"))
edges   <- line_edges(linemen)

runs[, win_end := pmin(snap_frame + WINDOW_SNAP * FPS, handoff_frame + WINDOW_HANDOFF * FPS)]

# ---- Lineman movement and puller paths ------------------------------------------------

move_list <- list()
path_list <- list()
for (wk in WEEKS) {
  trk <- readRDS(file.path(CACHE_DIR, sprintf("tracking_prep_wk%d.rds", wk)))
  r <- runs[week == wk, .(gameId, playId, snap_frame, handoff_frame, win_end)]
  ol <- merge(trk[role == "OL", .(gameId, playId, nflId, frameId, x, y)], r,
              by = c("gameId", "playId"))
  a <- ol[frameId == snap_frame, .(gameId, playId, nflId, y0 = y)]
  b <- ol[frameId == win_end, .(gameId, playId, nflId, y1 = y)]
  move_list[[wk]] <- merge(a, b, by = c("gameId", "playId", "nflId"))
  # Keep guard/tackle paths up to 1 s after the handoff to find turn-up points
  path_list[[wk]] <- ol[frameId >= snap_frame & frameId <= handoff_frame + TURN_WINDOW * FPS,
                        .(gameId, playId, nflId, frameId, x, y)]
  rm(trk); invisible(gc())
}
move <- rbindlist(move_list)
paths <- rbindlist(path_list)

move <- merge(move, linemen[, .(gameId, playId, nflId, position, is_center)],
              by = c("gameId", "playId", "nflId"))
move[, dy := y1 - y0]   # + = toward the offense's left
center0 <- move[is_center == TRUE, .(gameId, playId, cy0 = y0)]
move <- merge(move, center0, by = c("gameId", "playId"))

# Median movement of the other linemen, for the "against the line" rule
move[, med_others := vapply(seq_len(.N), function(i) median(dy[-i]), numeric(1)),
     by = .(gameId, playId)]

# ---- Gap (pull) rule ---------------------------------------------------------------------

move[, crosses_center := sign(y0 - cy0) != sign(y1 - cy0)]
move[, against_line := sign(dy) != sign(med_others) & abs(med_others) > 0]
pullers <- move[position %in% c("G", "T") & abs(dy) >= PULL_YARDS &
                  (crosses_center | against_line)]
# One puller per run: the biggest lateral mover
pullers <- pullers[order(-abs(dy)), .SD[1], by = .(gameId, playId)]
pullers[, side := fifelse(dy > 0, "L", "R")]

# ---- Zone rule ----------------------------------------------------------------------------

zone <- move[, .(n_left  = sum(dy >=  ZONE_YARDS),
                 n_right = sum(dy <= -ZONE_YARDS),
                 mean_dy = mean(dy)), by = .(gameId, playId)]
zone[, side := fcase(n_left >= ZONE_MIN_OL & n_left >= n_right, "L",
                     n_right >= ZONE_MIN_OL, "R",
                     default = NA_character_)]
zone[, flow := fifelse(side == "L", mean_dy, -mean_dy)]   # movement toward the play side

# ---- Combine -------------------------------------------------------------------------------

scheme <- runs[, .(gameId, playId, week)]
scheme[pullers, on = .(gameId, playId), `:=`(scheme = "gap", design_side = i.side,
                                             puller_id = i.nflId, puller_dy = i.dy)]
scheme[zone[!is.na(side)], on = .(gameId, playId),
       `:=`(zone_side = i.side, zone_flow = i.flow)]
scheme[is.na(scheme) & !is.na(zone_side), `:=`(scheme = "zone", design_side = zone_side)]
scheme[is.na(scheme), scheme := "other"]

# Inside vs outside zone: 2-means on week 1 zone runs' flow, cut at the midpoint
set.seed(355)
km <- kmeans(scheme[week == 1 & scheme == "zone", zone_flow], centers = 2, nstart = 20)
ZONE_SPLIT <- mean(km$centers)
scheme[scheme == "zone", scheme := fifelse(zone_flow >= ZONE_SPLIT, "outside_zone", "inside_zone")]
message(sprintf("Inside/outside zone split: %.2f yd of line flow (week 1 centers %.2f and %.2f)",
                ZONE_SPLIT, min(km$centers), max(km$centers)))

# ---- Designed gap ----------------------------------------------------------------------------

# Puller's turn-up point: the frame of his biggest lateral displacement
pp <- merge(paths, pullers[, .(gameId, playId, nflId, side)], by = c("gameId", "playId", "nflId"))
pp <- merge(pp, move[, .(gameId, playId, nflId, y0)], by = c("gameId", "playId", "nflId"))
turn <- pp[, .SD[which.max(abs(y - y0))], by = .(gameId, playId)][, .(gameId, playId, y, side)]
# The gap is on the puller's side, even if he has not yet passed the center
turn <- merge(turn, unique(edges[, .(gameId, playId, center_y)]), by = c("gameId", "playId"))
turn[, y := fifelse(side == "L", pmax(y, center_y + 0.01), pmin(y, center_y - 0.01))]
turn <- gap_of_point(turn[, .(gameId, playId, y)], edges)

scheme[turn, on = .(gameId, playId), design_gap := i.gap]
scheme[scheme == "inside_zone",  design_gap := paste0(design_side, "B")]
scheme[scheme == "outside_zone", design_gap := paste0(design_side, "C")]

saveRDS(scheme, file.path(CACHE_DIR, "scheme.rds"))

# ---- Summary and comparison with the week 1 prototype ------------------------------------------

chk <- merge(scheme, runs[, .(gameId, playId, side_track, pbp_side)], by = c("gameId", "playId"))
chk[, design_lr := fifelse(design_side == "L", "left", "right")]
summ <- function(d) {
  d[, .(share = round(.N / nrow(d), 3),
        n = .N,
        runner_designed_side = round(mean(side_track == design_lr, na.rm = TRUE), 3),
        pbp_designed_side = round(mean((pbp_side == design_lr)[pbp_side %in% c("left", "right")],
                                       na.rm = TRUE), 3)),
    by = scheme][order(scheme)]
}
cat("\nWeek 1 (prototype: gap 33%/92%/93%, outside zone 15%/86%/82%,",
    "inside zone 15%/66%/58%, other 38%)\n")
print(summ(chk[week == 1]))
cat("\nWeeks 1 to 9\n")
print(summ(chk))
cat("\nDesigned gap by scheme\n")
print(dcast(scheme[!is.na(design_gap)], scheme ~ substr(design_gap, 2, 2), fun.aggregate = length))

# ==== STEP 3: EVERY GAP AT THE HANDOFF (plan 3.3, parts 1 to 4) ====================
#
# At the handoff, describe every gap on every run: its projected width when the
# runner gets there and the number of unblocked defenders who get there first.
# The rules are in build_gaps() under HELPERS, with the settings in GAP_SETTINGS.
#
# Output: cache/gaps.rds (one row per run and gap)

message("\n==== Step 3: gaps ====")

runs    <- readRDS(file.path(CACHE_DIR, "runs.rds"))[in_analysis == TRUE]
linemen <- readRDS(file.path(CACHE_DIR, "linemen.rds"))
scheme  <- readRDS(file.path(CACHE_DIR, "scheme.rds"))

ho   <- handoff_snapshot(runs)
gaps <- build_gaps(runs, linemen, scheme, ho)
saveRDS(gaps, file.path(CACHE_DIR, "gaps.rds"))

cat("\nGaps per run:\n"); print(table(gaps[, .N, by = .(gameId, playId)]$N))
cat("\nBy gap letter:\n")
print(gaps[, .(n = .N, width = round(median(width), 2), t_runner = round(median(t_runner), 2),
               unblocked = round(mean(n_unblocked), 2), share_open = round(mean(n_unblocked == 0), 2)),
           by = .(letter, is_edge)][order(letter, is_edge)])

# ==== STEP 4: GAP TAKEN (plan 3.4) =================================================
#
# The gap taken is the gap where the runner crosses the line of scrimmage.
# Gaps are drawn from where the linemen stand at the handoff, the same frame
# used for the best gap. (By the time the runner crosses, often 2 to 3 s after
# the snap, a zone line has flowed several yards and no longer marks the gaps.)
# Runs stopped behind the line use the runner's position at first contact (or
# at the end of the play).
#
# Output: cache/gap_taken.rds (one row per run)

message("\n==== Step 4: gap taken ====")

runs    <- readRDS(file.path(CACHE_DIR, "runs.rds"))[in_analysis == TRUE]
linemen <- readRDS(file.path(CACHE_DIR, "linemen.rds"))
edges   <- line_edges(linemen)
ctr_id  <- linemen[is_center == TRUE, .(gameId, playId, center_id = nflId)]

runs[, `:=`(
  taken_frame  = fcoalesce(cross_frame, contact_frame, end_frame),
  taken_source = fcase(!is.na(cross_frame), "crossed_los",
                       !is.na(contact_frame), "first_contact",
                       default = "end_of_play")
)]

# Edge players per side, long format (center k0, guard k1, tackle k2, end man k3)
edge_ids <- rbind(
  merge(edges[, .(gameId, playId, side, k = 0L)], ctr_id, by = c("gameId", "playId"))[
    , .(gameId, playId, side, k, nflId = center_id)],
  edges[, .(gameId, playId, side, k = 1L, nflId = id1)],
  edges[, .(gameId, playId, side, k = 2L, nflId = id2)],
  edges[, .(gameId, playId, side, k = 3L, nflId = id3)]
)[!is.na(nflId)]

# A pulling lineman has left his spot by the handoff; his gaps stay where he lined up
pullers <- readRDS(file.path(CACHE_DIR, "scheme.rds"))[!is.na(puller_id), .(gameId, playId, nflId = puller_id)]
pullers <- merge(pullers, linemen[, .(gameId, playId, nflId, snap_y = y)], by = c("gameId", "playId", "nflId"))

out <- list()
for (wk in WEEKS) {
  trk <- readRDS(file.path(CACHE_DIR, sprintf("tracking_prep_wk%d.rds", wk)))
  r  <- runs[week == wk, .(gameId, playId, frameId = taken_frame)]
  rh <- runs[week == wk, .(gameId, playId, frameId = handoff_frame)]
  runner <- trk[r, on = .(gameId, playId, frameId), nomatch = 0L][role == "carrier",
                                                                 .(gameId, playId, ry = y)]
  at <- trk[rh, on = .(gameId, playId, frameId), nomatch = 0L, .(gameId, playId, nflId, role, y)]

  e <- merge(edge_ids, at[, .(gameId, playId, nflId, y)], by = c("gameId", "playId", "nflId"))
  e[pullers, on = .(gameId, playId, nflId), y := i.snap_y]   # pullers keep their snap spot
  e <- order_edges(e)                                         # same edges as step 3
  center <- unique(e[k == 0L, .(gameId, playId, cy = y)])
  e <- merge(e[k > 0L], center, by = c("gameId", "playId"))
  e[, u := abs(y - cy)]
  d <- dcast(e, gameId + playId + side ~ k, value.var = "u")
  setnames(d, c("1", "2", "3"), c("d1", "d2", "d3"), skip_absent = TRUE)
  for (col in c("d1", "d2", "d3")) if (!col %in% names(d)) d[, (col) := NA_real_]

  p <- merge(runner, center, by = c("gameId", "playId"))
  p[, side := fifelse(ry > cy, "L", "R")]
  p <- merge(p, d, by = c("gameId", "playId", "side"), all.x = TRUE)
  p[is.na(d1), d1 := DEFAULT_SPLIT]
  p[is.na(d2), d2 := d1 + DEFAULT_SPLIT]
  p[, gap_taken := paste0(side, gap_letter(abs(ry - cy), d1, d2, d3))]
  out[[wk]] <- p[, .(gameId, playId, gap_taken, taken_lateral = ry - cy)]
  rm(trk); invisible(gc())
}
taken <- rbindlist(out)
taken <- merge(taken, runs[, .(gameId, playId, taken_frame, taken_source, pbp_dir)],
               by = c("gameId", "playId"))
saveRDS(taken, file.path(CACHE_DIR, "gap_taken.rds"))

cat("\nRuns with a gap taken:", nrow(taken), "of", nrow(runs), "\n")
print(table(taken$taken_source))
cat("\nGap letter taken by play-by-play direction (rows):\n")
taken[, pbp_part := sub("^(left|right) ", "", pbp_dir)]
print(dcast(taken[!is.na(pbp_part)], pbp_part ~ substr(gap_taken, 2, 2), fun.aggregate = length))

# ==== STEP 5: SCORE THE GAPS AND COMPARE (plan 3.3 parts 5 to 7, and 3.5) ==========
#
# Score every gap, pick the best gap, and compare designed, best and taken.
# The rules are in score_gaps() under HELPERS.
#
# Output:
#   cache/compare.rds              one row per run
#   output/gap_model.csv           regression coefficients
#   output/comparison_groups.csv   the four designed/best/taken groups
#   output/runner_read_rates.csv   runners with 100+ carries, ranked

message("\n==== Step 5: score and compare ====")

MIN_CARRIES <- 100

runs   <- readRDS(file.path(CACHE_DIR, "runs.rds"))[in_analysis == TRUE]
scheme <- readRDS(file.path(CACHE_DIR, "scheme.rds"))
gaps   <- readRDS(file.path(CACHE_DIR, "gaps.rds"))
taken  <- readRDS(file.path(CACHE_DIR, "gap_taken.rds"))

scored <- score_gaps(runs, scheme, gaps, taken)
model  <- scored$model
gaps   <- scored$gaps
cmp    <- scored$cmp

# ---- Gap score model ------------------------------------------------------------------

print(summary(model))

auc <- function(p, y) {
  r <- rank(p)
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
fit_auc <- auc(fitted(model), model$y)
cat(sprintf("\nModel fit on %d taken gaps, AUC = %.3f\n", length(model$y), fit_auc))

coefs <- data.table(term = names(coef(model)), estimate = coef(model),
                    std_error = sqrt(diag(vcov(model))))
coefs[, p_value := 2 * pnorm(-abs(estimate / std_error))]
coefs <- rbind(coefs, data.table(term = "AUC", estimate = fit_auc), fill = TRUE)
fwrite(coefs, file.path(OUT_DIR, "gap_model.csv"))

# ---- The four groups (runs with a designed gap) ------------------------------------------

groups <- cmp[!is.na(group), .(runs = .N, share = round(.N / sum(!is.na(cmp$group)), 3),
                                success = round(mean(success), 3),
                                mean_yards = round(mean(yards), 2)), by = group][order(-runs)]
cat("\nDesigned / best / taken groups (gap and zone runs):\n"); print(groups)
fwrite(groups, file.path(OUT_DIR, "comparison_groups.csv"))

# ---- Does taking the best gap pay off? -----------------------------------------------------

cat("\nOutcome by whether the runner took the best gap (all analysis runs):\n")
print(cmp[!is.na(took_best), .(runs = .N, success = round(mean(success), 3),
                               mean_yards = round(mean(yards), 2)), by = took_best])
cat("\nRead rate by scheme:\n")
print(cmp[, .(runs = .N, read_rate = round(mean(took_best, na.rm = TRUE), 3),
              designed_gap_taken = round(mean(gap_taken == design_gap), 3)), by = scheme])

# ---- Runner read rates -----------------------------------------------------------------------

runner <- cmp[!is.na(took_best), .(
  carries = .N,
  read_rate = mean(took_best),
  left_on_table = mean(left_on_table, na.rm = TRUE),
  success = mean(success),
  yards_per_carry = mean(yards)
), by = .(ballCarrierId, runner = ballCarrierDisplayName, position = carrier_pos)]
ranked <- runner[carries >= MIN_CARRIES][order(-read_rate)]
ranked[, rank := seq_len(.N)]
cols <- c("read_rate", "left_on_table", "success", "yards_per_carry")
ranked[, (cols) := lapply(.SD, round, 3), .SDcols = cols]
cat(sprintf("\nRunners with %d+ carries: %d\n", MIN_CARRIES, nrow(ranked)))
print(ranked[, .(rank, runner, carries, read_rate, left_on_table, success, yards_per_carry)])
fwrite(ranked, file.path(OUT_DIR, "runner_read_rates.csv"))

saveRDS(cmp, file.path(CACHE_DIR, "compare.rds"))
saveRDS(gaps, file.path(CACHE_DIR, "gaps_scored.rds"))
saveRDS(model, file.path(CACHE_DIR, "gap_model.rds"))

# ==== STEP 6: CHECKS AND SCHEME ANIMATIONS (plan 3.6) ==============================
#
# Checks that the labels and the read rate mean what we say they mean.
#   1. Runner side from tracking vs the side in playDescription
#   2. Gap taken vs the play-by-play direction (end / tackle / guard / middle)
#   3. Read rate in weeks 1-4 vs weeks 5-9: if it does not hold up, it is not a skill
#   4. Chance baseline: how often a runner blind to the defense, going the same
#      way, would hit the best gap
#   5. Animations of 10 runs per scheme, to check the scheme labels by eye
#
# Output: output/validation_*.csv, output/animations/<scheme>/*.mp4

message("\n==== Step 6: checks ====")

PLAYS_PER_SCHEME <- 10
MIN_HALF_CARRIES <- 40

runs <- readRDS(file.path(CACHE_DIR, "runs.rds"))
cmp  <- readRDS(file.path(CACHE_DIR, "compare.rds"))

# ---- 1. Side check ------------------------------------------------------------------------

side <- runs[in_analysis == TRUE & pbp_side %in% c("left", "right") & !is.na(side_track),
             .(runs = .N, agree = round(mean(side_track == pbp_side), 3)), by = week][order(week)]
side <- rbind(side, runs[in_analysis == TRUE & pbp_side %in% c("left", "right") & !is.na(side_track),
                         .(week = NA, runs = .N, agree = round(mean(side_track == pbp_side), 3))])
cat("1. Runner side vs play-by-play side (plan target: about 95%)\n"); print(side)
fwrite(side, file.path(OUT_DIR, "validation_side.csv"))

# ---- 2. Gap taken vs play-by-play direction ----------------------------------------------------

cmp <- merge(cmp, runs[, .(gameId, playId, pbp_dir)], by = c("gameId", "playId"))
cmp[, pbp_part := factor(sub("^(left|right) ", "", pbp_dir),
                         levels = c("up the middle", "guard", "tackle", "end"))]
cmp[, taken_letter := substr(gap_taken, 2, 2)]
dirs <- dcast(cmp[!is.na(pbp_part)], pbp_part ~ taken_letter, fun.aggregate = length)
cat("\n2. Gap letter taken (columns) by play-by-play direction (rows)\n"); print(dirs)
fwrite(dirs, file.path(OUT_DIR, "validation_gap_vs_pbp.csv"))

# ---- 3. Is the read rate stable? ---------------------------------------------------------------

cmp[, half := fifelse(week <= 4, "weeks_1_4", "weeks_5_9")]
halves <- dcast(cmp[!is.na(took_best), .(n = .N, read_rate = mean(took_best)),
                    by = .(ballCarrierId, ballCarrierDisplayName, half)],
                ballCarrierId + ballCarrierDisplayName ~ half, value.var = c("n", "read_rate"))
# Sensitivity check with a lower bar: more runners, noisier rates
loose <- halves[n_weeks_1_4 >= 20 & n_weeks_5_9 >= 20]
ct20 <- cor.test(loose$read_rate_weeks_1_4, loose$read_rate_weeks_5_9)
halves <- halves[n_weeks_1_4 >= MIN_HALF_CARRIES & n_weeks_5_9 >= MIN_HALF_CARRIES]
ct <- cor.test(halves$read_rate_weeks_1_4, halves$read_rate_weeks_5_9)
cat(sprintf(paste0("\n3. Read rate, weeks 1-4 vs weeks 5-9, %d runners with %d+ carries in each half:\n",
                   "   r = %.2f (95%% CI %.2f to %.2f), p = %.3f\n"),
            nrow(halves), MIN_HALF_CARRIES, ct$estimate, ct$conf.int[1], ct$conf.int[2], ct$p.value))
cat(sprintf("   With 20+ carries in each half: %d runners, r = %.2f (95%% CI %.2f to %.2f), p = %.3f\n",
            nrow(loose), ct20$estimate, ct20$conf.int[1], ct20$conf.int[2], ct20$p.value))
cat(if (ct$p.value < 0.05 && ct$estimate > 0)
  "   The read rate carries over between halves, so it behaves like a skill.\n" else
  "   The read rate does not carry over between halves: treat runner rankings as noise.\n")
fwrite(halves, file.path(OUT_DIR, "validation_split_half.csv"))

# ---- 4. Chance baseline --------------------------------------------------------------------------

# A runner blind to the defense: shuffle the gaps runners took among plays with
# the same scheme, the same set of gaps and the same heading side at the handoff
# (the side of the gap he can reach soonest). A uniform pick among the play's
# gaps ignores that runners mostly go where the play is called, so it is too low.
gaps    <- readRDS(file.path(CACHE_DIR, "gaps_scored.rds"))
heading <- gaps[order(t_runner), .SD[1], by = .(gameId, playId)][, .(gameId, playId, head_side = side)]
gapset  <- gaps[, .(gapset = paste(sort(gap), collapse = ","), n_gaps = .N), by = .(gameId, playId)]
bl <- merge(merge(cmp[!is.na(took_best)], heading, by = c("gameId", "playId")), gapset, by = c("gameId", "playId"))
set.seed(355)
hits <- replicate(200, bl[, .(best_gap, g = sample(gap_taken)), by = .(scheme, gapset, head_side)][, mean(g == best_gap)])
baseline <- data.table(read_rate = mean(bl$took_best), uniform_pick = mean(1 / bl$n_gaps), blind_same_way = mean(hits),
                       blind_lo = unname(quantile(hits, 0.025)), blind_hi = unname(quantile(hits, 0.975)))
cat(sprintf(paste0("\n4. Read rate %.3f vs a blind runner going the same way %.3f (95%% range %.3f to %.3f);",
                   " uniform pick %.3f\n"),
            baseline$read_rate, baseline$blind_same_way, baseline$blind_lo, baseline$blind_hi, baseline$uniform_pick))
fwrite(baseline, file.path(OUT_DIR, "validation_baseline.csv"))

# ---- 5. Scheme animations -------------------------------------------------------------------------

# Existing files are kept. Delete output/animations/ to render them again.
if (MAKE_ANIMATIONS) {
  set.seed(355)
  pick <- cmp[taken_source == "crossed_los", .SD[sample(.N, min(.N, PLAYS_PER_SCHEME))], by = scheme]
  for (i in seq_len(nrow(pick))) {
    anim_dir <- file.path(OUT_DIR, "animations", pick$scheme[i])
    dir.create(anim_dir, recursive = TRUE, showWarnings = FALSE)
    f <- file.path(anim_dir, sprintf("%s_%s.mp4", pick$gameId[i], pick$playId[i]))
    if (!file.exists(f)) animate_play(pick$gameId[i], pick$playId[i], f)
  }
  fwrite(pick[, .(scheme, gameId, playId, runner = ballCarrierDisplayName, design_gap, gap_taken)],
         file.path(OUT_DIR, "animations", "index.csv"))
  cat(sprintf("\n5. Wrote %d animations to %s\n", nrow(pick), file.path(OUT_DIR, "animations")))
}

# ==== STEP 7: FIGURES ==============================================================
#
#   fig_play_good_read.png        required play visual (a run where the back beat the call)
#   fig_play_followed_call.png    a run that followed the call into a closed gap
#   fig_groups.png                success rate of the four designed/best/taken groups
#   fig_best_vs_not.png           success when the runner took the best gap, by scheme
#   fig_runner_read_rates.png     read rate of runners with 100+ carries

message("\n==== Step 7: figures ====")

cmp    <- readRDS(file.path(CACHE_DIR, "compare.rds"))
ranked <- fread(file.path(OUT_DIR, "runner_read_rates.csv"))
gaps   <- readRDS(file.path(CACHE_DIR, "gaps_scored.rds"))

COL_SERIES_1 <- "#2a78d6"
COL_SERIES_2 <- "#eb6834"

chart_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(plot.background = element_rect(fill = COL_FIELD, colour = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = COL_GRID, linewidth = 0.3),
          axis.text = element_text(colour = COL_INK),
          axis.title = element_text(colour = COL_INK, size = 9),
          plot.title = element_text(colour = "#0b0b0b", face = "bold"),
          plot.subtitle = element_text(colour = COL_INK, margin = margin(b = 8)),
          plot.caption = element_text(colour = COL_INK, hjust = 0),
          legend.position = "top", legend.justification = "left",
          legend.text = element_text(colour = COL_INK),
          plot.margin = margin(12, 16, 12, 12))
}
save_fig <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(OUT_DIR, name), p, width = w, height = h, dpi = 200, bg = COL_FIELD)
  message("Wrote ", file.path(OUT_DIR, name))
}

# ---- Play pictures --------------------------------------------------------------------------

regular <- unique(ranked$ballCarrierId)
pick_play <- function(grp) {
  cand <- cmp[group == grp & taken_source == "crossed_los" & ballCarrierId %in% regular &
                yards >= 6 & yards <= 20 & substr(design_gap, 1, 1) != substr(best_gap, 1, 1)]
  if (nrow(cand) == 0) cand <- cmp[group == grp & taken_source == "crossed_los"]
  cand[order(-abs(best_score - taken_score), gameId, playId)][1]
}
# The report and slides walk through these two plays; search for others only if
# a play no longer lands in its group.
good <- cmp[gameId == 2022091800 & playId == 2172 & group == "Good read"]   # Raheem Mostert
if (nrow(good) == 0) good <- pick_play("Good read")
save_fig(plot_play_handoff(good$gameId, good$playId), "fig_play_good_read.png", 9, 6.5)

closed <- cmp[gameId == 2022092900 & playId == 1627 &
                group == "Followed the call into a closed gap"]               # Joe Mixon
if (nrow(closed) == 0)
  closed <- cmp[group == "Followed the call into a closed gap" & taken_source == "crossed_los" &
                  ballCarrierId %in% regular & yards <= 1][order(-left_on_table, gameId, playId)][1]
save_fig(plot_play_handoff(closed$gameId, closed$playId), "fig_play_followed_call.png", 9, 6.5)

# ---- The four groups -------------------------------------------------------------------------

grp <- cmp[!is.na(group), .(runs = .N, success = mean(success)), by = group]
grp[, group := factor(group, levels = grp[order(success), group])]
overall <- mean(cmp[!is.na(group), success])
p <- ggplot(grp, aes(success, group)) +
  geom_col(fill = COL_SERIES_1, width = 0.6) +
  geom_vline(xintercept = overall, colour = COL_INK, linetype = "dashed", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.0f%%  (%d runs)", 100 * success, runs)),
            hjust = -0.08, colour = COL_INK, size = 3.3) +
  annotate("text", x = overall, y = nrow(grp) + 0.55,
           label = sprintf("All gap and zone runs: %.0f%%", 100 * overall),
           hjust = -0.05, colour = COL_INK, size = 3) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0, 0.75),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Runs succeed most when the back leaves the call for the best gap",
       subtitle = "Success rate by how the designed, best and taken gaps line up (gap and zone runs)",
       x = "Run success rate", y = NULL,
       caption = "Success: 40% of yards to go on 1st down, 60% on 2nd, all of it on 3rd and 4th.") +
  chart_theme()
save_fig(p, "fig_groups.png", 8.5, 4.5)

# ---- Took the best gap vs not, by scheme ---------------------------------------------------------

sch_lab <- c(gap = "Gap (pull)", inside_zone = "Inside zone", outside_zone = "Outside zone",
             other = "Man / duo / other")
bv <- cmp[!is.na(took_best), .(success = mean(success), runs = .N), by = .(scheme, took_best)]
bv[, scheme := factor(sch_lab[scheme], levels = rev(sch_lab))]
# Second level is drawn on top within each scheme
bv[, choice := factor(fifelse(took_best, "Took the best gap", "Took another gap"),
                      levels = c("Took another gap", "Took the best gap"))]
p <- ggplot(bv, aes(success, scheme, fill = choice)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.66, colour = COL_FIELD, linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f%%", 100 * success)), position = position_dodge(width = 0.72),
            hjust = -0.15, colour = COL_INK, size = 3.1) +
  scale_fill_manual(values = c(`Took the best gap` = COL_SERIES_1, `Took another gap` = COL_SERIES_2),
                    breaks = c("Took the best gap", "Took another gap"), name = NULL) +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0, 0.7),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "The best gap pays off on inside zone and man runs, not on gap or outside zone",
       subtitle = "Run success rate by scheme and by whether the runner took the best gap",
       x = "Run success rate", y = NULL) +
  chart_theme()
save_fig(p, "fig_best_vs_not.png", 8.5, 4.5)

# ---- Runner read rates -----------------------------------------------------------------------------

# Chance line: a runner blind to the defense but going the same way (step 6, check 4)
chance <- fread(file.path(OUT_DIR, "validation_baseline.csv"))$blind_same_way
league <- mean(cmp$took_best, na.rm = TRUE)
ranked[, runner := factor(runner, levels = ranked[order(read_rate), runner])]
p <- ggplot(ranked, aes(read_rate, runner)) +
  geom_vline(xintercept = chance, colour = COL_INK, linetype = "dotted", linewidth = 0.4) +
  geom_vline(xintercept = league, colour = COL_INK, linetype = "dashed", linewidth = 0.4) +
  geom_segment(aes(x = chance, xend = read_rate, yend = runner), colour = COL_GRID, linewidth = 0.6) +
  geom_point(colour = COL_SERIES_1, size = 2.8) +
  annotate("text", x = chance, y = 0.4, label = sprintf("Blind pick, same way %.0f%%", 100 * chance),
           hjust = -0.05, colour = COL_INK, size = 3) +
  annotate("text", x = league, y = 0.4, label = sprintf("All runs %.0f%%", 100 * league),
           hjust = -0.05, colour = COL_INK, size = 3) +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0, 0.5)) +
  coord_cartesian(clip = "off") +
  labs(title = "Read rate of runners with 100+ carries",
       subtitle = "Share of runs where the runner took the highest-scoring gap at the handoff",
       x = "Read rate", y = NULL,
       caption = "Weeks 1 to 9, 2022. Runners differ only a little beyond chance (stress tests), so read the order loosely.") +
  chart_theme()
save_fig(p, "fig_runner_read_rates.png", 8.5, 6)

# ==== STEP 8: STRESS TESTS =========================================================
#
# Stress tests of the assumptions and results in steps 1 to 7. Set
# RUN_STRESS_TESTS to FALSE at the top to skip this step, or RUN_VARIANTS to
# FALSE to skip only part 6.
#
#   1. Data checks        penalty yards, carriers, handoff fallbacks
#   2. Chance baselines   how often a runner blind to the defense would hit the
#                         best gap, keeping his scheme, gap set and direction
#   3. Payoff checks      controls, clustered bootstrap, subsets, cross-fitting
#   4. Scheme checks      gap taken vs the designed side, wider designed gaps,
#                         zone split point
#   5. Skill checks       do runners differ in read rate beyond chance?
#   6. Variants           redo steps 3 and 5 with one setting changed at a time
#
# Output: output/stress_*.csv

# Everything from here to the end of the file runs only when RUN_STRESS_TESTS is TRUE.
if (RUN_STRESS_TESTS) {
message("\n==== Step 8: stress tests ====")

N_PERM <- 200   # permutations for each blind-runner baseline
N_BOOT <- 1000  # bootstrap draws
old_width <- options(width = 160)

runs <- readRDS(file.path(CACHE_DIR, "runs.rds"))[in_analysis == TRUE]
cmp  <- readRDS(file.path(CACHE_DIR, "compare.rds"))[!is.na(took_best)]
gaps <- readRDS(file.path(CACHE_DIR, "gaps_scored.rds"))
sch  <- readRDS(file.path(CACHE_DIR, "scheme.rds"))

# Heading side at the handoff: the side of the gap the runner can reach soonest
# given his speed and direction. Gap set: the gaps a play has (6 to 8).
heading <- gaps[order(t_runner), .SD[1], by = .(gameId, playId)][, .(gameId, playId, head_side = side)]
gapset  <- gaps[, .(gapset = paste(sort(gap), collapse = ","), n_gaps = .N), by = .(gameId, playId)]

add_keys <- function(d) {
  d <- merge(d, heading, by = c("gameId", "playId"))
  merge(d, gapset, by = c("gameId", "playId"))
}
a <- add_keys(cmp)
a <- merge(a, runs[, .(gameId, playId, down, yardsToGo, defendersInTheBox, possessionTeam,
                       handoff_source, playResult, expectedPointsAdded)], by = c("gameId", "playId"))

# Round for printing: shares and differences to 3 places, p-values to 3 significant digits
fmt <- function(d) d[, lapply(.SD, function(v) if (is.double(v)) signif(v, 3) else v)]
rnd <- function(d) d[, lapply(.SD, function(v) if (is.double(v)) round(v, 3) else v)]

diff_success <- function(d) d[, mean(success[took_best]) - mean(success[!took_best])]
p_success <- function(d) {
  prop.test(table(factor(d$took_best, c(TRUE, FALSE)), factor(d$success, c(1, 0))))$p.value
}
# Blind runner: shuffle the gaps runners took among plays in the same strata.
# Returns the mean and 95% range of how often the shuffled gap is the best gap.
blind <- function(d, strata, n = N_PERM) {
  hits <- replicate(n, {
    s <- d[, .(best_gap, g = sample(gap_taken)), by = strata]
    mean(s$g == s$best_gap)
  })
  c(mean = mean(hits), lo = unname(quantile(hits, 0.025)), hi = unname(quantile(hits, 0.975)))
}
set.seed(355)

# ---- 8.1 Data checks ------------------------------------------------------------------------------

cat("1. Data checks\n")
cat(sprintf("   Runs where penalty yards change the result: %d (success differs on %d)\n",
            a[playResult != yards, .N],
            a[playResult != yards & run_success(down, yardsToGo, playResult) != success, .N]))
cat("   Carriers:", paste(a[, .N, by = carrier_pos][order(-N), sprintf("%s %d", carrier_pos, N)], collapse = ", "), "\n")
cat("   Handoff source:", paste(a[, .N, by = handoff_source][, sprintf("%s %d", handoff_source, N)], collapse = ", "), "\n")

# ---- 8.2 Chance baselines ----------------------------------------------------------------------------

cat("\n2. How often would a runner blind to the defense hit the best gap?\n")
base_rows <- list(
  data.table(baseline = "Uniform pick among the play's gaps", mean = mean(1 / a$n_gaps), lo = NA, hi = NA),
  data.table(baseline = "Blind, same scheme and gap set", t(blind(a, c("scheme", "gapset")))),
  data.table(baseline = "Blind, same scheme, gap set and heading side", t(blind(a, c("scheme", "gapset", "head_side"))))
)
baselines <- rbindlist(base_rows)
baselines <- rbind(data.table(baseline = "Runners (read rate)", mean = mean(a$took_best), lo = NA, hi = NA), baselines)
print(rnd(baselines))

by_scheme <- a[, {
  b <- blind(.SD, c("gapset", "head_side"))
  .(runs = .N, read_rate = mean(took_best), blind_same_heading = b[["mean"]], blind_hi = b[["hi"]])
}, by = scheme]
cat("   By scheme:\n"); print(rnd(by_scheme))

# How clearly the best gap stands out: score margin over the second-best gap
margin <- gaps[order(-score), .(margin = score[1] - score[2]), by = .(gameId, playId)]
a <- merge(a, margin, by = c("gameId", "playId"))
a[, margin_bin := cut(margin, c(-Inf, 0.01, 0.03, 0.06, Inf),
                      labels = c("under 1 pt", "1 to 3 pts", "3 to 6 pts", "over 6 pts"))]
by_margin <- a[, .(runs = .N, read_rate = mean(took_best),
                   success_best = mean(success[took_best]), success_other = mean(success[!took_best])),
               by = margin_bin][order(margin_bin)]
cat("   Read rate by how far the best gap's score beats the next gap:\n")
print(rnd(by_margin))

fwrite(rbind(baselines[, .(group = "all", baseline, mean, lo, hi)],
             by_scheme[, .(group = scheme, baseline = "Blind, same gap set and heading side",
                           mean = blind_same_heading, lo = NA, hi = blind_hi)]),
       file.path(OUT_DIR, "stress_baselines.csv"))
fwrite(by_margin, file.path(OUT_DIR, "stress_margin.csv"))

# ---- 8.3 Payoff checks -----------------------------------------------------------------------------------

cat("\n3. Does taking the best gap still pay off?\n")
m <- glm(success ~ took_best + best_score + factor(down) + log(yardsToGo) + short_yardage + scheme +
           factor(pmin(defendersInTheBox, 8)) + I(carrier_pos == "RB"), family = binomial, data = a)
ame <- mean(predict(m, transform(a, took_best = TRUE), type = "response") -
              predict(m, transform(a, took_best = FALSE), type = "response"))

boot_by <- function(cluster) {
  ids <- unique(a[[cluster]])
  draws <- replicate(N_BOOT, {
    pick <- data.table(sample(ids, replace = TRUE)); setnames(pick, cluster)
    diff_success(a[pick, on = cluster, allow.cartesian = TRUE])
  })
  quantile(draws, c(0.025, 0.975))
}
ci_game <- boot_by("gameId")
ci_team <- boot_by("possessionTeam")

# Cross-fit: score weights from the other half of the season
gw <- merge(gaps, runs[, .(gameId, playId, week)], by = c("gameId", "playId"))
cross <- rbindlist(lapply(list(1:4, 5:9), function(train) {
  fit <- glm(success ~ width + n_unblocked, family = binomial, data = gw[is_taken == TRUE & week %in% train])
  test <- gw[!week %in% train]
  test[, s := predict(fit, test, type = "response")]
  b <- test[order(-s, t_runner), .SD[1], by = .(gameId, playId)][, .(gameId, playId, best_cf = gap)]
  merge(a[!week %in% train], b, by = c("gameId", "playId"))
}))[, took_best := gap_taken == best_cf]

subsets <- list(
  "All runs"                                  = a,
  "Running backs only"                        = a[carrier_pos == "RB"],
  "Runner crossed the line"                   = a[taken_source == "crossed_los"],
  "Handoff event in the data"                 = a[handoff_source == "event"],
  "Not short yardage"                         = a[short_yardage == FALSE],
  "1st and 10 only"                           = a[down == 1 & yardsToGo == 10],
  "Weeks 1 to 4"                              = a[week <= 4],
  "Weeks 5 to 9"                              = a[week >= 5],
  "Score weights from the other half (cross-fit)" = cross
)
payoff <- rbindlist(lapply(names(subsets), function(nm) {
  d <- subsets[[nm]]
  data.table(check = nm, runs = nrow(d), read_rate = mean(d$took_best),
             success_best = mean(d$success[d$took_best]), success_other = mean(d$success[!d$took_best]),
             diff = diff_success(d), p = p_success(d))
}))
payoff <- rbind(payoff, data.table(
  check = c("With controls (average effect)", "95% CI, games resampled", "95% CI, offenses resampled",
            "EPA per run (took best minus not)"),
  diff = c(ame, NA, NA, a[, mean(expectedPointsAdded[took_best], na.rm = TRUE) - mean(expectedPointsAdded[!took_best], na.rm = TRUE)]),
  lo = c(NA, ci_game[[1]], ci_team[[1]], NA), hi = c(NA, ci_game[[2]], ci_team[[2]], NA)), fill = TRUE)
print(fmt(payoff))

ps <- a[, .(runs = .N, diff = diff_success(.SD), p = p_success(.SD)), by = scheme]
ps[, p_holm := p.adjust(p, "holm")]
cat("   By scheme, with Holm-adjusted p-values:\n")
print(fmt(ps))
cat(sprintf("   Median yards: %s when taking the best gap, %s otherwise (Wilcoxon p = %.2g)\n",
            a[took_best == TRUE, median(yards)], a[took_best == FALSE, median(yards)],
            wilcox.test(yards ~ took_best, data = a)$p.value))
fwrite(rbind(payoff, ps[, .(check = paste("Scheme:", scheme), runs, diff, p = p_holm)], fill = TRUE),
       file.path(OUT_DIR, "stress_payoff.csv"))

# ---- 8.4 Scheme checks -------------------------------------------------------------------------------------

cat("\n4. Scheme checks\n")
d <- a[!is.na(design_side)]
d[, where := paste(fifelse(substr(gap_taken, 1, 1) == design_side, "play side", "back side"),
                   substr(gap_taken, 2, 2))]
where <- dcast(d[, .N, by = .(scheme, where)][, share := N / sum(N), by = scheme],
               scheme ~ where, value.var = "share", fill = 0)
cat("   Gap taken relative to the designed side (share of runs):\n")
print(rnd(where))

# Wider designed gaps: inside zone = play-side A or B, outside zone = play-side C or D
in_design <- function(g, side, gap, scheme) {
  fifelse(scheme == "inside_zone", substr(g, 2, 2) %in% c("A", "B") & substr(g, 1, 1) == side,
  fifelse(scheme == "outside_zone", substr(g, 2, 2) %in% c("C", "D") & substr(g, 1, 1) == side,
          g == gap))
}
d[, `:=`(best_in = in_design(best_gap, design_side, design_gap, scheme),
         taken_in = in_design(gap_taken, design_side, design_gap, scheme))]
d[, group_wide := fcase(best_in & took_best, "Worked as designed",
                        !best_in & took_best, "Good read",
                        best_in & !took_best, "Missed the open gap",
                        !best_in & taken_in, "Followed the call into a closed gap",
                        default = "Took a third gap")]
groups_wide <- merge(
  d[, .(runs_main = .N, success_main = mean(success)), by = .(group = group)],
  d[, .(runs_wide = .N, success_wide = mean(success)), by = .(group = group_wide)], by = "group")
cat("   Groups with the main and the wider designed gaps:\n")
print(rnd(groups_wide[order(-success_main)]))

z <- merge(a[scheme %in% c("inside_zone", "outside_zone")], sch[, .(gameId, playId, zone_flow)],
           by = c("gameId", "playId"))
zone_cut <- rbindlist(lapply(c(1.5, 1.75, 2.0, 2.25, 2.5), function(cut) {
  z[, .(runs = .N, diff = diff_success(.SD)), by = .(zone = fifelse(zone_flow >= cut, "outside", "inside"))][
    , cut := cut]
}))
cat("   Payoff by zone split point (yards of line flow):\n")
print(dcast(zone_cut, cut ~ zone, value.var = c("runs", "diff"))[, lapply(.SD, function(v) round(v, 3))])
# A gap closed at the handoff (width 0 after the spacer) is not always closed when
# the runner gets there: success of taken gaps by width and free defenders
closed_gaps <- gaps[is_taken == TRUE, .(runs = .N, success = mean(success)),
                    by = .(closed_at_handoff = width == 0, unblocked = fifelse(n_unblocked <= 1, "0 or 1", "2 or more"))]
cat("   Success through the gap taken, by width at the handoff and free defenders:\n")
print(rnd(closed_gaps[order(closed_at_handoff, unblocked)]))
fwrite(groups_wide, file.path(OUT_DIR, "stress_groups.csv"))
fwrite(zone_cut, file.path(OUT_DIR, "stress_zone_split.csv"))

# ---- 8.5 Skill checks ----------------------------------------------------------------------------------------

cat("\n5. Do runners differ in read rate beyond chance?\n")
a[, expected := mean(took_best), by = scheme]   # league rate for the scheme of each carry
rr <- a[, .(carries = .N, read_rate = mean(took_best), expected = mean(expected),
            var_noise = sum(expected * (1 - expected)) / .N^2), by = ballCarrierId]
skill <- rbindlist(lapply(c(50, 100), function(k) {
  x <- rr[carries >= k]
  chi <- sum((x$read_rate - x$expected)^2 / x$var_noise)
  data.table(min_carries = k, runners = nrow(x), chi2 = chi, p = pchisq(chi, nrow(x), lower.tail = FALSE),
             true_sd = sqrt(max(0, var(x$read_rate - x$expected) - mean(x$var_noise))),
             noise_sd = sqrt(mean(x$var_noise)))
}))
big <- rr[carries >= 80, ballCarrierId]
half_r <- function(col) mean(replicate(N_PERM, {
  b <- a[ballCarrierId %in% big][, h := sample(rep(1:2, length.out = .N)), by = ballCarrierId]
  w <- dcast(b[, .(v = mean(get(col))), by = .(ballCarrierId, h)], ballCarrierId ~ h, value.var = "v")
  cor(w$`1`, w$`2`)
}))
r_read <- half_r("took_best"); r_succ <- half_r("success")
skill[, `:=`(random_half_r_read = r_read, reliability_read = 2 * r_read / (1 + r_read),
             random_half_r_success = r_succ)]
print(skill[, lapply(.SD, function(v) round(v, 3))])
fwrite(skill, file.path(OUT_DIR, "stress_skill.csv"))

# ---- 8.6 Variants: one setting changed at a time -----------------------------------------------------------

# Headline numbers for one version of the analysis (the main one or a variant)
headline <- function(cmp_v) {
  d <- add_keys(cmp_v[!is.na(took_best)])
  half <- dcast(d[, .(n = .N, r = mean(took_best)), by = .(ballCarrierId, h = week <= 4)],
                ballCarrierId ~ h, value.var = c("n", "r"))[n_TRUE >= 40 & n_FALSE >= 40]
  sd <- function(s) d[scheme == s, diff_success(.SD)]
  set.seed(355)   # same blind-runner draws whichever order the variants run in
  data.table(read_rate = mean(d$took_best), blind_same_heading = blind(d, c("scheme", "gapset", "head_side"), 50)[["mean"]],
             success_best = mean(d$success[d$took_best]), success_other = mean(d$success[!d$took_best]),
             diff = diff_success(d), p = p_success(d),
             inside_zone = sd("inside_zone"), other = sd("other"), gap = sd("gap"), outside_zone = sd("outside_zone"),
             good_read = d[group == "Good read", mean(success)], worked_as_designed = d[group == "Worked as designed", mean(success)],
             good_read_is_top_group = d[!is.na(group), .(s = mean(success)), by = group][which.max(s), group] == "Good read",
             split_half_r = cor(half$r_TRUE, half$r_FALSE))
}

# Each variant changes one entry of GAP_SETTINGS (step 3) or the gap score model (step 5)
variants <- list(
  "Runner speed toward the gap ignored (v0 = 0)" = list(gaps = list(runner_speed = "zero")),
  "Runner speed counted in every direction"      = list(gaps = list(runner_speed = "full")),
  "Acceleration 6 yd/s2"                         = list(gaps = list(accel = 6)),
  "Acceleration 8 yd/s2"                         = list(gaps = list(accel = 8)),
  "Top speed 7 yd/s"                             = list(gaps = list(top_speed = 7)),
  "No projection of the edges"                   = list(gaps = list(max_projection = 0)),
  "Projection up to 1.0 s"                       = list(gaps = list(max_projection = 1.0)),
  "Block radius 1.0 yd"                          = list(gaps = list(block_radius = 1.0)),
  "Block radius 2.0 yd"                          = list(gaps = list(block_radius = 2.0)),
  "No 1-yard spacer"                             = list(gaps = list(spacer = 0)),
  "Edge gap cap 3 yd"                            = list(gaps = list(edge_cap = 3)),
  "Edge gap cap 8 yd"                            = list(gaps = list(edge_cap = 8)),
  "Snapshot 0.5 s after the handoff"             = list(gaps = list(snapshot_delay = 5L)),
  "Score counts all defenders, blocked or not"   = list(model = success ~ width + n_first),
  "Score uses unblocked defenders only"          = list(model = success ~ n_unblocked)
)

if (RUN_VARIANTS) {
  cat("\n6. Variants (one setting changed at a time)\n")
  linemen_v <- readRDS(file.path(CACHE_DIR, "linemen.rds"))
  taken_v   <- readRDS(file.path(CACHE_DIR, "gap_taken.rds"))
  gaps_v    <- readRDS(file.path(CACHE_DIR, "gaps.rds"))   # step 3 with the main settings
  ho_v      <- list(`0` = handoff_snapshot(runs))
  for (delay in unique(unlist(lapply(variants, function(v) v$gaps$snapshot_delay))))
    ho_v[[as.character(delay)]] <- handoff_snapshot(runs, delay)

  run_variant <- function(name) {
    v <- variants[[name]]
    g <- gaps_v
    if (!is.null(v$gaps)) {
      cfg <- modifyList(GAP_SETTINGS, v$gaps)
      g <- build_gaps(runs, linemen_v, sch, ho_v[[as.character(cfg$snapshot_delay)]], cfg)
    }
    res <- score_gaps(runs, sch, g, taken_v, if (is.null(v$model)) GAP_MODEL else v$model)
    cbind(variant = name, headline(res$cmp))
  }
  # Several at once on Mac and Linux outside RStudio; one after another otherwise
  forks_ok <- .Platform$OS.type == "unix" && Sys.getenv("RSTUDIO") != "1"
  res <- if (forks_ok) parallel::mclapply(names(variants), run_variant,
                                          mc.cores = max(1L, min(4L, parallel::detectCores() - 1L)))
         else lapply(names(variants), run_variant)
  failed <- vapply(res, inherits, logical(1), what = "try-error")
  if (any(failed)) stop("Variant failed: ", names(variants)[failed][1], "\n", res[failed][[1]])
  var_tab <- rbind(cbind(variant = "Main analysis", headline(cmp)), rbindlist(res))
  print(fmt(var_tab[, .(variant, read_rate, blind_same_heading, success_best, success_other, diff,
                        inside_zone, gap, split_half_r)]))
  fwrite(var_tab, file.path(OUT_DIR, "stress_variants.csv"))
}

options(old_width)
}  # end of step 8

message("\nDone. Figures and tables are in ", normalizePath(OUT_DIR), ".")
