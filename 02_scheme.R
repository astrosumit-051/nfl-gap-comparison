# 02_scheme.R  (plan step 3.2)
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

source("R/config.R")
source("R/gaps.R")

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

# ---- Lineman movement and puller paths --------------------------------------

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

# ---- Gap (pull) rule ---------------------------------------------------------

move[, crosses_center := sign(y0 - cy0) != sign(y1 - cy0)]
move[, against_line := sign(dy) != sign(med_others) & abs(med_others) > 0]
pullers <- move[position %in% c("G", "T") & abs(dy) >= PULL_YARDS &
                  (crosses_center | against_line)]
# One puller per run: the biggest lateral mover
pullers <- pullers[order(-abs(dy)), .SD[1], by = .(gameId, playId)]
pullers[, side := fifelse(dy > 0, "L", "R")]

# ---- Zone rule ----------------------------------------------------------------

zone <- move[, .(n_left  = sum(dy >=  ZONE_YARDS),
                 n_right = sum(dy <= -ZONE_YARDS),
                 mean_dy = mean(dy)), by = .(gameId, playId)]
zone[, side := fcase(n_left >= ZONE_MIN_OL & n_left >= n_right, "L",
                     n_right >= ZONE_MIN_OL, "R",
                     default = NA_character_)]
zone[, flow := fifelse(side == "L", mean_dy, -mean_dy)]   # movement toward the play side

# ---- Combine ------------------------------------------------------------------

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

# ---- Designed gap -------------------------------------------------------------

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

# ---- Summary and comparison with the week 1 prototype -------------------------

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
