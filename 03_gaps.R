# 03_gaps.R  (plan step 3.3, parts 1 to 4)
# At the handoff, describe every gap on every run: its projected width when the
# runner gets there and the number of unblocked defenders who get there first.
#
# Runner and defender arrival times use a simple motion model: start from the
# player's speed toward the gap, accelerate at 4 yd/s^2, top out at 8 yd/s.
# Gap edges are the linemen's lateral positions projected forward with their
# velocity at the handoff, at most 0.5 s ahead (plan rule).
# Width = lateral distance between the two edge players, minus 1 yard. An edge
# gap's outer side is 5 yd outside the end man (or the sideline if nearer).
# A defender is blocked when an offensive player is within 1.5 yd of him AND is
# between him and the gap (closer to the gap, on the same side of him).
#
# Output: cache/gaps.rds (one row per run and gap)

source("R/config.R")
source("R/gaps.R")

MAX_PROJECTION <- 0.5   # s
ACCEL          <- 4     # yd/s^2
TOP_SPEED      <- 8     # yd/s
EDGE_CAP       <- 5     # yd, widest an edge gap can be
BLOCK_RADIUS   <- 1.5   # yd
SPACER         <- 1     # yd taken off every width (plan rule)

# Time to cover distance d from speed v0 (toward the target) with the motion model
time_to_reach <- function(d, v0, a = ACCEL, vmax = TOP_SPEED) {
  v0 <- pmin(pmax(v0, 0), vmax)
  t1 <- (vmax - v0) / a
  d1 <- v0 * t1 + 0.5 * a * t1^2
  fifelse(d <= d1, (-v0 + sqrt(v0^2 + 2 * a * d)) / a, t1 + (d - d1) / vmax)
}

runs    <- readRDS(file.path(CACHE_DIR, "runs.rds"))[in_analysis == TRUE]
linemen <- readRDS(file.path(CACHE_DIR, "linemen.rds"))
edges   <- line_edges(linemen)

# ---- Players at the handoff ----------------------------------------------------

snap_list <- list()
for (wk in WEEKS) {
  trk <- readRDS(file.path(CACHE_DIR, sprintf("tracking_prep_wk%d.rds", wk)))
  r <- runs[week == wk, .(gameId, playId, frameId = handoff_frame)]
  snap_list[[wk]] <- trk[r, on = .(gameId, playId, frameId), nomatch = 0L,
                         .(gameId, playId, nflId, role, position, x, y, s, vx, vy)]
  rm(trk); invisible(gc())
}
ho <- rbindlist(snap_list)
ho <- merge(ho, runs[, .(gameId, playId, los_x)], by = c("gameId", "playId"))

# ---- Gap edges at the handoff ----------------------------------------------------

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
edge_long <- merge(edge_long, ho[, .(gameId, playId, nflId, ex = x, ey = y, evy = vy)],
                   by = c("gameId", "playId", "nflId"))
# A pulling lineman has left his spot by the handoff. His gaps stay where he
# lined up, so use his position at the snap, standing still.
pullers <- readRDS(file.path(CACHE_DIR, "scheme.rds"))[!is.na(puller_id), .(gameId, playId, nflId = puller_id)]
pullers <- merge(pullers, linemen[, .(gameId, playId, nflId, snap_y = y)], by = c("gameId", "playId", "nflId"))
edge_long[pullers, on = .(gameId, playId, nflId), `:=`(ey = i.snap_y, evy = 0)]

inner <- edge_long[k < 3L, .(gameId, playId, side, k_out = k + 1L, in_y = ey, in_vy = evy, in_x = ex)]
outer <- edge_long[k > 0L, .(gameId, playId, side, k_out = k, out_y = ey, out_vy = evy, out_x = ex)]
gaps <- merge(inner, outer, by = c("gameId", "playId", "side", "k_out"), all.x = TRUE)

# Edge gaps: outside the outermost edge player on each side
outermost <- edge_long[, .SD[which.max(k)], by = .(gameId, playId, side)]
edge_gaps <- outermost[, .(gameId, playId, side, k_out = k + 1L, in_y = ey, in_vy = evy, in_x = ex)]
gaps <- rbind(gaps[!is.na(out_y)], edge_gaps, fill = TRUE)
gaps[, letter := GAP_LETTERS[k_out]]
gaps[, is_edge := is.na(out_y)]
gaps[, gap := paste0(side, letter)]
gaps[, sgn := fifelse(side == "L", 1, -1)]   # outward direction in y

# Edge gap's outer side: 5 yd outside the end man, or the sideline if nearer.
# Defenders on the edge count through the unblocked-defender rule, not the width.
defs <- ho[role == "defense"]
gaps[is_edge == TRUE, `:=`(
  out_y  = fifelse(sgn > 0, pmin(in_y + EDGE_CAP, FIELD_WIDTH), pmax(in_y - EDGE_CAP, 0)),
  out_vy = in_vy
)]

# ---- Runner arrival and projected width ------------------------------------------

carrier <- ho[role == "carrier", .(gameId, playId, rx = x, ry = y, rvx = vx, rvy = vy)]
gaps <- merge(gaps, carrier, by = c("gameId", "playId"))
gaps <- merge(gaps, runs[, .(gameId, playId, los_x)], by = c("gameId", "playId"))

gaps[, `:=`(gx = los_x, gy = (in_y + out_y) / 2)]   # aim point: middle of the gap at the line
gaps[, run_dist := sqrt((gx - rx)^2 + (gy - ry)^2)]
gaps[, run_v0 := (rvx * (gx - rx) + rvy * (gy - ry)) / pmax(run_dist, 1e-6)]
gaps[, t_runner := time_to_reach(run_dist, run_v0)]
gaps[, t_proj := pmin(t_runner, MAX_PROJECTION)]
gaps[, width := pmax(abs((out_y + out_vy * t_proj) - (in_y + in_vy * t_proj)) - SPACER, 0)]
# Aim point moves with the projected edges
gaps[, gy := ((in_y + in_vy * t_proj) + (out_y + out_vy * t_proj)) / 2]

# ---- Unblocked defenders who arrive first ----------------------------------------

blockers <- ho[role %in% c("OL", "TE_line", "offense") & position != "QB",
               .(gameId, playId, bid = nflId, bx = x, by = y)]
dg <- merge(gaps[, .(gameId, playId, gap, gx, gy, t_runner)],
            defs[, .(gameId, playId, did = nflId, dx = x, dy = y, dvx = vx, dvy = vy)],
            by = c("gameId", "playId"), allow.cartesian = TRUE)
dg[, d_dist := sqrt((gx - dx)^2 + (gy - dy)^2)]
dg[, d_v0 := (dvx * (gx - dx) + dvy * (gy - dy)) / pmax(d_dist, 1e-6)]
dg[, t_def := time_to_reach(d_dist, d_v0)]
dg <- dg[t_def <= t_runner]

# Blocked: a blocker within 1.5 yd, closer to the gap than the defender, and on
# the gap side of him (positive dot product).
db <- merge(dg[, .(gameId, playId, gap, did, dx, dy, gx, gy, d_dist)],
            blockers, by = c("gameId", "playId"), allow.cartesian = TRUE)
db <- db[sqrt((bx - dx)^2 + (by - dy)^2) <= BLOCK_RADIUS &
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
saveRDS(gaps, file.path(CACHE_DIR, "gaps.rds"))

# ---- Summary ----------------------------------------------------------------------

cat("\nGaps per run:\n"); print(table(gaps[, .N, by = .(gameId, playId)]$N))
cat("\nBy gap letter:\n")
print(gaps[, .(n = .N, width = round(median(width), 2), t_runner = round(median(t_runner), 2),
               unblocked = round(mean(n_unblocked), 2), share_open = round(mean(n_unblocked == 0), 2)),
           by = .(letter, is_edge)][order(letter, is_edge)])
