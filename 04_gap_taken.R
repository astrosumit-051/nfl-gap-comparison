# 04_gap_taken.R  (plan step 3.4)
# The gap taken is the gap where the runner crosses the line of scrimmage.
# Gaps are drawn from where the linemen stand at the handoff, the same frame
# used for the best gap. (By the time the runner crosses, often 2 to 3 s after
# the snap, a zone line has flowed several yards and no longer marks the gaps.)
# Runs stopped behind the line use the runner's position at first contact (or
# at the end of the play).
#
# Output: cache/gap_taken.rds (one row per run)

source("R/config.R")
source("R/gaps.R")

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
  e <- order_edges(e)                                         # same edges as 03_gaps.R
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

# ---- Summary: gap taken vs the play-by-play direction ----------------------------

cat("\nRuns with a gap taken:", nrow(taken), "of", nrow(runs), "\n")
print(table(taken$taken_source))
cat("\nGap letter taken by play-by-play direction (rows):\n")
taken[, pbp_part := sub("^(left|right) ", "", pbp_dir)]
print(dcast(taken[!is.na(pbp_part)], pbp_part ~ substr(gap_taken, 2, 2), fun.aggregate = length))
