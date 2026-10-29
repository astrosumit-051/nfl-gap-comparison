# 01_prepare.R  (plan step 3.1)
# Flip plays, find the key frames, the line of scrimmage, the offensive line,
# the tight ends on the line and the point where the runner crosses the line.
#
# Input:  cache/ files from 00_setup.R
# Output:
#   cache/runs.rds              one row per run, with flags and key frames
#   cache/linemen.rds           one row per OL / on-line TE per run, at the snap
#   cache/tracking_prep_wkN.rds flipped tracking with a `role` column

source("R/config.R")

runs    <- readRDS(file.path(CACHE_DIR, "plays_runs.rds"))
players <- readRDS(file.path(CACHE_DIR, "players.rds"))

# ---- Play-level flags -------------------------------------------------------

runs[, `:=`(
  success       = run_success(down, yardsToGo, playResult),
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

# ---- Per-week processing ----------------------------------------------------

play_info <- list()
line_info <- list()

for (wk in WEEKS) {
  trk <- readRDS(file.path(CACHE_DIR, sprintf("tracking_runs_wk%d.rds", wk)))
  flip_tracking(trk)

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

# ---- Runner side and the check against the play-by-play ----------------------

# After the flip, +y is the offense's left.
runs[, side_track := fifelse(cross_y > ball_y, "left", "right")]
runs[, pbp_match := fifelse(pbp_side %in% c("left", "right"), side_track == pbp_side, NA)]

# Runs used in the main analysis: a snap, a handoff, not inside the opponent's 5
runs[, has_handoff := !is.na(handoff_frame)]
runs[, in_analysis := !inside_opp5 & !is.na(snap_frame) & has_handoff]

saveRDS(runs, file.path(CACHE_DIR, "runs.rds"))
saveRDS(linemen, file.path(CACHE_DIR, "linemen.rds"))

# ---- Summary ----------------------------------------------------------------

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
