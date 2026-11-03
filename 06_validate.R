# 06_validate.R  (plan step 3.6)
# Checks that the labels and the read rate mean what we say they mean.
#   1. Runner side from tracking vs the side in playDescription
#   2. Gap taken vs the play-by-play direction (end / tackle / guard / middle)
#   3. Read rate in weeks 1-4 vs weeks 5-9: if it does not hold up, it is not a skill
#   4. Chance baseline: how often a runner blind to the defense, going the same
#      way, would hit the best gap
#   5. Animations of 10 runs per scheme, to check the scheme labels by eye
#
# Output: output/validation_*.csv, output/animations/<scheme>/*.mp4

source("R/config.R")
source("R/plot_play.R")

MAKE_ANIMATIONS  <- as.logical(Sys.getenv("MAKE_ANIMATIONS", "TRUE"))  # set FALSE to skip (about 5 minutes)
PLAYS_PER_SCHEME <- 10
MIN_HALF_CARRIES <- 40

runs <- readRDS(file.path(CACHE_DIR, "runs.rds"))
cmp  <- readRDS(file.path(CACHE_DIR, "compare.rds"))

# ---- 1. Side check ----------------------------------------------------------------

side <- runs[in_analysis == TRUE & pbp_side %in% c("left", "right") & !is.na(side_track),
             .(runs = .N, agree = round(mean(side_track == pbp_side), 3)), by = week][order(week)]
side <- rbind(side, runs[in_analysis == TRUE & pbp_side %in% c("left", "right") & !is.na(side_track),
                         .(week = NA, runs = .N, agree = round(mean(side_track == pbp_side), 3))])
cat("1. Runner side vs play-by-play side (plan target: about 95%)\n"); print(side)
fwrite(side, file.path(OUT_DIR, "validation_side.csv"))

# ---- 2. Gap taken vs play-by-play direction ---------------------------------------------

cmp <- merge(cmp, runs[, .(gameId, playId, pbp_dir)], by = c("gameId", "playId"))
cmp[, pbp_part := factor(sub("^(left|right) ", "", pbp_dir),
                         levels = c("up the middle", "guard", "tackle", "end"))]
cmp[, taken_letter := substr(gap_taken, 2, 2)]
dirs <- dcast(cmp[!is.na(pbp_part)], pbp_part ~ taken_letter, fun.aggregate = length)
cat("\n2. Gap letter taken (columns) by play-by-play direction (rows)\n"); print(dirs)
fwrite(dirs, file.path(OUT_DIR, "validation_gap_vs_pbp.csv"))

# ---- 3. Is the read rate stable? --------------------------------------------------------

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

# ---- 4. Chance baseline -------------------------------------------------------------------

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

# ---- 5. Scheme animations ------------------------------------------------------------------

if (MAKE_ANIMATIONS) {
  set.seed(355)
  pick <- cmp[taken_source == "crossed_los", .SD[sample(.N, min(.N, PLAYS_PER_SCHEME))], by = scheme]
  for (i in seq_len(nrow(pick))) {
    dir <- file.path(OUT_DIR, "animations", pick$scheme[i])
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    f <- file.path(dir, sprintf("%s_%s.mp4", pick$gameId[i], pick$playId[i]))
    if (!file.exists(f)) animate_play(pick$gameId[i], pick$playId[i], f)
  }
  fwrite(pick[, .(scheme, gameId, playId, runner = ballCarrierDisplayName, design_gap, gap_taken)],
         file.path(OUT_DIR, "animations", "index.csv"))
  cat(sprintf("\n5. Wrote %d animations to %s\n", nrow(pick), file.path(OUT_DIR, "animations")))
}
