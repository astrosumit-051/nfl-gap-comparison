# 08_stress_tests.R
# Stress tests of the assumptions and results in scripts 01 to 07. Run it after
# run_all.R. It is not part of run_all.R because the parameter variants take
# a few minutes (set RUN_VARIANTS=FALSE to skip them).
#
#   1. Data checks        penalty yards, carriers, handoff fallbacks
#   2. Chance baselines   how often a runner blind to the defense would hit the
#                         best gap, keeping his scheme, gap set and direction
#   3. Payoff checks      controls, clustered bootstrap, subsets, cross-fitting
#   4. Scheme checks      gap taken vs the designed side, wider designed gaps,
#                         zone split point
#   5. Skill checks       do runners differ in read rate beyond chance?
#   6. Variants           rerun 03 and 05 with one setting changed at a time
#
# Output: output/stress_*.csv

source("R/config.R")

RUN_VARIANTS <- as.logical(Sys.getenv("RUN_VARIANTS", "TRUE"))
N_PERM       <- 200   # permutations for each blind-runner baseline
N_BOOT       <- 1000  # bootstrap draws
options(width = 160)

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

# ---- 1. Data checks -----------------------------------------------------------------------

cat("1. Data checks\n")
cat(sprintf("   Runs where penalty yards change the result: %d (success differs on %d)\n",
            a[playResult != yards, .N],
            a[playResult != yards & run_success(down, yardsToGo, playResult) != success, .N]))
cat("   Carriers:", paste(a[, .N, by = carrier_pos][order(-N), sprintf("%s %d", carrier_pos, N)], collapse = ", "), "\n")
cat("   Handoff source:", paste(a[, .N, by = handoff_source][, sprintf("%s %d", handoff_source, N)], collapse = ", "), "\n")

# ---- 2. Chance baselines ------------------------------------------------------------------------

cat("\n2. How often would a runner blind to the defense hit the best gap?\n")
base_rows <- list(
  data.table(baseline = "Uniform pick among the play's gaps", mean = mean(1 / a$n_gaps), lo = NA, hi = NA),
  data.table(baseline = "Blind, same scheme and gap set", t(blind(a, c("scheme", "gapset")))),
  data.table(baseline = "Blind, same scheme, gap set and heading side", t(blind(a, c("scheme", "gapset", "head_side"))))
)
baselines <- rbindlist(base_rows)
baselines <- rbind(data.table(baseline = "Runners (read rate)", mean = mean(a$took_best), lo = NA, hi = NA), baselines)
print(baselines[, lapply(.SD, function(v) if (is.double(v)) round(v, 3) else v)])

by_scheme <- a[, {
  b <- blind(.SD, c("gapset", "head_side"))
  .(runs = .N, read_rate = mean(took_best), blind_same_heading = b[["mean"]], blind_hi = b[["hi"]])
}, by = scheme]
cat("   By scheme:\n"); print(by_scheme[, lapply(.SD, function(v) if (is.double(v)) round(v, 3) else v)])

# How clearly the best gap stands out: score margin over the second-best gap
margin <- gaps[order(-score), .(margin = score[1] - score[2]), by = .(gameId, playId)]
a <- merge(a, margin, by = c("gameId", "playId"))
a[, margin_bin := cut(margin, c(-Inf, 0.01, 0.03, 0.06, Inf),
                      labels = c("under 1 pt", "1 to 3 pts", "3 to 6 pts", "over 6 pts"))]
by_margin <- a[, .(runs = .N, read_rate = mean(took_best),
                   success_best = mean(success[took_best]), success_other = mean(success[!took_best])),
               by = margin_bin][order(margin_bin)]
cat("   Read rate by how far the best gap's score beats the next gap:\n")
print(by_margin[, lapply(.SD, function(v) if (is.double(v)) round(v, 3) else v)])

fwrite(rbind(baselines[, .(group = "all", baseline, mean, lo, hi)],
             by_scheme[, .(group = scheme, baseline = "Blind, same gap set and heading side",
                           mean = blind_same_heading, lo = NA, hi = blind_hi)]),
       file.path(OUT_DIR, "stress_baselines.csv"))
fwrite(by_margin, file.path(OUT_DIR, "stress_margin.csv"))

# ---- 3. Payoff checks ----------------------------------------------------------------------------

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

# ---- 4. Scheme checks ------------------------------------------------------------------------------

cat("\n4. Scheme checks\n")
d <- a[!is.na(design_side)]
d[, where := paste(fifelse(substr(gap_taken, 1, 1) == design_side, "play side", "back side"),
                   substr(gap_taken, 2, 2))]
where <- dcast(d[, .N, by = .(scheme, where)][, share := N / sum(N), by = scheme],
               scheme ~ where, value.var = "share", fill = 0)
cat("   Gap taken relative to the designed side (share of runs):\n")
print(where[, lapply(.SD, function(v) if (is.double(v)) round(v, 3) else v)])

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
print(groups_wide[order(-success_main), lapply(.SD, function(v) if (is.double(v)) round(v, 3) else v)])

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
closed <- gaps[is_taken == TRUE, .(runs = .N, success = mean(success)),
               by = .(closed_at_handoff = width == 0, unblocked = fifelse(n_unblocked <= 1, "0 or 1", "2 or more"))]
cat("   Success through the gap taken, by width at the handoff and free defenders:\n")
print(closed[order(closed_at_handoff, unblocked), lapply(.SD, function(v) if (is.double(v)) round(v, 3) else v)])
fwrite(groups_wide, file.path(OUT_DIR, "stress_groups.csv"))
fwrite(zone_cut, file.path(OUT_DIR, "stress_zone_split.csv"))

# ---- 5. Skill checks -----------------------------------------------------------------------------------

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

# ---- 6. Variants: one setting changed at a time --------------------------------------------------------

headline <- function(cmp_v, gaps_v) {
  d <- add_keys(cmp_v[!is.na(took_best)])
  half <- dcast(d[, .(n = .N, r = mean(took_best)), by = .(ballCarrierId, h = week <= 4)],
                ballCarrierId ~ h, value.var = c("n", "r"))[n_TRUE >= 40 & n_FALSE >= 40]
  sd <- function(s) d[scheme == s, diff_success(.SD)]
  data.table(read_rate = mean(d$took_best), blind_same_heading = blind(d, c("scheme", "gapset", "head_side"), 50)[["mean"]],
             success_best = mean(d$success[d$took_best]), success_other = mean(d$success[!d$took_best]),
             diff = diff_success(d), p = p_success(d),
             inside_zone = sd("inside_zone"), other = sd("other"), gap = sd("gap"), outside_zone = sd("outside_zone"),
             good_read = d[group == "Good read", mean(success)], worked_as_designed = d[group == "Worked as designed", mean(success)],
             good_read_is_top_group = d[!is.na(group), .(s = mean(success)), by = group][which.max(s), group] == "Good read",
             split_half_r = cor(half$r_TRUE, half$r_FALSE))
}

set_const <- function(txt, name, value) {
  out <- sub(sprintf("^%s( +)<- [0-9.]+", name), sprintf("%s\\1<- %s", name, value), txt)
  stopifnot(!identical(out, txt)); out
}
swap <- function(txt, old, new) {
  out <- gsub(old, new, txt, fixed = TRUE)
  stopifnot(!identical(out, txt)); out
}
RUNNER_V0 <- "(rvx * (gx - rx) + rvy * (gy - ry)) / pmax(run_dist, 1e-6)"
MODEL     <- "success ~ width + n_unblocked"
variants <- list(
  "Runner speed toward the gap ignored (v0 = 0)" = list(g = function(t) swap(t, RUNNER_V0, "0")),
  "Runner speed counted in every direction"      = list(g = function(t) swap(t, RUNNER_V0, "sqrt(rvx^2 + rvy^2)")),
  "Acceleration 6 yd/s2"                         = list(g = function(t) set_const(t, "ACCEL", 6)),
  "Acceleration 8 yd/s2"                         = list(g = function(t) set_const(t, "ACCEL", 8)),
  "Top speed 7 yd/s"                             = list(g = function(t) set_const(t, "TOP_SPEED", 7)),
  "No projection of the edges"                   = list(g = function(t) set_const(t, "MAX_PROJECTION", 0)),
  "Projection up to 1.0 s"                       = list(g = function(t) set_const(t, "MAX_PROJECTION", 1.0)),
  "Block radius 1.0 yd"                          = list(g = function(t) set_const(t, "BLOCK_RADIUS", 1.0)),
  "Block radius 2.0 yd"                          = list(g = function(t) set_const(t, "BLOCK_RADIUS", 2.0)),
  "No 1-yard spacer"                             = list(g = function(t) set_const(t, "SPACER", 0)),
  "Edge gap cap 3 yd"                            = list(g = function(t) set_const(t, "EDGE_CAP", 3)),
  "Edge gap cap 8 yd"                            = list(g = function(t) set_const(t, "EDGE_CAP", 8)),
  "Snapshot 0.5 s after the handoff"             = list(g = function(t) swap(t, "frameId = handoff_frame)]", "frameId = handoff_frame + 5L)]")),
  "Score counts all defenders, blocked or not"   = list(s = function(t) swap(t, MODEL, "success ~ width + n_first")),
  "Score uses unblocked defenders only"          = list(s = function(t) swap(t, MODEL, "success ~ n_unblocked"))
)

run_variant <- function(name) {
  v <- variants[[name]]
  dir <- file.path(tempdir(), paste0("variant_", gsub("[^A-Za-z0-9]+", "_", name)))
  unlink(dir, recursive = TRUE)
  dir.create(file.path(dir, "cache"), recursive = TRUE); dir.create(file.path(dir, "output"))
  link <- function(from, to) if (!file.symlink(normalizePath(from), to)) file.copy(from, to)
  link("R", file.path(dir, "R"))
  inputs <- c("runs.rds", "linemen.rds", "scheme.rds", "gap_taken.rds",
              sprintf("tracking_prep_wk%d.rds", WEEKS))
  for (f in inputs) link(file.path(CACHE_DIR, f), file.path(dir, "cache", f))
  g <- readLines("03_gaps.R"); s <- readLines("05_score_and_compare.R")
  if (!is.null(v$g)) g <- v$g(g)
  if (!is.null(v$s)) s <- v$s(s)
  writeLines(g, file.path(dir, "03_gaps.R")); writeLines(s, file.path(dir, "05_score_and_compare.R"))
  rscript <- file.path(R.home("bin"), "Rscript")
  owd <- setwd(dir); on.exit(setwd(owd))
  for (f in c("03_gaps.R", "05_score_and_compare.R")) {
    if (system2(rscript, f, stdout = FALSE, stderr = FALSE) != 0) stop("variant failed: ", name)
  }
  setwd(owd)
  cbind(variant = name, headline(readRDS(file.path(dir, "cache", "compare.rds")),
                                 readRDS(file.path(dir, "cache", "gaps_scored.rds"))))
}

if (RUN_VARIANTS) {
  cat("\n6. Variants (one setting changed at a time; a few minutes)\n")
  cores <- max(1L, min(4L, parallel::detectCores() - 1L))
  res <- if (.Platform$OS.type == "unix") parallel::mclapply(names(variants), run_variant, mc.cores = cores)
         else lapply(names(variants), run_variant)
  var_tab <- rbind(cbind(variant = "Main analysis", headline(cmp, gaps)), rbindlist(res))
  print(fmt(var_tab[, .(variant, read_rate, blind_same_heading, success_best, success_other, diff,
                        inside_zone, gap, split_half_r)]))
  fwrite(var_tab, file.path(OUT_DIR, "stress_variants.csv"))
}
