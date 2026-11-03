# 05_score_and_compare.R  (plan steps 3.3 parts 5 to 7, and 3.5)
# Score every gap, pick the best gap, and compare designed, best and taken.
#
# Gap score = expected run success from a logistic regression of success on the
# taken gap's width and number of unblocked defenders (plan rule: fit only on
# the gaps the runners took). The best gap is the gap with the highest score.
# Yards left on the table = expected success of the best gap minus expected
# success of the gap taken (in success-probability points).
#
# Output:
#   cache/compare.rds              one row per run
#   output/gap_model.csv           regression coefficients
#   output/comparison_groups.csv   the four designed/best/taken groups
#   output/runner_read_rates.csv   runners with 100+ carries, ranked

source("R/config.R")

MIN_CARRIES <- 100

runs   <- readRDS(file.path(CACHE_DIR, "runs.rds"))[in_analysis == TRUE]
scheme <- readRDS(file.path(CACHE_DIR, "scheme.rds"))
gaps   <- readRDS(file.path(CACHE_DIR, "gaps.rds"))
taken  <- readRDS(file.path(CACHE_DIR, "gap_taken.rds"))

gaps <- merge(gaps, runs[, .(gameId, playId, success, yards)], by = c("gameId", "playId"))
gaps <- merge(gaps, taken[, .(gameId, playId, gap_taken)], by = c("gameId", "playId"))
gaps[, is_taken := gap == gap_taken]

# ---- Gap score model ------------------------------------------------------------

fit_data <- gaps[is_taken == TRUE]
model <- glm(success ~ width + n_unblocked, family = binomial, data = fit_data)
print(summary(model))

auc <- function(p, y) {
  r <- rank(p)
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
fit_auc <- auc(fitted(model), fit_data$success)
cat(sprintf("\nModel fit on %d taken gaps, AUC = %.3f\n", nrow(fit_data), fit_auc))

coefs <- data.table(term = names(coef(model)), estimate = coef(model),
                    std_error = sqrt(diag(vcov(model))))
coefs[, p_value := 2 * pnorm(-abs(estimate / std_error))]
coefs <- rbind(coefs, data.table(term = "AUC", estimate = fit_auc), fill = TRUE)
fwrite(coefs, file.path(OUT_DIR, "gap_model.csv"))

gaps[, score := predict(model, newdata = gaps, type = "response")]

# ---- Best gap -------------------------------------------------------------------

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

# ---- The four groups (runs with a designed gap) -------------------------------------

cmp[!is.na(design_gap), group := fcase(
  design_gap == best_gap & gap_taken == best_gap,   "Worked as designed",
  design_gap != best_gap & gap_taken == best_gap,   "Good read",
  design_gap == best_gap & gap_taken != best_gap,   "Missed the open gap",
  design_gap != best_gap & gap_taken == design_gap, "Followed the call into a closed gap",
  default = "Took a third gap"
)]
groups <- cmp[!is.na(group), .(runs = .N, share = round(.N / sum(!is.na(cmp$group)), 3),
                                success = round(mean(success), 3),
                                mean_yards = round(mean(yards), 2)), by = group][order(-runs)]
cat("\nDesigned / best / taken groups (gap and zone runs):\n"); print(groups)
fwrite(groups, file.path(OUT_DIR, "comparison_groups.csv"))

# ---- Does taking the best gap pay off? -----------------------------------------------

cat("\nOutcome by whether the runner took the best gap (all analysis runs):\n")
print(cmp[!is.na(took_best), .(runs = .N, success = round(mean(success), 3),
                               mean_yards = round(mean(yards), 2)), by = took_best])
cat("\nRead rate by scheme:\n")
print(cmp[, .(runs = .N, read_rate = round(mean(took_best, na.rm = TRUE), 3),
              designed_gap_taken = round(mean(gap_taken == design_gap), 3)), by = scheme])

# ---- Runner read rates ------------------------------------------------------------------

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
