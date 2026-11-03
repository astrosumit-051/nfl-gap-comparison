# run_all.R
# Run every step in order from a fresh R session. Open nfl-gap-comparison.Rproj
# first so the working directory is the project root.

scripts <- c(
  "00_setup.R",             # packages, data check, cache the clean designed runs
  "01_prepare.R",           # 3.1 flip, key frames, line of scrimmage, linemen
  "02_scheme.R",            # 3.2 scheme and designed gap
  "03_gaps.R",              # 3.3 gap width and unblocked defenders at the handoff
  "04_gap_taken.R",         # 3.4 gap taken
  "05_score_and_compare.R", # 3.3/3.5 gap score, best gap, groups, read rates
  "06_validate.R",          # 3.6 checks and scheme animations
  "07_figures.R"            # report figures
)

for (s in scripts) {
  message("\n==== ", s, " ====")
  source(s, echo = FALSE)
  rm(list = setdiff(ls(), "scripts"))
}

# Optional: source("08_stress_tests.R") for the stress tests (a few minutes)
