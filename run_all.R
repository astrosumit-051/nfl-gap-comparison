# run_all.R
# Run every step in order from a fresh R session. Open nfl-gap-comparison.Rproj
# first so the working directory is the project root.

scripts <- c(
  "00_setup.R",
  "01_prepare.R"
  # 02_scheme.R, 03_gaps.R, ... are added as they are written
)

for (s in scripts) {
  message("\n==== ", s, " ====")
  source(s, echo = FALSE)
  rm(list = setdiff(ls(), "scripts"))
}
