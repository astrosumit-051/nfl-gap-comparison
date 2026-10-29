# Designed gap vs best gap vs gap taken

DSC 355 Project 3 (due 10 Nov 2026). On NFL designed runs from the Big Data Bowl
2024 tracking data (2022 season, weeks 1 to 9), we compare three gaps: the gap the
play was designed for, the best gap at the handoff, and the gap the runner took.

## Setup

1. Install R (4.3 or newer) and RStudio.
2. Get the Big Data Bowl 2024 CSVs (`games.csv`, `plays.csv`, `players.csv`,
   `tracking_2024_week_1.csv` to `_9.csv`). They are not in this repo.
3. Tell the scripts where the CSVs are. The default is Sumit's path in
   `R/config.R`. Everyone else adds one line to `~/.Renviron` and restarts R:

   ```
   NFL_DATA_DIR=/path/to/nfl-big-data-bowl-2024
   ```

4. Open `nfl-gap-comparison.Rproj` and run `source("run_all.R")`.

`00_setup.R` installs missing packages (data.table, ggplot2) and writes compact
copies of the run plays to `cache/` (about 110 MB, about 1 minute). Later runs
reuse the cache. Delete `cache/` to rebuild it.

## Scripts

| Script | Plan step | What it does |
| --- | --- | --- |
| `00_setup.R` | | Packages, data check, keep clean designed runs only |
| `01_prepare.R` | 3.1 | Flip plays, snap and handoff frames, line of scrimmage, linemen, on-line TEs, where the runner crosses the line, side check vs play-by-play |

## Data rules

- Designed run = blank `passResult`; plays nullified by penalty removed (6,227 runs).
- Runs starting inside the opponent's 5 are dropped from the main analysis (323).
- Short yardage (3rd/4th and 2 or less) is kept and flagged (489).
- Designed QB runs have no handoff and stay out of the main analysis (390).
- Run tracking starts 0.5 s before the snap (snap = frame 6), not at the handoff.
