# Designed gap vs best gap vs gap taken

DSC 355 Project 3 (due 10 Nov 2026). On NFL designed runs from the Big Data Bowl
2024 tracking data (2022 season, weeks 1 to 9), we compare three gaps: the gap the
play was designed for, the best gap at the handoff, and the gap the runner took.

## Setup

1. Install R (4.3 or newer) and RStudio.
2. Get the Big Data Bowl 2024 CSVs (`games.csv`, `plays.csv`, `players.csv`,
   `tracking_2024_week_1.csv` to `_9.csv`). They are not in this repo.
3. Tell the code where the CSVs are. The default is Sumit's path, set near the
   top of `nfl_gap_comparison.R`. Everyone else adds one line to `~/.Renviron`
   and restarts R:

   ```
   NFL_DATA_DIR=/path/to/nfl-big-data-bowl-2024
   ```

4. Open `nfl-gap-comparison.Rproj`, open `nfl_gap_comparison.R` and click
   **Source**.

All the code is in that one file. The first run takes about 15 minutes, about
half of it rendering 40 animations. It installs missing packages (data.table, ggplot2,
gganimate, av) and writes compact copies of the run plays to `cache/`. Delete
`cache/` to rebuild it. Three switches at the top of the file (or in
`~/.Renviron`) skip the slow parts:

| Switch | Default | Skips |
| --- | --- | --- |
| `MAKE_ANIMATIONS` | `TRUE` | The 40 scheme animations in step 6 |
| `RUN_STRESS_TESTS` | `TRUE` | Step 8, the stress tests |
| `RUN_VARIANTS` | `TRUE` | Step 8 part 6, the 15 reruns with one setting changed |

## Steps in `nfl_gap_comparison.R`

RStudio's outline (the list icon at the top right of the editor) jumps between
the steps. Each step reads what the earlier steps saved in `cache/`, so after one
full run you can rerun a single step.

| Step | Plan step | What it does |
| --- | --- | --- |
| Settings and helpers | | Data path, switches, gap definitions, the gap and score rules, play pictures |
| 0 | | Packages, data check, keep clean designed runs only |
| 1 | 3.1 | Flip plays, snap and handoff frames, line of scrimmage, linemen, on-line TEs, where the runner crosses the line |
| 2 | 3.2 | Scheme from lineman movement (gap/pull, inside zone, outside zone, other) and the designed gap |
| 3 | 3.3 | At the handoff: every gap's projected width and the unblocked defenders who reach it before the runner |
| 4 | 3.4 | The gap where the runner crosses the line |
| 5 | 3.3, 3.5 | Logistic gap score, best gap, the four designed/best/taken groups, runner read rates |
| 6 | 3.6 | Side check, gap taken vs play-by-play, split-half read rate, chance baseline, scheme animations |
| 7 | | Report figures, including the play visualization |
| 8 | | Stress tests: chance baselines, payoff checks, scheme checks, skill tests, and 15 reruns with one setting changed at a time |

## Outputs (`output/`)

- `fig_play_good_read.png`, `fig_play_followed_call.png`: play pictures at the handoff
- `fig_groups.png`, `fig_best_vs_not.png`, `fig_runner_read_rates.png`: summary charts
- `gap_model.csv`, `comparison_groups.csv`, `runner_read_rates.csv`: result tables
- `validation_*.csv`, `animations/<scheme>/*.mp4`: checks
- `stress_*.csv`: stress tests from step 8

## Data rules and definitions

- Designed run = blank `passResult`; plays nullified by penalty removed (6,227 runs).
- Runs starting inside the opponent's 5 are dropped (323). Short yardage (3rd/4th
  and 2 or less) is kept and flagged (489). Designed QB runs have no handoff and are
  left out (390). 5,563 runs remain.
- Run tracking starts 0.5 s before the snap (snap = frame 6), not at the handoff.
- Success = 40% of yards to go on 1st down, 60% on 2nd, 100% on 3rd and 4th, using
  the yards the run gained before penalties (`prePenaltyPlayResult`).
- Gaps: A (center to guard), B (guard to tackle), C (tackle to end man, or the edge
  if there is no end man), D (outside the end man). L and R are the offense's left
  and right. Gaps are drawn from the line at the handoff; a pulling lineman keeps
  his snap spot. A lineman who has crossed inside his neighbour by then closes
  the gap between them.
