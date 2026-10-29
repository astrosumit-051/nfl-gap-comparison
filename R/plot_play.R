# Play pictures: a still at the handoff (the report's play visualization) and an
# animation for checking scheme labels. Both need the caches from 01 to 05.

suppressPackageStartupMessages(library(ggplot2))

# Colors: reference data-viz palette (categorical slots 1 and 2, blue ramp)
COL_OFFENSE <- "#2a78d6"
COL_DEFENSE <- "#eb6834"
COL_RUNNER  <- "#0b0b0b"
COL_INK     <- "#52514e"
COL_FIELD   <- "#fcfcfb"
COL_GRID    <- "#e4e3df"
SCORE_RAMP  <- c("#cde2fb", "#6da7ec", "#256abf", "#104281")

SCHEME_LABELS <- c(gap = "gap run (pulling lineman)", inside_zone = "inside zone",
                   outside_zone = "outside zone", other = "man / duo / other")

load_play <- function(game_id, play_id) {
  cmp  <- readRDS(file.path(CACHE_DIR, "compare.rds"))[gameId == game_id & playId == play_id]
  runs <- readRDS(file.path(CACHE_DIR, "runs.rds"))[gameId == game_id & playId == play_id]
  gaps <- readRDS(file.path(CACHE_DIR, "gaps_scored.rds"))[gameId == game_id & playId == play_id]
  trk  <- readRDS(file.path(CACHE_DIR, sprintf("tracking_prep_wk%d.rds", runs$week)))
  trk  <- trk[gameId == game_id & playId == play_id]
  list(cmp = cmp, run = runs, gaps = gaps, trk = trk)
}

field_theme <- function() {
  theme_void(base_size = 11) +
    theme(plot.background = element_rect(fill = COL_FIELD, colour = NA),
          plot.title = element_text(colour = "#0b0b0b", face = "bold", margin = margin(b = 4)),
          plot.subtitle = element_text(colour = COL_INK, margin = margin(b = 8)),
          plot.caption = element_text(colour = COL_INK, hjust = 0),
          legend.position = "bottom",
          legend.title = element_text(colour = COL_INK, size = 9),
          legend.text = element_text(colour = COL_INK, size = 8),
          plot.margin = margin(12, 12, 12, 12))
}

# Still picture at the handoff: gaps shaded by score, designed and best gap
# marked, and the runner's path from the handoff to the end of the play.
plot_play_handoff <- function(game_id, play_id) {
  p <- load_play(game_id, play_id)
  run <- p$run; cmp <- p$cmp; g <- p$gaps; trk <- p$trk
  los <- run$los_x
  at <- trk[frameId == run$handoff_frame & role != "football"]
  # Runner's path from the handoff to 1.5 s after he reaches the line
  path_end <- fcoalesce(run$cross_frame, run$contact_frame, run$end_frame) + 15L
  path <- trk[role == "carrier" & frameId >= run$handoff_frame & frameId <= path_end]

  g[, `:=`(ymin = pmin(in_y, out_y), ymax = pmax(in_y, out_y))]

  xlim <- c(los - 10, max(los + 12, max(path$x) + 2))
  near <- at[x >= xlim[1] & x <= xlim[2]]
  ylim <- range(c(g$ymin, g$ymax, path$y, near$y)) + c(-1.5, 1.5)
  ylim <- c(max(ylim[1], 0), min(ylim[2], FIELD_WIDTH))

  # Edge gaps are open-ended: draw them out to the edge of the picture
  g[is_edge == TRUE & side == "L", ymax := ylim[2]]
  g[is_edge == TRUE & side == "R", ymin := ylim[1]]
  # Label gaps tall enough to hold text; dark fills get light text
  g[, label := fifelse(ymax - ymin >= 0.7, gap, "")]
  g[, dark := score >= median(score)]
  design <- g[gap == cmp$design_gap]
  best <- g[gap == cmp$best_gap]

  at[, side := fifelse(role == "defense", "Defense", fifelse(role == "carrier", "Runner", "Offense"))]

  ggplot() +
    geom_vline(xintercept = seq(10, 110, 5), colour = COL_GRID, linewidth = 0.4) +
    geom_vline(xintercept = los, colour = COL_INK, linewidth = 0.6, linetype = "dashed") +
    geom_rect(data = g, aes(xmin = los - 0.8, xmax = los + 0.8, ymin = ymin, ymax = ymax,
                            fill = score), colour = COL_FIELD, linewidth = 0.6) +
    geom_text(data = g[dark == TRUE], aes(x = los, y = (ymin + ymax) / 2, label = label),
              colour = "#ffffff", size = 2.6, fontface = "bold") +
    geom_text(data = g[dark == FALSE], aes(x = los, y = (ymin + ymax) / 2, label = label),
              colour = "#0b0b0b", size = 2.6, fontface = "bold") +
    { if (nrow(design) > 0)
        geom_rect(data = design, aes(xmin = los - 1, xmax = los + 1, ymin = ymin, ymax = ymax),
                  fill = NA, colour = "#0b0b0b", linewidth = 0.7, linetype = "22") } +
    geom_rect(data = best, aes(xmin = los - 1.1, xmax = los + 1.1, ymin = ymin, ymax = ymax),
              fill = NA, colour = "#0b0b0b", linewidth = 1.1) +
    geom_path(data = path, aes(x, y), colour = COL_RUNNER, linewidth = 0.7) +
    geom_point(data = at, aes(x, y, colour = side), size = 3.2, stroke = 0.6) +
    scale_colour_manual(values = c(Offense = COL_OFFENSE, Defense = COL_DEFENSE, Runner = COL_RUNNER),
                        name = NULL) +
    scale_fill_gradientn(colours = SCORE_RAMP, name = "Gap score (expected success)",
                         breaks = scales::breaks_pretty(3),
                         labels = scales::label_percent(accuracy = 1)) +
    annotate("text", x = los + 1.5, y = best$ymax[1] + 0.4, label = "Best gap",
             hjust = 0, size = 3.2, colour = "#0b0b0b", fontface = "bold") +
    { if (nrow(design) > 0 && cmp$design_gap != cmp$best_gap)
        annotate("text", x = los + 1.5, y = design$ymin[1] - 0.4, label = "Designed gap",
                 hjust = 0, size = 3.2, colour = "#0b0b0b") } +
    coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = sprintf("%s, %s", cmp$ballCarrierDisplayName, SCHEME_LABELS[cmp$scheme]),
         subtitle = sprintf("Designed %s, best %s, taken %s. Result: %d yard%s. Offense moves left to right.",
                            fifelse(is.na(cmp$design_gap), "none", cmp$design_gap),
                            cmp$best_gap, cmp$gap_taken, run$yards,
                            fifelse(abs(run$yards) == 1, "", "s")),
         caption = sprintf("Game %s, play %s. Field at the handoff; line is the runner's path after it.",
                           game_id, play_id)) +
    field_theme()
}

# Animation from the snap to the end of the play, for checking scheme labels.
animate_play <- function(game_id, play_id, file, fps = 10) {
  p <- load_play(game_id, play_id)
  run <- p$run; cmp <- p$cmp; trk <- copy(p$trk)
  last <- fcoalesce(run$cross_frame, run$contact_frame, run$end_frame) + 15L
  trk <- trk[frameId >= run$snap_frame - 5 & frameId <= last]
  trk[, side := fcase(role == "football", "Ball", role == "defense", "Defense",
                      role == "carrier", "Runner", role %in% c("OL", "TE_line"), "Line",
                      default = "Offense")]
  los <- run$los_x
  anim <- ggplot(trk, aes(x, y, colour = side, group = nflId)) +
    geom_vline(xintercept = los, colour = COL_INK, linetype = "dashed") +
    geom_point(aes(size = side == "Ball")) +
    scale_size_manual(values = c(`FALSE` = 3, `TRUE` = 1.5), guide = "none") +
    scale_colour_manual(values = c(Line = "#104281", Offense = COL_OFFENSE, Defense = COL_DEFENSE,
                                   Runner = COL_RUNNER, Ball = "#8a5a00"), name = NULL) +
    coord_fixed(xlim = c(los - 12, los + 18), ylim = range(trk$y) + c(-1, 1)) +
    labs(title = sprintf("%s: %s, designed %s, taken %s",
                         cmp$ballCarrierDisplayName, SCHEME_LABELS[cmp$scheme],
                         fifelse(is.na(cmp$design_gap), "none", cmp$design_gap), cmp$gap_taken),
         subtitle = "{round(frame_time, 1)} s after the snap") +
    field_theme() +
    gganimate::transition_time(t)
  n <- uniqueN(trk$frameId)
  # MP4 through the av package is fast; GIF through gifski if the file name asks for it
  renderer <- if (grepl("\\.gif$", file)) gganimate::gifski_renderer() else gganimate::av_renderer()
  gganimate::anim_save(file, gganimate::animate(anim, nframes = n, fps = fps, width = 640,
                                                height = 400, renderer = renderer))
}
