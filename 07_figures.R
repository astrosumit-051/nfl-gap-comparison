# 07_figures.R
# Figures for the report and slides.
#   fig_play_good_read.png        required play visual (a run where the back beat the call)
#   fig_play_followed_call.png    a run that followed the call into a closed gap
#   fig_groups.png                success rate of the four designed/best/taken groups
#   fig_best_vs_not.png           success when the runner took the best gap, by scheme
#   fig_runner_read_rates.png     read rate of runners with 100+ carries

source("R/config.R")
source("R/plot_play.R")

cmp    <- readRDS(file.path(CACHE_DIR, "compare.rds"))
ranked <- fread(file.path(OUT_DIR, "runner_read_rates.csv"))
gaps   <- readRDS(file.path(CACHE_DIR, "gaps_scored.rds"))

COL_SERIES_1 <- "#2a78d6"
COL_SERIES_2 <- "#eb6834"

chart_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(plot.background = element_rect(fill = COL_FIELD, colour = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = COL_GRID, linewidth = 0.3),
          axis.text = element_text(colour = COL_INK),
          axis.title = element_text(colour = COL_INK, size = 9),
          plot.title = element_text(colour = "#0b0b0b", face = "bold"),
          plot.subtitle = element_text(colour = COL_INK, margin = margin(b = 8)),
          plot.caption = element_text(colour = COL_INK, hjust = 0),
          legend.position = "top", legend.justification = "left",
          legend.text = element_text(colour = COL_INK),
          plot.margin = margin(12, 16, 12, 12))
}
save_fig <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(OUT_DIR, name), p, width = w, height = h, dpi = 200, bg = COL_FIELD)
  message("Wrote ", file.path(OUT_DIR, name))
}

# ---- Play pictures --------------------------------------------------------------

regular <- unique(ranked$ballCarrierId)
pick_play <- function(grp) {
  cand <- cmp[group == grp & taken_source == "crossed_los" & ballCarrierId %in% regular &
                yards >= 6 & yards <= 20 & substr(design_gap, 1, 1) != substr(best_gap, 1, 1)]
  if (nrow(cand) == 0) cand <- cmp[group == grp & taken_source == "crossed_los"]
  cand[order(-abs(best_score - taken_score), gameId, playId)][1]
}
# The report and slides walk through these two plays; search for others only if
# a play no longer lands in its group.
good <- cmp[gameId == 2022091800 & playId == 2172 & group == "Good read"]   # Raheem Mostert
if (nrow(good) == 0) good <- pick_play("Good read")
save_fig(plot_play_handoff(good$gameId, good$playId), "fig_play_good_read.png", 9, 6.5)

closed <- cmp[gameId == 2022092900 & playId == 1627 &
                group == "Followed the call into a closed gap"]               # Joe Mixon
if (nrow(closed) == 0)
  closed <- cmp[group == "Followed the call into a closed gap" & taken_source == "crossed_los" &
                  ballCarrierId %in% regular & yards <= 1][order(-left_on_table, gameId, playId)][1]
save_fig(plot_play_handoff(closed$gameId, closed$playId), "fig_play_followed_call.png", 9, 6.5)

# ---- The four groups -----------------------------------------------------------------

grp <- cmp[!is.na(group), .(runs = .N, success = mean(success)), by = group]
grp[, group := factor(group, levels = grp[order(success), group])]
overall <- mean(cmp[!is.na(group), success])
p <- ggplot(grp, aes(success, group)) +
  geom_col(fill = COL_SERIES_1, width = 0.6) +
  geom_vline(xintercept = overall, colour = COL_INK, linetype = "dashed", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.0f%%  (%d runs)", 100 * success, runs)),
            hjust = -0.08, colour = COL_INK, size = 3.3) +
  annotate("text", x = overall, y = nrow(grp) + 0.55,
           label = sprintf("All gap and zone runs: %.0f%%", 100 * overall),
           hjust = -0.05, colour = COL_INK, size = 3) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0, 0.75),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Runs succeed most when the back leaves the call for the best gap",
       subtitle = "Success rate by how the designed, best and taken gaps line up (gap and zone runs)",
       x = "Run success rate", y = NULL,
       caption = "Success: 40% of yards to go on 1st down, 60% on 2nd, all of it on 3rd and 4th.") +
  chart_theme()
save_fig(p, "fig_groups.png", 8.5, 4.5)

# ---- Took the best gap vs not, by scheme ---------------------------------------------------

sch_lab <- c(gap = "Gap (pull)", inside_zone = "Inside zone", outside_zone = "Outside zone",
             other = "Man / duo / other")
bv <- cmp[!is.na(took_best), .(success = mean(success), runs = .N), by = .(scheme, took_best)]
bv[, scheme := factor(sch_lab[scheme], levels = rev(sch_lab))]
# Second level is drawn on top within each scheme
bv[, choice := factor(fifelse(took_best, "Took the best gap", "Took another gap"),
                      levels = c("Took another gap", "Took the best gap"))]
p <- ggplot(bv, aes(success, scheme, fill = choice)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.66, colour = COL_FIELD, linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f%%", 100 * success)), position = position_dodge(width = 0.72),
            hjust = -0.15, colour = COL_INK, size = 3.1) +
  scale_fill_manual(values = c(`Took the best gap` = COL_SERIES_1, `Took another gap` = COL_SERIES_2),
                    breaks = c("Took the best gap", "Took another gap"), name = NULL) +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0, 0.7),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "The best gap pays off on inside zone and man runs, not on gap or outside zone",
       subtitle = "Run success rate by scheme and by whether the runner took the best gap",
       x = "Run success rate", y = NULL) +
  chart_theme()
save_fig(p, "fig_best_vs_not.png", 8.5, 4.5)

# ---- Runner read rates ------------------------------------------------------------------------

# Chance line: a runner blind to the defense but going the same way (06_validate.R)
chance <- fread(file.path(OUT_DIR, "validation_baseline.csv"))$blind_same_way
league <- mean(cmp$took_best, na.rm = TRUE)
ranked[, runner := factor(runner, levels = ranked[order(read_rate), runner])]
p <- ggplot(ranked, aes(read_rate, runner)) +
  geom_vline(xintercept = chance, colour = COL_INK, linetype = "dotted", linewidth = 0.4) +
  geom_vline(xintercept = league, colour = COL_INK, linetype = "dashed", linewidth = 0.4) +
  geom_segment(aes(x = chance, xend = read_rate, yend = runner), colour = COL_GRID, linewidth = 0.6) +
  geom_point(colour = COL_SERIES_1, size = 2.8) +
  annotate("text", x = chance, y = 0.4, label = sprintf("Blind pick, same way %.0f%%", 100 * chance),
           hjust = -0.05, colour = COL_INK, size = 3) +
  annotate("text", x = league, y = 0.4, label = sprintf("All runs %.0f%%", 100 * league),
           hjust = -0.05, colour = COL_INK, size = 3) +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0, 0.5)) +
  coord_cartesian(clip = "off") +
  labs(title = "Read rate of runners with 100+ carries",
       subtitle = "Share of runs where the runner took the highest-scoring gap at the handoff",
       x = "Read rate", y = NULL,
       caption = "Weeks 1 to 9, 2022. Runners differ only a little beyond chance (08_stress_tests.R), so read the order loosely.") +
  chart_theme()
save_fig(p, "fig_runner_read_rates.png", 8.5, 6)
