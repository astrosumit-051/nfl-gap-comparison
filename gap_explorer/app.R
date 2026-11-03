# gap_explorer/app.R
# Gap explorer: a Shiny app for the designed gap vs best gap vs gap taken.
# DSC 355 Project 3.
#
# Three tabs:
#   Film room            Filter the runs, pick one, and play it frame by frame.
#                        A gap meter lists every gap's score, with the designed,
#                        best and taken gaps tagged.
#   Be the running back  A round of 10 plays. Each play stops at the handoff; you
#                        pick a gap, see the answer, then watch the play. Your
#                        score is kept next to the real backs'.
#   Gap guide            What LA, RB, RC, LD... mean, and the five groups.
#
# How to run: source nfl_gap_comparison.R once (MAKE_ANIMATIONS = FALSE is fine),
# so cache/ holds compare.rds, gaps_scored.rds, runs.rds and tracking_prep_wkN.rds.
# Then open this file in RStudio and click Run App, or from the repo folder run
#   shiny::runApp("gap_explorer")
# The app reads ../cache by default. Set GAP_CACHE_DIR to use another folder.

needed <- c("shiny", "bslib", "data.table", "ggplot2", "scales")
missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(data.table)
  library(ggplot2)
})

# ==== DATA =========================================================================

find_cache <- function() {
  env <- Sys.getenv("GAP_CACHE_DIR", "")
  for (d in c(env, "../cache", "cache")) {
    if (nzchar(d) && file.exists(file.path(d, "compare.rds"))) return(normalizePath(d))
  }
  stop("No cache/compare.rds found. Source nfl_gap_comparison.R once first, ",
       "or set GAP_CACHE_DIR to the folder that holds the cache.")
}
CACHE_DIR <- find_cache()

cmp  <- readRDS(file.path(CACHE_DIR, "compare.rds"))
gaps <- readRDS(file.path(CACHE_DIR, "gaps_scored.rds"))
runs <- readRDS(file.path(CACHE_DIR, "runs.rds"))[in_analysis == TRUE]

play_cols <- intersect(c("gameId", "playId", "snap_frame", "handoff_frame", "cross_frame",
                         "contact_frame", "end_frame", "los_x", "possessionTeam",
                         "defensiveTeam", "quarter", "down", "yardsToGo", "playDescription"),
                       names(runs))
cmp <- merge(cmp, runs[, ..play_cols], by = c("gameId", "playId"))
cmp[, key := paste(gameId, playId, sep = "_")]
cmp[is.na(group), group := "No designed gap"]
gaps[, key := paste(gameId, playId, sep = "_")]
setkey(gaps, key)
setkey(cmp, key)

FPS <- 10
FIELD_WIDTH <- 160 / 3

GROUPS <- c("Good read", "Worked as designed", "Followed the call into a closed gap",
            "Took a third gap", "Missed the open gap", "No designed gap")
GROUP_MEANING <- c(
  "Good read" = "The called gap was not the best gap. The back left it and took the best gap.",
  "Worked as designed" = "The called gap was the best gap, and the back took it.",
  "Followed the call into a closed gap" = "The called gap was not the best gap, and the back took it anyway.",
  "Took a third gap" = "The called gap was not the best gap, and the back took neither the called gap nor the best gap.",
  "Missed the open gap" = "The called gap was the best gap, but the back went somewhere else.",
  "No designed gap" = "Man, duo or other blocking: there is no designed gap, so the run is in no group."
)
SCHEME_LABELS <- c(gap = "Gap run (pulling lineman)", inside_zone = "Inside zone",
                   outside_zone = "Outside zone", other = "Man / duo / other")

# Colors match the figures in nfl_gap_comparison.R
COL_OFFENSE <- "#2a78d6"
COL_DEFENSE <- "#eb6834"
COL_RUNNER  <- "#0b0b0b"
COL_BALL    <- "#8a5a00"
COL_INK     <- "#52514e"
COL_FIELD   <- "#fcfcfb"
COL_GRID    <- "#e4e3df"
COL_PICK    <- "#7b3fbf"
SCORE_RAMP  <- c("#cde2fb", "#6da7ec", "#256abf", "#104281")

# Tracking: one week at a time, trimmed to the columns and frames the app draws.
# The last three weeks used stay in memory.
trk_store <- new.env()
trk_order <- character(0)
week_tracking <- function(wk) {
  id <- as.character(wk)
  if (!exists(id, envir = trk_store, inherits = FALSE)) {
    trk <- readRDS(file.path(CACHE_DIR, sprintf("tracking_prep_wk%d.rds", wk)))
    win <- cmp[week == wk, .(gameId, playId,
                             first = snap_frame - 5L,
                             last = fcoalesce(cross_frame, contact_frame, end_frame) + 15L)]
    trk <- merge(trk[, .(gameId, playId, nflId, frameId, role, x, y, vx, vy)], win,
                 by = c("gameId", "playId"))
    trk <- trk[frameId >= first & frameId <= last][, c("first", "last") := NULL]
    trk[, key := paste(gameId, playId, sep = "_")]
    setkey(trk, key, frameId)
    assign(id, trk, envir = trk_store)
    trk_order <<- c(trk_order, id)
    if (length(trk_order) > 3) {
      rm(list = trk_order[1], envir = trk_store)
      trk_order <<- trk_order[-1]
      invisible(gc())
    }
  }
  get(id, envir = trk_store)
}


# Everything needed to draw one run
load_play <- function(k) {
  p <- cmp[k]
  trk <- week_tracking(p$week)[.(k)]
  g <- copy(gaps[.(k)])
  g[, `:=`(ymin = pmin(in_y, out_y), ymax = pmax(in_y, out_y))]
  path <- trk[role == "carrier"]
  at_ho <- trk[frameId == p$handoff_frame & role != "football"]

  # One fixed window per play, so the picture does not jump while it plays
  los <- p$los_x
  xlim <- c(los - 8, max(los + 10, min(max(path$x) + 2, los + 16)))
  near <- at_ho[x >= xlim[1] & x <= xlim[2]]
  ylim <- range(c(g$ymin, g$ymax, path[x <= xlim[2]]$y, near$y)) + c(-1.5, 1.5)
  ylim <- c(max(ylim[1], 0), min(ylim[2], FIELD_WIDTH))

  list(p = p, trk = trk, g = g, path = path, xlim = xlim, ylim = ylim,
       first = min(trk$frameId), last = max(trk$frameId))
}

fmt_pct <- function(x) ifelse(is.na(x), "-", sprintf("%.0f%%", 100 * x))
GAP_LETTERS <- c("A", "B", "C", "D")

# Gaps in screen order, left to right as seen from behind the offense:
# LD, LC, LB, LA, RA, RB, RC, RD
gap_order <- function(g) {
  g[order(side, fifelse(side == "L", -match(letter, GAP_LETTERS), match(letter, GAP_LETTERS)))]
}

# Plain words for a gap, e.g. "RB: right side, between the guard and the tackle"
gap_words <- function(g) {
  has_d <- g[letter == "D", unique(side)]
  side_word <- fifelse(g$side == "L", "left", "right")
  where <- fcase(
    g$letter == "A", "between the center and the guard",
    g$letter == "B", "between the guard and the tackle",
    g$letter == "C" & g$side %in% has_d, "between the tackle and the tight end",
    g$letter == "C", "outside the tackle (the edge)",
    default = "outside the tight end (the edge)")
  sprintf("%s side, %s", side_word, where)
}

# ==== FIELD PICTURE ================================================================
#
# Drawn from behind the offense: the offense moves up the screen and the
# offense's left is on the left. Screen x = FIELD_WIDTH - y, screen y = x.
#
# show: which marks to draw. "scores" shades the gaps by their score; "design",
# "best", "taken", "pick" outline those gaps; "path" draws the runner's path up
# to the frame; "arrows" shows where each player is heading over the next 0.5 s.
# The legend is HTML next to the plot, not part of it.

to_screen <- function(y) FIELD_WIDTH - y

draw_field <- function(pl, frame, show, pick = NA_character_) {
  p <- pl$p; los <- p$los_x
  g <- copy(pl$g)
  g[, `:=`(sx0 = to_screen(ymax), sx1 = to_screen(ymin))]
  g[, mid := (sx0 + sx1) / 2]
  g[, label := fifelse(sx1 - sx0 >= 1.1, gap, "")]
  at <- pl$trk[frameId == frame]
  at[, who := fcase(role == "football", "Ball", role == "defense", "Defense",
                    role == "carrier", "Runner", default = "Offense")]
  at[, `:=`(sx = to_screen(y), sy = x, dsx = -vy * 0.5, dsy = vx * 0.5)]
  players <- at[who != "Ball"]
  ball <- at[who == "Ball"]

  box <- function(gap_name, pad, ...) {
    geom_rect(data = g[gap == gap_name],
              aes(xmin = sx0, xmax = sx1, ymin = los - pad, ymax = los + pad), fill = NA, ...)
  }

  plt <- ggplot() +
    geom_hline(yintercept = seq(10, 110, 5), colour = COL_GRID, linewidth = 0.4) +
    geom_hline(yintercept = los, colour = COL_INK, linewidth = 0.5, linetype = "dashed")

  if ("scores" %in% show) {
    g[, dark := score >= median(score)]
    plt <- plt +
      geom_rect(data = g, aes(xmin = sx0, xmax = sx1, ymin = los - 0.7, ymax = los + 0.7, fill = score),
                colour = COL_FIELD, linewidth = 0.5) +
      geom_text(data = g[dark == TRUE], aes(x = mid, y = los, label = label),
                colour = "#ffffff", size = 2.4, fontface = "bold") +
      geom_text(data = g[dark == FALSE], aes(x = mid, y = los, label = label),
                colour = "#0b0b0b", size = 2.4, fontface = "bold") +
      scale_fill_gradientn(colours = SCORE_RAMP, limits = range(gaps$score), guide = "none")
  } else {
    plt <- plt +
      geom_rect(data = g, aes(xmin = sx0, xmax = sx1, ymin = los - 0.7, ymax = los + 0.7),
                fill = "#f1f0ec", colour = "#b9b8b3", linewidth = 0.4) +
      geom_text(data = g, aes(x = mid, y = los, label = label),
                colour = "#0b0b0b", size = 2.4, fontface = "bold")
  }

  if ("design" %in% show && !is.na(p$design_gap))
    plt <- plt + box(p$design_gap, 0.9, colour = "#0b0b0b", linewidth = 0.6, linetype = "22")
  if ("best" %in% show)  plt <- plt + box(p$best_gap, 1.05, colour = "#0b0b0b", linewidth = 1)
  if ("taken" %in% show) plt <- plt + box(p$gap_taken, 1.2, colour = COL_DEFENSE, linewidth = 0.9)
  if ("pick" %in% show && !is.na(pick))
    plt <- plt + box(pick, 1.35, colour = COL_PICK, linewidth = 0.9, linetype = "41")

  if ("path" %in% show) {
    path <- pl$path[frameId >= p$handoff_frame & frameId <= frame]
    if (nrow(path) > 1)
      plt <- plt + geom_path(data = path, aes(to_screen(y), x), colour = COL_RUNNER, linewidth = 0.6)
  }
  if ("arrows" %in% show)
    plt <- plt + geom_segment(data = players, aes(x = sx, y = sy, xend = sx + dsx, yend = sy + dsy, colour = who),
                              linewidth = 0.4, arrow = arrow(length = unit(0.1, "cm")))

  plt +
    geom_point(data = players, aes(sx, sy, colour = who, size = who == "Runner"), stroke = 0.5) +
    { if (nrow(ball) > 0) geom_point(data = ball, aes(sx, sy), colour = COL_BALL, size = 1.2) } +
    scale_size_manual(values = c(`FALSE` = 2.4, `TRUE` = 3.4), guide = "none") +
    scale_colour_manual(values = c(Offense = COL_OFFENSE, Defense = COL_DEFENSE, Runner = COL_RUNNER),
                        guide = "none") +
    coord_fixed(xlim = rev(to_screen(pl$ylim)), ylim = pl$xlim, expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = COL_FIELD, colour = NA),
          plot.margin = margin(4, 4, 4, 4))
}

# ==== HTML PIECES ====================================================================

dot <- function(col, label) span(class = "lg", span(class = "lg-dot", style = paste0("background:", col)), label)
box_key <- function(cls, label) span(class = "lg", span(class = paste("lg-box", cls)), label)

field_legend <- function(scores = TRUE, pick = FALSE) {
  div(class = "field-legend",
      dot(COL_OFFENSE, "Offense"), dot(COL_DEFENSE, "Defense"), dot(COL_RUNNER, "Runner"),
      if (scores) span(class = "lg", span(class = "lg-ramp"), "Gap score, low to high"),
      if (scores) box_key("k-design", "Designed"),
      if (scores) box_key("k-best", "Best"),
      if (scores) box_key("k-taken", "Taken"),
      if (pick) box_key("k-pick", "Your pick"))
}

tag_chip <- function(cls, label) span(class = paste("chip", cls), label)

# One row per gap: name, plain words, score bar, tags. The bar runs from the
# lowest to the highest score in the data so differences are visible.
gap_meter <- function(pl, pick = NA_character_, reveal = TRUE, hint = FALSE) {
  g <- gap_order(copy(pl$g)); p <- pl$p
  g[, words := gap_words(g)]
  lo <- min(gaps$score); hi <- max(gaps$score)
  rows <- lapply(seq_len(nrow(g)), function(i) {
    r <- g[i]
    tags_i <- tagList(
      if (!is.na(pick) && r$gap == pick) tag_chip("c-pick", "Your pick"),
      if (reveal && !is.na(p$design_gap) && r$gap == p$design_gap) tag_chip("c-design", "Designed"),
      if (reveal && r$gap == p$best_gap) tag_chip("c-best", "Best"),
      if (reveal && r$gap == p$gap_taken) tag_chip("c-taken", "Taken"))
    detail <- sprintf("%.1f yd wide, %d unblocked defender%s, runner there in %.1f s",
                      r$width, r$n_unblocked, if (r$n_unblocked == 1) "" else "s", r$t_runner)
    div(class = paste("gm-row", if (reveal && r$gap == p$best_gap) "is-best"),
        div(class = "gm-name", r$gap),
        div(class = "gm-main",
            div(class = "gm-words", r$words, tags_i),
            if (reveal) div(class = "gm-bar",
                            div(class = "gm-fill", style = sprintf("width:%.0f%%", 100 * (r$score - lo) / (hi - lo))),
                            span(class = "gm-val", fmt_pct(r$score))),
            if (reveal || hint) div(class = "gm-detail", detail)))
  })
  div(class = "gap-meter", rows)
}

tile <- function(label, value, sub = NULL, cls = NULL) {
  div(class = paste("tile", cls), div(class = "tile-label", label), div(class = "tile-value", value),
      if (!is.null(sub)) div(class = "tile-sub", sub))
}

play_label <- function(d) {
  sprintf("Wk %d · %s vs %s · %s · %s · %d yds", d$week, d$possessionTeam, d$defensiveTeam,
          d$ballCarrierDisplayName, SCHEME_LABELS[d$scheme], d$yards)
}
down_text <- function(p) sprintf("%s and %d", c("1st", "2nd", "3rd", "4th")[p$down], p$yardsToGo)

# Schematic of the line for the gap guide: TE T G C G T TE with the gaps between
gap_guide_plot <- function() {
  pos <- data.table(x = c(-4.5, -3, -1.5, 0, 1.5, 3, 4.5),
                    lab = c("TE", "T", "G", "C", "G", "T", "TE"))
  gp <- data.table(x = c(-5.6, -3.75, -2.25, -0.75, 0.75, 2.25, 3.75, 5.6),
                   lab = c("LD", "LC", "LB", "LA", "RA", "RB", "RC", "RD"))
  ggplot() +
    geom_rect(data = gp, aes(xmin = x - 0.6, xmax = x + 0.6, ymin = 0.9, ymax = 1.7), fill = "#cde2fb") +
    geom_text(data = gp, aes(x, 1.3, label = lab), fontface = "bold", size = 3.6, colour = "#104281") +
    geom_point(data = pos, aes(x, 0), size = 9, colour = COL_OFFENSE) +
    geom_text(data = pos, aes(x, 0, label = lab), colour = "#ffffff", fontface = "bold", size = 3) +
    annotate("text", x = 0, y = -0.9, label = "Seen from behind the offense: the offense's left is on the left",
             size = 3.4, colour = COL_INK) +
    annotate("segment", x = 6.6, xend = 6.6, y = -0.4, yend = 1.6, colour = COL_INK,
             arrow = arrow(length = unit(0.2, "cm"))) +
    annotate("text", x = 6.75, y = 0.6, label = "offense\nmoves up", hjust = 0, size = 3, colour = COL_INK) +
    coord_cartesian(xlim = c(-6.4, 7.8), ylim = c(-1.2, 2)) +
    theme_void() +
    theme(plot.background = element_rect(fill = COL_FIELD, colour = NA))
}

# ==== UI ============================================================================

theme_app <- bs_theme(version = 5, bg = "#fcfcfb", fg = "#1b1b1a", primary = "#256abf",
                      font_scale = 0.88,
                      base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif"))

app_css <- tags$style(HTML(sprintf("
  .card-header { font-weight: 600; }
  .recalculating { opacity: 1 !important; }
  .tiles { display: grid; grid-template-columns: repeat(auto-fit, minmax(130px, 1fr)); gap: 10px; margin-bottom: 12px; }
  .tile { background: #fff; border: 1px solid #e4e3df; border-radius: 10px; padding: 10px 12px; }
  .tile-label { font-size: .72rem; text-transform: uppercase; letter-spacing: .04em; color: #6b6a66; }
  .tile-value { font-size: 1.35rem; font-weight: 700; line-height: 1.2; }
  .tile-sub { font-size: .78rem; color: #6b6a66; }
  .tile.t-group { grid-column: span 2; }
  .tile.t-group .tile-value { font-size: 1.05rem; }
  .field-legend { display: flex; flex-wrap: wrap; gap: 6px 14px; font-size: .78rem; color: #52514e; margin: 6px 0 2px; }
  .lg { display: inline-flex; align-items: center; gap: 5px; }
  .lg-dot { width: 9px; height: 9px; border-radius: 50%%; display: inline-block; }
  .lg-ramp { width: 56px; height: 9px; border-radius: 2px; display: inline-block; background: linear-gradient(90deg, %s); }
  .lg-box { width: 16px; height: 10px; display: inline-block; }
  .k-design { border: 1.5px dashed #0b0b0b; } .k-best { border: 2px solid #0b0b0b; }
  .k-taken { border: 2px solid %s; } .k-pick { border: 2px dashed %s; }
  .gap-meter { display: flex; flex-direction: column; gap: 6px; }
  .gm-row { display: flex; gap: 10px; padding: 6px 8px; border-radius: 8px; border: 1px solid transparent; }
  .gm-row.is-best { border-color: #0b0b0b; background: #f5f8fd; }
  .gm-name { font-weight: 700; width: 30px; flex: none; font-size: .95rem; }
  .gm-main { flex: 1; min-width: 0; }
  .gm-words { font-size: .8rem; color: #3a3936; display: flex; flex-wrap: wrap; gap: 4px; align-items: center; }
  .gm-bar { position: relative; height: 12px; background: #eef2f8; border-radius: 6px; margin-top: 4px; }
  .gm-fill { height: 100%%; background: #256abf; border-radius: 6px; }
  .gm-val { position: absolute; right: 6px; top: -2px; font-size: .72rem; font-weight: 600; }
  .gm-detail { font-size: .72rem; color: #6b6a66; margin-top: 2px; }
  .chip { font-size: .68rem; font-weight: 600; padding: 0 6px; border-radius: 999px; line-height: 1.5; }
  .c-design { border: 1px dashed #0b0b0b; } .c-best { background: #0b0b0b; color: #fff; }
  .c-taken { background: %s; color: #fff; } .c-pick { background: %s; color: #fff; }
  .progress-segs { display: flex; gap: 4px; margin: 2px 0 10px; }
  .progress-segs div { flex: 1; height: 5px; border-radius: 3px; background: #e4e3df; }
  .progress-segs .done-right { background: #2e9e5b; } .progress-segs .done-wrong { background: #d0443e; }
  .progress-segs .now { background: #256abf; }
  .btn.choice { display: flex; align-items: center; gap: 10px; width: 100%%; text-align: left; padding: 7px 10px;
            margin-bottom: 6px; border: 1px solid #d6d5d0; border-radius: 8px; background: #fff; }
  .btn.choice:hover:not([disabled]) { border-color: #256abf; background: #f5f8fd; }
  .btn.choice .gm-name { width: 30px; }
  .btn.choice .c-words { font-size: .8rem; color: #3a3936; flex: 1; }
  .btn.choice .c-hint { font-size: .72rem; color: #6b6a66; }
  .btn.choice.is-right { border: 2px solid #2e9e5b; background: #eef8f1; }
  .btn.choice.is-wrong { border: 2px solid #d0443e; background: #fcefee; }
  .btn.choice[disabled] { opacity: 1; cursor: default; }
  .answer-box { border-radius: 8px; padding: 8px 12px; margin-bottom: 8px; font-size: .88rem; }
  .ab-right { border: 1px solid #2e9e5b; background: #eef8f1; }
  .ab-wrong { border: 1px solid #d0443e; background: #fcefee; }
  .ab-info { border: 1px solid #e4e3df; background: #fff; }
  .ab-label { font-size: .7rem; text-transform: uppercase; letter-spacing: .04em; color: #6b6a66; }
  .muted { color: #6b6a66; font-size: .82rem; }
",
paste(SCORE_RAMP, collapse = ","), COL_DEFENSE, COL_PICK, COL_DEFENSE, COL_PICK)))

runners <- sort(unique(cmp$ballCarrierDisplayName))
teams <- sort(unique(cmp$possessionTeam))
scheme_choices <- setNames(names(SCHEME_LABELS), SCHEME_LABELS)

film_tab <- nav_panel("Film room",
  layout_sidebar(
    sidebar = sidebar(width = 270,
      selectInput("f_group", "Group", c("All runs" = "", GROUPS)),
      selectInput("f_scheme", "Scheme", c("All schemes" = "", scheme_choices)),
      selectizeInput("f_runner", "Runner", c("All runners" = "", runners)),
      selectInput("f_team", "Offense", c("All teams" = "", teams)),
      radioButtons("f_result", "Result", c("All" = "", "Success" = "1", "Failure" = "0"), inline = TRUE),
      checkboxInput("f_regular", "Only backs with 100+ carries", FALSE),
      hr(),
      span(class = "muted", textOutput("f_count", inline = TRUE)),
      selectizeInput("f_play", "Play", choices = NULL, options = list(placeholder = "Pick a play")),
      div(class = "d-flex gap-2",
          actionButton("f_prev", "Previous", class = "btn-sm btn-outline-secondary"),
          actionButton("f_random", "Random", class = "btn-sm btn-primary"),
          actionButton("f_next", "Next", class = "btn-sm btn-outline-secondary")),
      checkboxInput("f_arrows", "Show where players are heading", FALSE)
    ),
    uiOutput("f_tiles"),
    layout_columns(col_widths = c(7, 5),
      card(
        card_header(textOutput("f_title", inline = TRUE)),
        plotOutput("f_field", height = "420px"),
        field_legend(),
        uiOutput("f_slider_ui")
      ),
      card(card_header("Every gap at the handoff"), uiOutput("f_meter"),
           p(class = "muted mb-0 mt-1", "Score = chance the run succeeds through that gap, from its width and the unblocked defenders who get there before the runner."))
    ),
    layout_columns(col_widths = c(7, 5),
      card(card_header("What happened"), uiOutput("f_story")),
      card(card_header("Success by group, runs in this filter"),
           plotOutput("f_groups", height = "220px"))
    )
  )
)

quiz_tab <- nav_panel("Be the running back",
  layout_sidebar(
    sidebar = sidebar(width = 270, open = "desktop",
      h6("How to play"),
      p(class = "muted", "Each round has 10 runs. The play stops at the handoff. Pick the gap you would hit, then watch what the real back did."),
      selectInput("q_scheme", "Scheme", c("All schemes" = "", scheme_choices)),
      checkboxInput("q_hard", "Only plays where the call was not the best gap", TRUE),
      checkboxInput("q_show_call", "Show the play call", TRUE),
      checkboxInput("q_arrows", "Show where players are heading", TRUE),
      checkboxInput("q_hint", "Hint: show defenders per gap", FALSE),
      actionButton("q_new_round", "Start a new round", class = "btn-sm btn-outline-secondary w-100")
    ),
    uiOutput("q_progress"),
    layout_columns(col_widths = c(7, 5),
      card(
        card_header(uiOutput("q_title", inline = TRUE)),
        conditionalPanel("!output.q_done", plotOutput("q_field", height = "420px")),
        uiOutput("q_review"),
        uiOutput("q_legend"),
        uiOutput("q_slider_ui")
      ),
      uiOutput("q_panel")
    )
  )
)

guide_tab <- nav_panel("Gap guide",
  layout_columns(col_widths = c(7, 5),
    card(card_header("What LA, RB, RC, LD... mean"),
      plotOutput("guide_plot", height = "210px"),
      tags$ul(
        tags$li(strong("The letter"), " says how far out from the center the gap is. ",
                strong("A"), " is between the center and a guard, ", strong("B"), " between the guard and the tackle, ",
                strong("C"), " just outside the tackle (between the tackle and a tight end on the line, or the edge when there is none), and ",
                strong("D"), " outside the tight end. A side only has a D gap when a tight end or extra lineman is on the line there."),
        tags$li(strong("The L or R"), " is the offense's left or right, as the runner sees it. So RB is the right B gap and LD is the edge outside the left tight end."),
        tags$li("This is the same naming coaches use when they call a run (\"34 Power\" aims at a B gap, outside zone at the C gap). Using it lets us put the designed gap, the best gap and the gap taken on one scale."),
        tags$li("We draw the gaps from where the linemen stand at the handoff. A pulling lineman keeps his snap spot. If a lineman has stepped inside his neighbour by then, the gap between them is closed and shows as a sliver, so some plays show fewer letters on the field. The gap list next to the field always shows every gap.")
      )),
    card(card_header("The three gaps and the five groups"),
      tags$ul(
        tags$li(strong("Designed gap: "), "where the blocking scheme points. A gap run follows the pulling lineman; inside zone aims at the play-side B gap; outside zone at the play-side C gap. Man, duo and other runs have none."),
        tags$li(strong("Best gap: "), "the gap with the highest score at the handoff."),
        tags$li(strong("Gap taken: "), "where the runner crosses the line of scrimmage.")
      ),
      tags$ul(lapply(GROUPS, function(gr) tags$li(strong(paste0(gr, ":")), " ", GROUP_MEANING[[gr]]))),
      p(class = "muted", "The best gap is an estimate. It uses only what is on the field at the handoff, and we never see what would have happened in a gap the runner did not take."))
  )
)

ui <- page_navbar(title = "Gap explorer", theme = theme_app, fillable = FALSE, header = app_css,
                  film_tab, quiz_tab, guide_tab)

# ==== SERVER ========================================================================

regular_ids <- cmp[, .N, by = ballCarrierId][N >= 100, ballCarrierId]
ROUND_SIZE <- 10

group_success_plot <- function(d) {
  s <- d[, .(runs = .N, success = mean(success)), by = group]
  s[, group := factor(group, levels = rev(GROUPS))]
  overall <- mean(d$success)
  ggplot(s, aes(success, group)) +
    geom_col(fill = COL_OFFENSE, width = 0.6) +
    geom_vline(xintercept = overall, colour = COL_INK, linetype = "dashed") +
    geom_text(aes(label = sprintf("%.0f%% (n=%d)", 100 * success, runs)), hjust = -0.1, size = 2.8,
              colour = COL_INK) +
    scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1), expand = c(0, 0)) +
    scale_y_discrete(labels = function(x) gsub("Followed the call into a closed gap", "Followed call, closed gap", x)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 9) +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA),
          axis.text = element_text(colour = COL_INK))
}

server <- function(input, output, session) {

  output$guide_plot <- renderPlot(gap_guide_plot(), res = 96)

  # ---- Film room ---------------------------------------------------------------

  filtered <- reactive({
    d <- cmp
    if (nzchar(input$f_group))  d <- d[group == input$f_group]
    if (nzchar(input$f_scheme)) d <- d[scheme == input$f_scheme]
    if (nzchar(input$f_runner)) d <- d[ballCarrierDisplayName == input$f_runner]
    if (nzchar(input$f_team))   d <- d[possessionTeam == input$f_team]
    if (nzchar(input$f_result)) d <- d[success == as.integer(input$f_result)]
    if (isTRUE(input$f_regular)) d <- d[ballCarrierId %in% regular_ids]
    d[order(week, gameId, playId)]
  })

  output$f_count <- renderText(sprintf("%s runs match", format(nrow(filtered()), big.mark = ",")))

  set_play <- function(d, k) {
    updateSelectizeInput(session, "f_play", choices = setNames(d$key, play_label(d)), selected = k, server = TRUE)
  }
  observeEvent(filtered(), {
    d <- filtered()
    keep <- if (isolate(input$f_play) %in% d$key) isolate(input$f_play) else
      if (nrow(d) > 0) d$key[sample.int(nrow(d), 1)] else character(0)
    set_play(d, keep)
  })
  step_play <- function(by) {
    d <- filtered(); if (nrow(d) == 0) return()
    i <- match(input$f_play, d$key); if (is.na(i)) i <- 0
    set_play(d, d$key[((i - 1 + by) %% nrow(d)) + 1])
  }
  observeEvent(input$f_next, step_play(1))
  observeEvent(input$f_prev, step_play(-1))
  observeEvent(input$f_random, {
    d <- filtered(); if (nrow(d) > 0) set_play(d, d$key[sample.int(nrow(d), 1)])
  })

  f_play <- reactive({
    req(input$f_play, input$f_play %in% cmp$key)
    load_play(input$f_play)
  })

  output$f_tiles <- renderUI({
    p <- f_play()$p
    div(class = "tiles",
        tile("Group", p$group, GROUP_MEANING[[p$group]], cls = "t-group"),
        tile("Designed gap", fifelse(is.na(p$design_gap), "none", p$design_gap), SCHEME_LABELS[p$scheme]),
        tile("Best gap", p$best_gap, sprintf("score %s", fmt_pct(p$best_score))),
        tile("Gap taken", p$gap_taken, sprintf("score %s", fmt_pct(p$taken_score))),
        tile("Result", sprintf("%+d yd", p$yards), sprintf("%s, %s", down_text(p), fifelse(p$success == 1, "success", "failure"))))
  })

  output$f_slider_ui <- renderUI({
    pl <- f_play()
    sliderInput("f_frame", NULL, min = pl$first, max = pl$last, value = pl$p$handoff_frame, step = 1,
                width = "100%", ticks = FALSE, post = " frame",
                animate = animationOptions(interval = 200, loop = FALSE))   # half speed
  })

  output$f_title <- renderText({
    pl <- f_play(); p <- pl$p
    fr <- input$f_frame; if (is.null(fr)) fr <- p$handoff_frame
    tag <- fcase(fr == p$snap_frame, " (snap)", fr == p$handoff_frame, " (handoff)", default = "")
    sprintf("%s, week %d, %s vs %s · %+.1f s from the snap%s", p$ballCarrierDisplayName, p$week,
            p$possessionTeam, p$defensiveTeam, (fr - p$snap_frame) / FPS, tag)
  })

  output$f_field <- renderPlot({
    pl <- f_play()
    fr <- input$f_frame
    if (is.null(fr) || fr < pl$first || fr > pl$last) fr <- pl$p$handoff_frame
    draw_field(pl, fr, c("scores", "design", "best", "taken", "path", if (isTRUE(input$f_arrows)) "arrows"))
  }, res = 96)

  output$f_meter <- renderUI(gap_meter(f_play()))

  output$f_story <- renderUI({
    p <- f_play()$p
    tagList(
      p(class = "mb-1", p$playDescription),
      p(class = "muted mb-0", sprintf(
        "%s The best gap's score was %s and the gap taken scored %s, so %.0f points of success were left on the table.",
        fifelse(is.na(p$design_gap), "No designed gap on this run.",
                sprintf("The call was %s.", p$design_gap)),
        fmt_pct(p$best_score), fmt_pct(p$taken_score), 100 * fcoalesce(p$left_on_table, 0))))
  })

  output$f_groups <- renderPlot({
    d <- filtered(); req(nrow(d) > 0)
    group_success_plot(d)
  }, res = 96, bg = "transparent")

  # ---- Be the running back --------------------------------------------------------
  #
  # A round is 10 plays. For each play: "ask" (field frozen at the handoff, pick
  # a gap), then "answer" (same picture plus scores and marks, a slider to watch
  # the play). After the 10th answer the panel shows the round summary.

  quiz <- reactiveValues(round = character(0), i = 1L, pick = NA_character_, history = data.table())

  quiz_pool <- reactive({
    d <- cmp[taken_source == "crossed_los"]
    if (nzchar(input$q_scheme)) d <- d[scheme == input$q_scheme]
    if (isTRUE(input$q_hard)) d <- d[!is.na(design_gap) & design_gap != best_gap]
    d
  })
  new_round <- function() {
    d <- quiz_pool(); req(nrow(d) > 0)
    quiz$round <- d$key[sample.int(nrow(d), min(ROUND_SIZE, nrow(d)))]
    quiz$i <- 1L
    quiz$pick <- NA_character_
    quiz$history <- data.table()
  }
  observeEvent(TRUE, new_round(), once = TRUE)
  observeEvent(input$q_new_round, new_round())
  observeEvent(input$q_again, new_round())
  observeEvent(list(input$q_scheme, input$q_hard), new_round(), ignoreInit = TRUE)
  observeEvent(input$q_next, {
    quiz$i <- quiz$i + 1L
    quiz$pick <- NA_character_
  })

  answered <- reactive(!is.na(quiz$pick))
  finished <- reactive(quiz$i > length(quiz$round))
  output$q_done <- reactive(finished())
  outputOptions(output, "q_done", suspendWhenHidden = FALSE)

  # Round review, one row per play (shown when the round is over)
  output$q_review <- renderUI({
    if (!finished()) return(NULL)
    h <- merge(quiz$history, cmp[, .(play = key, ballCarrierDisplayName, yards, success)], by = "play", sort = FALSE)
    tags$table(class = "table table-sm",
      tags$thead(tags$tr(tags$th("#"), tags$th("Back"), tags$th("You"), tags$th("Best"), tags$th("Back took"), tags$th("Result"))),
      tags$tbody(lapply(seq_len(nrow(h)), function(j) {
        r <- h[j]
        tags$tr(tags$td(j), tags$td(r$ballCarrierDisplayName),
                tags$td(span(class = if (r$pick == r$best) "text-success fw-bold" else "text-danger fw-bold", r$pick)),
                tags$td(r$best), tags$td(r$taken),
                tags$td(sprintf("%+d yd, %s", r$yards, fifelse(r$success == 1, "success", "failure"))))
      })))
  })
  q_play <- reactive({ req(!finished()); load_play(quiz$round[quiz$i]) })

  record_pick <- function(gp) {
    if (answered()) return()
    pl <- q_play(); p <- pl$p
    quiz$pick <- gp
    quiz$history <- rbind(quiz$history, data.table(
      play = p$key, pick = gp, best = p$best_gap, taken = p$gap_taken, design = p$design_gap,
      pick_score = pl$g[gap == gp, score], best_score = p$best_score, taken_score = p$taken_score))
  }
  for (gp in paste0(rep(c("L", "R"), each = 4), GAP_LETTERS)) local({
    this <- gp
    observeEvent(input[[paste0("q_pick_", this)]], record_pick(this), ignoreInit = TRUE)
  })

  output$q_progress <- renderUI({
    n <- length(quiz$round); h <- quiz$history
    segs <- lapply(seq_len(n), function(j) {
      cls <- if (j <= nrow(h)) (if (h$pick[j] == h$best[j]) "done-right" else "done-wrong") else
        if (j == quiz$i) "now" else ""
      div(class = cls)
    })
    div(div(class = "d-flex justify-content-between muted",
            span(if (finished()) "Round complete" else sprintf("Play %d of %d", quiz$i, n)),
            span(sprintf("Best gap found: %d of %d", sum(h$pick == h$best), nrow(h)))),
        div(class = "progress-segs", segs))
  })

  output$q_title <- renderUI({
    if (finished()) return("Review your round")
    p <- q_play()$p
    if (!answered()) {
      sprintf("%s · %s · %s", down_text(p), SCHEME_LABELS[p$scheme],
              if (isTRUE(input$q_show_call) && !is.na(p$design_gap)) paste("The call:", p$design_gap) else "Call hidden")
    } else {
      sprintf("%s, week %d, %s vs %s", p$ballCarrierDisplayName, p$week, p$possessionTeam, p$defensiveTeam)
    }
  })

  output$q_slider_ui <- renderUI({
    if (finished() || !answered()) return(NULL)
    pl <- q_play()
    div(p(class = "muted mb-0", "Press play to watch the run from the handoff."),
        sliderInput("q_frame", NULL, min = pl$p$handoff_frame, max = pl$last, value = pl$p$handoff_frame,
                    step = 1, width = "100%", ticks = FALSE, post = " frame",
                    animate = animationOptions(interval = 200, loop = FALSE)))
  })

  output$q_legend <- renderUI({
    if (finished()) return(NULL)
    field_legend(scores = answered(), pick = answered())
  })

  output$q_field <- renderPlot({
    req(!finished())
    pl <- q_play(); p <- pl$p
    arrows <- if (isTRUE(input$q_arrows)) "arrows"
    if (!answered()) {
      draw_field(pl, p$handoff_frame, c(if (isTRUE(input$q_show_call)) "design", arrows))
    } else {
      fr <- input$q_frame
      if (is.null(fr) || fr < p$handoff_frame || fr > pl$last) fr <- p$handoff_frame
      draw_field(pl, fr, c("scores", "design", "best", "taken", "path", "pick"), pick = quiz$pick)
    }
  }, res = 96)

  choice_list <- function(pl) {
    g <- gap_order(copy(pl$g)); p <- pl$p
    g[, words := gap_words(g)]
    lapply(seq_len(nrow(g)), function(i) {
      r <- g[i]
      cls <- if (answered()) {
        if (r$gap == p$best_gap) "is-right" else if (r$gap == quiz$pick) "is-wrong" else ""
      } else ""
      hint <- if (isTRUE(input$q_hint) && !answered())
        span(class = "c-hint", sprintf("%d unblocked", r$n_unblocked))
      actionButton(paste0("q_pick_", r$gap), class = paste("choice", cls),
                   disabled = if (answered()) NA else NULL,
                   label = tagList(span(class = "gm-name", r$gap), span(class = "c-words", r$words), hint,
                                   if (answered() && r$gap == quiz$pick) tag_chip("c-pick", "You"),
                                   if (answered() && r$gap == p$gap_taken) tag_chip("c-taken", "Taken")))
    })
  }

  output$q_panel <- renderUI({
    h <- quiz$history
    if (finished()) {
      return(card(card_header("Your round"),
        div(class = "tiles", style = "grid-template-columns: repeat(2, 1fr);",
            tile("Best gap found", sprintf("%d / %d", sum(h$pick == h$best), nrow(h))),
            tile("NFL backs, same plays", sprintf("%d / %d", sum(h$taken == h$best), nrow(h))),
            tile("Your expected success", fmt_pct(mean(h$pick_score))),
            tile("Backs' expected success", fmt_pct(mean(h$taken_score, na.rm = TRUE)))),
        p(class = "muted", sprintf("Always taking the best gap would have given %s. Across all 5,559 runs, backs took the best gap %s of the time.",
                                   fmt_pct(mean(h$best_score)), fmt_pct(mean(cmp$took_best)))),
        actionButton("q_again", "Play another round", class = "btn-primary")))
    }
    pl <- q_play(); p <- pl$p
    if (!answered()) {
      return(card(card_header("Which gap do you hit?"),
                  choice_list(pl),
                  p(class = "muted mb-0", "Gaps are listed left to right as you see them on the field.")))
    }
    right <- quiz$pick == p$best_gap
    card(card_header(if (right) "You found the best gap" else "Not the best gap"),
      div(class = paste("answer-box", if (right) "ab-right" else "ab-wrong"),
          div(class = "ab-label", "Your answer"),
          strong(quiz$pick), sprintf(" (score %s)", fmt_pct(pl$g[gap == quiz$pick, score]))),
      if (!right) div(class = "answer-box ab-right", div(class = "ab-label", "Best gap"),
                      strong(p$best_gap), sprintf(" (score %s)", fmt_pct(p$best_score))),
      div(class = "answer-box ab-info", div(class = "ab-label", "What the back did"),
          sprintf("%s took %s and gained %d yards (%s). ", p$ballCarrierDisplayName, p$gap_taken, p$yards,
                  fifelse(p$success == 1, "success", "failure")),
          span(class = "muted", paste0(p$group, ": ", GROUP_MEANING[[p$group]]))),
      choice_list(pl),
      actionButton("q_next", if (quiz$i < length(quiz$round)) "Next play" else "See my round",
                   class = "btn-primary w-100"))
  })
}

shinyApp(ui, server)
