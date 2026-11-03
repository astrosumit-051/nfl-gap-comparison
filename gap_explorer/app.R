# gap_explorer/app.R
# Gap explorer: a Shiny app for the designed gap vs best gap vs gap taken.
# DSC 355 Project 3.
#
# Two tabs:
#   Film room            Filter the runs, pick one, and play it frame by frame.
#                        Gaps are shaded by their score at the handoff, with the
#                        designed, best and taken gaps marked.
#   Be the running back  The play stops at the handoff. Click the gap you would
#                        hit. The app then shows the designed, best and taken
#                        gaps and keeps your read rate next to the real backs'.
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
  xlim <- c(los - 9, max(los + 12, min(max(path$x) + 2, los + 20)))
  near <- at_ho[x >= xlim[1] & x <= xlim[2]]
  ylim <- range(c(g$ymin, g$ymax, path[x <= xlim[2]]$y, near$y)) + c(-1.5, 1.5)
  ylim <- c(max(ylim[1], 0), min(ylim[2], FIELD_WIDTH))
  g[is_edge == TRUE & side == "L", ymax := ylim[2]]
  g[is_edge == TRUE & side == "R", ymin := ylim[1]]

  list(p = p, trk = trk, g = g, path = path, xlim = xlim, ylim = ylim)
}

# Which gap a click falls in (y is the lateral field position)
gap_at <- function(g, y) {
  if (is.null(y)) return(NA_character_)
  hit <- g[ymin <= y & ymax >= y]
  if (nrow(hit) == 0) return(NA_character_)
  hit[which.min(ymax - ymin), gap]
}

fmt_pct <- function(x) ifelse(is.na(x), "-", sprintf("%.0f%%", 100 * x))

# ==== FIELD PICTURE ================================================================
#
# Drawn from behind the offense: the offense moves up the screen and the
# offense's left is on the left. Screen x = FIELD_WIDTH - y, screen y = x.
#
# show: which marks to draw. "scores" shades the gaps by their score; "design",
# "best", "taken" outline those gaps; "path" draws the runner's path up to the
# frame; "arrows" shows where each player is heading over the next 0.5 s; "pick"
# outlines the gap the viewer picked.

to_screen <- function(y) FIELD_WIDTH - y

# Gaps in screen order, left to right: LD, LC, LB, LA, RA, RB, RC, RD
gap_order <- function(g) g[order(side, fifelse(side == "L", -match(letter, GAP_LETTERS), match(letter, GAP_LETTERS)))]
GAP_LETTERS <- c("A", "B", "C", "D")

draw_field <- function(pl, frame, show, pick = NA_character_) {
  p <- pl$p; los <- p$los_x
  g <- copy(pl$g)
  g[, `:=`(sx0 = to_screen(ymax), sx1 = to_screen(ymin))]
  g[, mid := (sx0 + sx1) / 2]
  at <- pl$trk[frameId == frame]
  at[, who := fcase(role == "football", "Ball", role == "defense", "Defense",
                    role == "carrier", "Runner", default = "Offense")]
  at[, `:=`(sx = to_screen(y), sy = x, dsx = -vy * 0.5, dsy = vx * 0.5)]
  players <- at[who != "Ball"]
  ball <- at[who == "Ball"]

  g[, roles := ""]
  add_role <- function(gap_name, label) {
    if (!is.na(gap_name)) g[gap == gap_name, roles := fifelse(roles == "", label, paste(roles, label, sep = " + "))]
  }
  if ("pick" %in% show) add_role(pick, "Your pick")
  if ("design" %in% show) add_role(p$design_gap, "Designed")
  if ("best" %in% show) add_role(p$best_gap, "Best")
  if ("taken" %in% show) add_role(p$gap_taken, "Taken")
  g[, label := fifelse(sx1 - sx0 >= 0.9, gap, "")]

  box <- function(gap_name, pad, ...) {
    d <- g[gap == gap_name]
    geom_rect(data = d, aes(xmin = sx0, xmax = sx1, ymin = los - pad, ymax = los + pad), fill = NA, ...)
  }

  plt <- ggplot() +
    geom_hline(yintercept = seq(10, 110, 5), colour = COL_GRID, linewidth = 0.4) +
    geom_hline(yintercept = los, colour = COL_INK, linewidth = 0.6, linetype = "dashed")

  if ("scores" %in% show) {
    g[, dark := score >= median(score)]
    plt <- plt +
      geom_rect(data = g, aes(xmin = sx0, xmax = sx1, ymin = los - 0.8, ymax = los + 0.8, fill = score),
                colour = COL_FIELD, linewidth = 0.6) +
      geom_text(data = g[dark == TRUE], aes(x = mid, y = los, label = label),
                colour = "#ffffff", size = 3, fontface = "bold") +
      geom_text(data = g[dark == FALSE], aes(x = mid, y = los, label = label),
                colour = "#0b0b0b", size = 3, fontface = "bold") +
      scale_fill_gradientn(colours = SCORE_RAMP, name = "Gap score",
                           limits = range(gaps$score), labels = scales::label_percent(accuracy = 1))
  } else {
    plt <- plt +
      geom_rect(data = g, aes(xmin = sx0, xmax = sx1, ymin = los - 0.8, ymax = los + 0.8),
                fill = "#f1f0ec", colour = "#b9b8b3", linewidth = 0.5) +
      geom_text(data = g, aes(x = mid, y = los, label = label),
                colour = "#0b0b0b", size = 3, fontface = "bold")
  }

  if ("design" %in% show && !is.na(p$design_gap))
    plt <- plt + box(p$design_gap, 1, colour = "#0b0b0b", linewidth = 0.8, linetype = "22")
  if ("best" %in% show)  plt <- plt + box(p$best_gap, 1.15, colour = "#0b0b0b", linewidth = 1.2)
  if ("taken" %in% show) plt <- plt + box(p$gap_taken, 1.3, colour = COL_DEFENSE, linewidth = 1)
  if ("pick" %in% show && !is.na(pick))
    plt <- plt + box(pick, 1.45, colour = COL_PICK, linewidth = 1, linetype = "41")

  if ("path" %in% show) {
    path <- pl$path[frameId >= p$handoff_frame & frameId <= frame]
    if (nrow(path) > 1)
      plt <- plt + geom_path(data = path, aes(to_screen(y), x), colour = COL_RUNNER, linewidth = 0.8)
  }
  if ("arrows" %in% show)
    plt <- plt + geom_segment(data = players, aes(x = sx, y = sy, xend = sx + dsx, yend = sy + dsy, colour = who),
                              linewidth = 0.5, arrow = arrow(length = unit(0.12, "cm")), show.legend = FALSE)

  plt <- plt +
    geom_point(data = players, aes(sx, sy, colour = who, size = who == "Runner"), stroke = 0.6) +
    { if (nrow(ball) > 0) geom_point(data = ball, aes(sx, sy), colour = COL_BALL, size = 1.6) }

  # Role labels sit behind the line, one row each so neighbours do not overlap
  lab <- gap_order(g[roles != ""])
  if (nrow(lab) > 0) {
    lab[, row := seq_len(.N)]
    plt <- plt + geom_label(data = lab, aes(x = mid, y = los - 1.4 - 1.25 * row, label = roles),
                            size = 3.1, fontface = "bold", colour = "#0b0b0b",
                            fill = scales::alpha("#ffffff", 0.85))
  }

  plt +
    scale_size_manual(values = c(`FALSE` = 3.4, `TRUE` = 4.6), guide = "none") +
    scale_colour_manual(values = c(Offense = COL_OFFENSE, Defense = COL_DEFENSE, Runner = COL_RUNNER),
                        name = NULL) +
    coord_fixed(xlim = rev(to_screen(pl$ylim)), ylim = pl$xlim, expand = FALSE) +
    theme_void(base_size = 13) +
    theme(plot.background = element_rect(fill = COL_FIELD, colour = NA),
          legend.position = "bottom", legend.box = "horizontal",
          legend.title = element_text(colour = COL_INK, size = 10, vjust = 0.8),
          legend.text = element_text(colour = COL_INK, size = 10),
          legend.key.width = unit(1.4, "cm"),
          legend.spacing.x = unit(0.6, "cm"),
          plot.margin = margin(8, 8, 8, 8))
}

play_label <- function(d) {
  sprintf("Wk %d \u00b7 %s vs %s \u00b7 %s \u00b7 %s \u00b7 %d yds", d$week, d$possessionTeam, d$defensiveTeam,
          d$ballCarrierDisplayName, SCHEME_LABELS[d$scheme], d$yards)
}

gap_table <- function(pl) {
  g <- pl$g; p <- pl$p
  mark <- function(x) fifelse(x, "\u2713", "")
  gap_order(g)[, .(Gap = gap,
                            `Width (yd)` = round(width, 1),
                            `Unblocked defenders` = n_unblocked,
                            `Runner gets there (s)` = round(t_runner, 2),
                            Score = fmt_pct(score),
                            Designed = mark(!is.na(p$design_gap) & gap == p$design_gap),
                            Best = mark(gap == p$best_gap),
                            Taken = mark(gap == p$gap_taken))]
}

# ==== UI ============================================================================

theme_app <- bs_theme(version = 5, bg = "#fcfcfb", fg = "#1b1b1a", primary = "#256abf",
                      base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif"))

legend_note <- p(class = "text-muted small mb-0",
  "Seen from behind the offense, which moves up the screen. ",
  "Gaps sit on the line of scrimmage (dashed) as they were at the handoff, shaded by their score (expected success). ",
  "Dashed black box = designed gap, solid black box = best gap, orange box = gap taken.")

runners <- sort(unique(cmp$ballCarrierDisplayName))
teams <- sort(unique(cmp$possessionTeam))

film_tab <- nav_panel("Film room",
  layout_sidebar(
    sidebar = sidebar(width = 300,
      selectInput("f_group", "Group", c("All runs" = "", GROUPS)),
      selectInput("f_scheme", "Scheme", c("All schemes" = "", setNames(names(SCHEME_LABELS), SCHEME_LABELS))),
      selectizeInput("f_runner", "Runner", c("All runners" = "", runners)),
      selectInput("f_team", "Offense", c("All teams" = "", teams)),
      radioButtons("f_result", "Result", c("All" = "", "Success" = "1", "Failure" = "0"), inline = TRUE),
      checkboxInput("f_regular", "Only backs with 100+ carries", FALSE),
      hr(),
      textOutput("f_count", inline = TRUE),
      selectizeInput("f_play", "Play", choices = NULL, options = list(placeholder = "Pick a play")),
      div(class = "d-flex gap-2",
          actionButton("f_prev", "Previous", class = "btn-sm btn-outline-secondary"),
          actionButton("f_random", "Random", class = "btn-sm btn-primary"),
          actionButton("f_next", "Next", class = "btn-sm btn-outline-secondary")),
      hr(),
      checkboxInput("f_arrows", "Show where players are heading (next 0.5 s)", FALSE)
    ),
    layout_columns(col_widths = c(8, 4),
      card(full_screen = TRUE,
        card_header(textOutput("f_title", inline = TRUE)),
        plotOutput("f_field", height = "auto"),
        uiOutput("f_slider_ui"),
        legend_note
      ),
      uiOutput("f_summary")
    ),
    layout_columns(col_widths = c(7, 5),
      card(card_header("Every gap at the handoff"), tableOutput("f_gaps")),
      card(card_header("Success by group, runs in this filter"),
           plotOutput("f_groups", height = "260px"))
    )
  )
)

quiz_tab <- nav_panel("Be the running back",
  layout_sidebar(
    sidebar = sidebar(width = 300,
      p("The play stops at the handoff. Click the gap you would hit, on the field or with the buttons."),
      selectInput("q_scheme", "Scheme", c("All schemes" = "", setNames(names(SCHEME_LABELS), SCHEME_LABELS))),
      checkboxInput("q_hard", "Only plays where the call was not the best gap", TRUE),
      checkboxInput("q_show_call", "Show me the play call (designed gap)", TRUE),
      checkboxInput("q_arrows", "Show where players are heading", TRUE),
      actionButton("q_next", "Next play", class = "btn-primary w-100"),
      hr(),
      uiOutput("q_scoreboard"),
      actionButton("q_reset", "Reset score", class = "btn-sm btn-outline-secondary mt-2")
    ),
    layout_columns(col_widths = c(8, 4),
      card(full_screen = TRUE,
        card_header(textOutput("q_title", inline = TRUE)),
        plotOutput("q_field", height = "auto", click = "q_click"),
        uiOutput("q_buttons"),
        legend_note
      ),
      uiOutput("q_result")
    )
  )
)

about_tab <- nav_panel("How it works",
  card(card_body(
    h4("Three gaps on every run"),
    tags$ul(
      tags$li(strong("Designed gap: "), "the gap the blocking scheme points to. A gap run follows the pulling lineman; inside zone aims at the play-side B gap; outside zone aims outside the play-side tackle. Man, duo and other runs have no designed gap."),
      tags$li(strong("Best gap: "), "the gap with the highest score at the handoff. The score is the expected run success from a logistic regression on the gap's projected width and the unblocked defenders who reach it before the runner."),
      tags$li(strong("Gap taken: "), "where the runner crosses the line of scrimmage.")
    ),
    h4("The five groups"),
    tags$ul(lapply(GROUPS, function(gr) tags$li(strong(gr), ": ", GROUP_MEANING[[gr]]))),
    p(class = "text-muted",
      "The best gap is an estimate. It uses only what is on the field at the handoff, and we never see what would have happened in a gap the runner did not take.")
  ))
)

ui <- page_navbar(title = "Gap explorer", theme = theme_app, fillable = FALSE,
                  header = tags$style("#f_field.recalculating { opacity: 1; }"),
                  film_tab, quiz_tab, about_tab)

# ==== SERVER ========================================================================

regular_ids <- cmp[, .N, by = ballCarrierId][N >= 100, ballCarrierId]

group_success_plot <- function(d) {
  s <- d[, .(runs = .N, success = mean(success)), by = group]
  s[, group := factor(group, levels = rev(GROUPS))]
  overall <- mean(d$success)
  ggplot(s, aes(success, group)) +
    geom_col(fill = COL_OFFENSE, width = 0.65) +
    geom_vline(xintercept = overall, colour = COL_INK, linetype = "dashed") +
    geom_text(aes(label = sprintf("%.0f%% (n=%d)", 100 * success, runs)), hjust = -0.1, size = 3.2,
              colour = COL_INK) +
    scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1), expand = c(0, 0)) +
    scale_y_discrete(labels = function(x) gsub("Followed the call into a closed gap", "Followed call, closed gap", x)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = NA),
          axis.text = element_text(colour = COL_INK))
}

# Plot height that fits the play's window at the plot's width, plus the legend
field_height <- function(pl, width) {
  if (is.null(width) || width <= 0) width <- 700
  h <- width * diff(pl$xlim) / diff(pl$ylim) + 70
  round(min(max(h, 320), 720))
}

server <- function(input, output, session) {

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

  observeEvent(filtered(), {
    d <- filtered()
    keep <- if (isolate(input$f_play) %in% d$key) isolate(input$f_play) else
      if (nrow(d) > 0) d$key[sample.int(nrow(d), 1)] else character(0)
    updateSelectizeInput(session, "f_play", choices = setNames(d$key, play_label(d)),
                         selected = keep, server = TRUE)
  })

  step_play <- function(by) {
    d <- filtered(); if (nrow(d) == 0) return()
    i <- match(input$f_play, d$key); if (is.na(i)) i <- 0
    i <- ((i - 1 + by) %% nrow(d)) + 1
    updateSelectizeInput(session, "f_play", choices = setNames(d$key, play_label(d)),
                         selected = d$key[i], server = TRUE)
  }
  observeEvent(input$f_next, step_play(1))
  observeEvent(input$f_prev, step_play(-1))
  observeEvent(input$f_random, {
    d <- filtered(); if (nrow(d) == 0) return()
    updateSelectizeInput(session, "f_play", choices = setNames(d$key, play_label(d)),
                         selected = d$key[sample.int(nrow(d), 1)], server = TRUE)
  })

  f_play <- reactive({
    req(input$f_play, input$f_play %in% cmp$key)
    load_play(input$f_play)
  })

  output$f_slider_ui <- renderUI({
    pl <- f_play(); p <- pl$p
    fr <- range(pl$trk$frameId)
    sliderInput("f_frame", NULL, min = fr[1], max = fr[2], value = p$handoff_frame, step = 1,
                width = "100%", ticks = FALSE,
                animate = animationOptions(interval = 200, loop = FALSE))   # half speed
  })

  output$f_title <- renderText({
    pl <- f_play(); p <- pl$p
    fr <- input$f_frame; if (is.null(fr)) fr <- p$handoff_frame
    when <- (fr - p$snap_frame) / FPS
    tag <- fcase(fr == p$snap_frame, " (snap)", fr == p$handoff_frame, " (handoff)",
                 default = "")
    sprintf("%s \u00b7 %s \u00b7 %+.1f s from the snap%s", p$ballCarrierDisplayName, SCHEME_LABELS[p$scheme], when, tag)
  })

  output$f_field <- renderPlot({
    pl <- f_play()
    fr <- input$f_frame
    if (is.null(fr) || !fr %in% pl$trk$frameId) fr <- pl$p$handoff_frame
    show <- c("scores", "design", "best", "taken", "path", if (isTRUE(input$f_arrows)) "arrows")
    draw_field(pl, fr, show)
  }, res = 96, height = function() field_height(f_play(), session$clientData$output_f_field_width))

  output$f_summary <- renderUI({
    p <- f_play()$p
    card(
      card_header(p$group),
      card_body(
        p(GROUP_MEANING[[p$group]]),
        tags$table(class = "table table-sm mb-2",
          tags$tr(tags$td("Designed gap"), tags$td(strong(fifelse(is.na(p$design_gap), "none", p$design_gap)))),
          tags$tr(tags$td("Best gap"), tags$td(strong(p$best_gap), sprintf(" (score %s)", fmt_pct(p$best_score)))),
          tags$tr(tags$td("Gap taken"), tags$td(strong(p$gap_taken), sprintf(" (score %s)", fmt_pct(p$taken_score)))),
          tags$tr(tags$td("Left on the table"), tags$td(sprintf("%.0f points of success", 100 * fcoalesce(p$left_on_table, 0)))),
          tags$tr(tags$td("Result"), tags$td(sprintf("%d yards, %s", p$yards, fifelse(p$success == 1, "success", "failure")))),
          if (!is.null(p$down)) tags$tr(tags$td("Down"), tags$td(sprintf("%d and %d", p$down, p$yardsToGo)))
        ),
        if (!is.null(p$playDescription)) p(class = "small text-muted mb-0", p$playDescription)
      )
    )
  })

  output$f_groups <- renderPlot({
    d <- filtered(); req(nrow(d) > 0)
    group_success_plot(d)
  }, res = 96, bg = "transparent")

  output$f_gaps <- renderTable(gap_table(f_play()), striped = TRUE, spacing = "s", align = "l")

  # ---- Be the running back --------------------------------------------------------

  quiz <- reactiveValues(key = NULL, pick = NA_character_, history = data.table())

  quiz_pool <- reactive({
    d <- cmp[taken_source == "crossed_los"]
    if (nzchar(input$q_scheme)) d <- d[scheme == input$q_scheme]
    if (isTRUE(input$q_hard)) d <- d[!is.na(design_gap) & design_gap != best_gap]
    d
  })

  new_quiz_play <- function() {
    d <- quiz_pool(); req(nrow(d) > 0)
    seen <- if (nrow(quiz$history) > 0) quiz$history$play else character(0)
    fresh <- d[!key %in% seen]; if (nrow(fresh) == 0) fresh <- d
    quiz$key <- fresh$key[sample.int(nrow(fresh), 1)]
    quiz$pick <- NA_character_
  }
  observeEvent(TRUE, new_quiz_play(), once = TRUE)
  observeEvent(input$q_next, new_quiz_play())
  observeEvent(list(input$q_scheme, input$q_hard), new_quiz_play(), ignoreInit = TRUE)
  observeEvent(input$q_reset, { quiz$history <- data.table(); new_quiz_play() })

  q_play <- reactive({ req(quiz$key); load_play(quiz$key) })

  record_pick <- function(gp) {
    if (is.na(gp) || !is.na(quiz$pick)) return()
    pl <- q_play(); p <- pl$p
    quiz$pick <- gp
    quiz$history <- rbind(quiz$history, data.table(
      play = p$key, pick = gp, best = p$best_gap, taken = p$gap_taken, design = p$design_gap,
      pick_score = pl$g[gap == gp, score], best_score = p$best_score, taken_score = p$taken_score,
      success = p$success))
  }
  observeEvent(input$q_click, record_pick(gap_at(q_play()$g, FIELD_WIDTH - input$q_click$x)))

  output$q_buttons <- renderUI({
    g <- gap_order(q_play()$g)
    div(class = "d-flex flex-wrap gap-1 justify-content-center my-2",
        lapply(g$gap, function(gp)
          actionButton(paste0("q_btn_", gp), gp,
                       class = paste("btn-sm", if (!is.na(quiz$pick) && quiz$pick == gp) "btn-dark" else "btn-outline-dark"),
                       disabled = if (!is.na(quiz$pick)) "disabled" else NULL)))
  })
  for (gp in paste0(rep(c("L", "R"), each = 4), c("A", "B", "C", "D"))) local({
    this <- gp
    observeEvent(input[[paste0("q_btn_", this)]], record_pick(this), ignoreInit = TRUE)
  })

  output$q_title <- renderText({
    p <- q_play()$p
    if (is.na(quiz$pick)) {
      sprintf("%s \u00b7 %d and %d \u00b7 %s. Which gap do you hit?", SCHEME_LABELS[p$scheme], p$down, p$yardsToGo,
              if (isTRUE(input$q_show_call) && !is.na(p$design_gap)) paste("The call is", p$design_gap) else "Call hidden")
    } else {
      sprintf("%s, week %d, %s vs %s", p$ballCarrierDisplayName, p$week, p$possessionTeam, p$defensiveTeam)
    }
  })

  output$q_field <- renderPlot({
    pl <- q_play(); p <- pl$p
    arrows <- if (isTRUE(input$q_arrows)) "arrows"
    if (is.na(quiz$pick)) {
      show <- c(if (isTRUE(input$q_show_call)) "design", arrows)
      draw_field(pl, p$handoff_frame, show)
    } else {
      last <- max(pl$path$frameId)
      draw_field(pl, last, c("scores", "design", "best", "taken", "path", "pick"), pick = quiz$pick)
    }
  }, res = 96, height = function() field_height(q_play(), session$clientData$output_q_field_width))

  output$q_result <- renderUI({
    pl <- q_play(); p <- pl$p
    if (is.na(quiz$pick)) {
      return(card(card_header("Your read"), card_body(
        p("You see the field at the handoff: blue is the offense, orange the defense, black the runner."),
        p("Look for the gap that is wide and has no unblocked defender who can get there before you."))))
    }
    pick_score <- pl$g[gap == quiz$pick, score]
    verdict <- if (quiz$pick == p$best_gap) "You found the best gap." else
      sprintf("The best gap was %s.", p$best_gap)
    card(card_header(verdict),
      card_body(
        tags$table(class = "table table-sm mb-2",
          tags$tr(tags$td("Your pick"), tags$td(strong(quiz$pick), sprintf(" (score %s)", fmt_pct(pick_score)))),
          tags$tr(tags$td("Best gap"), tags$td(strong(p$best_gap), sprintf(" (score %s)", fmt_pct(p$best_score)))),
          tags$tr(tags$td("Designed gap"), tags$td(strong(fifelse(is.na(p$design_gap), "none", p$design_gap)))),
          tags$tr(tags$td(p$ballCarrierDisplayName, " took"), tags$td(strong(p$gap_taken), sprintf(" (score %s)", fmt_pct(p$taken_score)))),
          tags$tr(tags$td("Result"), tags$td(sprintf("%d yards, %s", p$yards, fifelse(p$success == 1, "success", "failure"))))
        ),
        p(strong(p$group), ": ", GROUP_MEANING[[p$group]]),
        p(class = "small text-muted mb-0", "The purple dashed box is your pick. Click Next play for another.")
      ))
  })

  output$q_scoreboard <- renderUI({
    h <- quiz$history
    if (nrow(h) == 0) return(p(class = "text-muted", "Your score shows here after your first pick."))
    tags$div(
      h6(sprintf("After %d play%s", nrow(h), if (nrow(h) == 1) "" else "s")),
      tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th(""), tags$th("You"), tags$th("NFL back"))),
        tags$tr(tags$td("Took the best gap"), tags$td(fmt_pct(mean(h$pick == h$best))),
                tags$td(fmt_pct(mean(h$taken == h$best)))),
        tags$tr(tags$td("Took the call"), tags$td(fmt_pct(mean(h$pick == h$design, na.rm = TRUE))),
                tags$td(fmt_pct(mean(h$taken == h$design, na.rm = TRUE)))),
        tags$tr(tags$td("Expected success"), tags$td(fmt_pct(mean(h$pick_score))),
                tags$td(fmt_pct(mean(h$taken_score, na.rm = TRUE))))
      ),
      p(class = "small text-muted",
        sprintf("Always taking the best gap would give %s expected success. Across all runs, backs took the best gap %s of the time.",
                fmt_pct(mean(h$best_score)), fmt_pct(mean(cmp$took_best))))
    )
  })
}

shinyApp(ui, server)
