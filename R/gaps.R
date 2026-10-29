# Gap definitions shared by the scheme, best-gap and gap-taken steps.
#
# Gaps are named by side (L or R, the offense's left or right) and letter:
#   A  between the center and the guard
#   B  between the guard and the tackle
#   C  between the tackle and the end man (on-line TE or extra lineman);
#      with no end man on that side, C is the edge outside the tackle
#   D  outside the end man (only when there is an end man)
# The outermost gap on each side is the edge gap.
#
# Players on each side are ordered by lateral distance from the center at the
# snap: the 1st is the guard, the 2nd the tackle, the 3rd the end man. Typical
# splits are about 1.5 yards per player.

GAP_LETTERS   <- c("A", "B", "C", "D")
DEFAULT_SPLIT <- 1.5   # yards, used when a side has fewer linemen than expected

# One row per run and side with the lateral distance of each edge from the center.
#   d1 = guard, d2 = tackle, d3 = end man (NA if none)
line_edges <- function(linemen) {
  ctr <- linemen[is_center == TRUE, .(gameId, playId, center_y = y, center_x = x)]
  l <- merge(linemen[is_center == FALSE], ctr, by = c("gameId", "playId"))
  l[, `:=`(side = fifelse(y > center_y, "L", "R"), u = abs(y - center_y))]
  setorder(l, gameId, playId, side, u)
  l[, k := seq_len(.N), by = .(gameId, playId, side)]
  e <- l[, .(d1 = u[k == 1][1], d2 = u[k == 2][1], d3 = u[k == 3][1],
             id1 = nflId[k == 1][1], id2 = nflId[k == 2][1], id3 = nflId[k == 3][1]),
         by = .(gameId, playId, side)]
  # Make sure both sides exist for every run
  grid <- ctr[, .(side = c("L", "R")), by = .(gameId, playId)]
  e <- merge(grid, e, by = c("gameId", "playId", "side"), all.x = TRUE)
  e[is.na(d1), d1 := DEFAULT_SPLIT]
  e[is.na(d2), d2 := d1 + DEFAULT_SPLIT]
  e <- merge(e, ctr, by = c("gameId", "playId"))
  e[]
}

# Gap letter for a lateral distance u from the center, given that side's edges.
gap_letter <- function(u, d1, d2, d3) {
  fcase(u < d1, "A",
        u < d2, "B",
        is.na(d3) | u < d3, "C",
        default = "D")
}

# Side and letter for points y on each run. `pts` needs gameId, playId, y.
gap_of_point <- function(pts, edges) {
  pts <- merge(pts, unique(edges[, .(gameId, playId, center_y)]), by = c("gameId", "playId"))
  pts[, side := fifelse(y > center_y, "L", "R")]
  pts <- merge(pts, edges[, .(gameId, playId, side, d1, d2, d3)],
               by = c("gameId", "playId", "side"))
  pts[, gap := paste0(side, gap_letter(abs(y - center_y), d1, d2, d3))]
  pts[, c("center_y", "d1", "d2", "d3") := NULL]
  pts[]
}
