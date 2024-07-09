# c.rects ----------------------------------------------------------------------

#' Combine Rectangles
#'
#' @param \dots one or more objects of class "rects"
#' @export
c.rects <- function(...)
{
  rbind(...)
}

# check_rects ------------------------------------------------------------------
check_rects <- function(x)
{
  stopifnot(inherits(x, "rects"))
}

# find_lim ---------------------------------------------------------------------
find_lim <- function(r, axis = "x")
{
  check_rects(r)
  coord <- select_columns(r, paste0("ll", axis))
  size <- select_columns(r, ifelse(axis == "x", "w", "h"))
  c(min(coord, na.rm = TRUE), max(coord + size, na.rm = TRUE))
}

# get_mids ---------------------------------------------------------------------
get_mids <- function(rects)
{
  data <- select_columns(rects, c("llx", "lly", "w", "h"))

  data.frame(
    x = data$llx + data$w/2,
    y = data$lly + data$h/2
  )
}

# init_plot --------------------------------------------------------------------
init_plot <- function(
    width = 1,
    height = 1,
    xlim = c(0, width),
    ylim = c(0, height),
    axes = FALSE
)
{
  plot(
    NA,
    xlim = xlim,
    ylim = ylim,
    xlab = "",
    ylab = "",
    asp = 1,
    axes = axes
  )
}

# move -------------------------------------------------------------------------

#' Generic Function to Move Objects
#'
#' @param rects objects to be moved
#' @param \dots arguments passed to class methods
#' @export
move <- function(rects, ...)
{
  UseMethod("move")
}

# move.rects -------------------------------------------------------------------

#' Move Rectangles
#'
#' @param rects A "rects" object
#' @param dx delta x
#' @param dy delta y
#' @param top top position
#' @param bottom bottom position
#' @param left left position
#' @param right right position
#' @param each logical indicating whether to move each rectangle or the group of
#'   rectangles
#' @param \dots additional arguments (currently not used)
#' @export
move.rects <- function(
    rects,
    dx = 0,
    dy = 0,
    top = NULL,
    bottom = NULL,
    left = NULL,
    right = NULL,
    each = FALSE,
    ...
)
{
  check_rects(rects)

  #dx = 0; dy = 0; top = NULL; bottom = NULL; left = NULL; right = NULL
  is_set <- !sapply(FUN = is.null, list(
    top = top,
    bottom = bottom,
    left = left,
    right = right
  ))

  if (is_set["top"]) {
    upper_y <- rects$lly + rects$h
    dy <- top - if (each) upper_y else max(upper_y)
  } else if (is_set["bottom"]) {
    lower_y <- rects$lly
    dy <- bottom - if (each) lower_y else min(lower_y)
  }

  if (is_set["left"]) {
    left_x <- rects$llx
    dx <- left - if (each) left_x else min(left_x)
  } else if (is_set["right"]) {
    right_x <- rects$llx + rects$w
    dx <- right - if (each) right_x else max(right_x)
  }

  rects$llx <- rects$llx + dx
  rects$lly <- rects$lly + dy

  rects
}

# new_rects --------------------------------------------------------------------

#' Create a "rects" object
#'
#' @param w width(s) of rectangle(s)
#' @param h height(s) of rectangle(s)
#' @param llx x position(s) of lower left corner(s) of rectangle(s)
#' @param lly y position(s) of lower left corner(s) of rectangle(s)
#' @param lbl_text text label(s)
#' @param lbl_align label alignment(s), default: "centre"
#' @param density density of shade lines. Default: -1
#' @param angle angle of shade lines. Default: 45
#' @param col fill colour(s) of rectangles
#' @param border border colour(s) of rectangles
#' @param lty line type(s) of rectangles
#' @param lwd line width(s) of rectangles
#' @param n number of rectangles. All other arguments are recycled to vectors of
#'   this length.
#' @export
new_rects <- function(
    w = 1,
    h = 1,
    llx = 0,
    lly = 0,
    lbl_text = NULL,
    lbl_align = "centre",
    density = -1,
    angle = 45,
    col = NA,
    border = graphics::par("fg"),
    lty = graphics::par("lty"),
    lwd = graphics::par("lwd"),
    n = NULL
)
{
  #llx = 0; lly = 0; lty = 1; lwd = 1; label = NA
  #w = 2; h = 3; llx = 1:4

  vectors <- list(
    w = w,
    h = h,
    llx = llx,
    lly = lly,
    lbl_align = lbl_align,
    density = density,
    angle = angle,
    col = col,
    border = border,
    lty = lty,
    lwd = lwd
  )

  # length of the longest vector or length given in n
  n <- default_if_null(n, max(lengths(vectors)))

  # Resize all vectors to length n
  vectors <- lapply(vectors, rep, length.out = n)

  vectors[["lbl_text"]] <- if (is.null(lbl_text)) {
    paste0("r", seq_len(n))
  } else {
    rep(lbl_text, length.out = n)
  }

  vectors <- vectors[kwb.utils::pairwise(names(vectors))]

  # Convert to data frame and add class "rects"
  kwb.utils::addClass(as.data.frame(vectors), "rects")
}

# plot.rects -------------------------------------------------------------------

#' Plot Rectangles
#'
#' @param x object of class "rects"
#' @param add logical indicating whether to add rectangles to an existing plot
#'   or to start a new plot
#' @param cex character expansion factor
#' @param y not used. Just there to comply with the generic plot() interface.
#' @param \dots additional arguments (currently not used)
#' @export
plot.rects <- function(
    x, add = !is.null(grDevices::dev.list()), cex = 1, y = NULL, ...
)
{
  check_rects(x)

  if (!add) {
    init_plot(
      xlim = find_lim(x, "x"),
      ylim = find_lim(x, "y"),
      axes = TRUE
    )
  }

  args <- to_rect_args(remove_columns(x, pattern = "^lbl_"))

  do.call(graphics::rect, args)

  mids <- get_mids(x)

  labels <- kwb.utils::defaultIfNA(
    select_columns(x, "lbl_text"),
    seq_len(nrow(x))
  )

  align <- select_columns(x, "lbl_align")

  is_left <- align == "left"
  is_centre <- align == "centre"

  text <- function(x, y, labels, adj) {
    graphics::text(x = x, y = y, labels = labels, adj = adj, cex = cex)
  }

  # Left aligned labels
  if (any(is_left)) {
    text(
      x = x$llx[is_left],
      y = mids$y[is_left],
      labels = labels[is_left],
      adj = c(0, 0.5)
    )
  }

  # Centred labels
  if (any(is_centre)) {
    text(
      x = mids$x[is_centre],
      y = mids$y[is_centre],
      labels = labels[is_centre],
      adj = c(0.5, 0.5)
    )
  }

  invisible(x)
}

# separate ---------------------------------------------------------------------
separate <- function(x, ...)
{
  UseMethod("separate")
}

# separate.rects ---------------------------------------------------------------

#' Separate Rectangles
#'
#' @param x "rects" object
#' @param dx space in horizontal direction to be put in between the rectangles
#' @param dy space in vertical direction to be put in between the rectangles
#' @param \dots further arguments (currently not used)
#' @export
separate.rects <- function(x, dx = 0, dy = 0, ...)
{
  check_rects(x)
  mids <- get_mids(x)
  x$llx <- x$llx + (order(mids$x) - 1L) * dx
  x$lly <- x$lly + (order(mids$y) - 1L) * dy
  x
}

#' Stack Rectangles
#'
#' @param rects a "rects" object
#' @param \dots further arguments passed to \code{stack.rects}
#' @export
stack <- function(rects, ...)
{
  UseMethod("stack")
}

# stack.rects ------------------------------------------------------------------

#' Stack Rectangles Vertically or Horizontally
#'
#' @param rects a "rects" object as returned by \code{\link{new_rects}}
#' @param horizontal whether to stack the rectangles horizontally or not. The
#'   default is \code{FALSE}, i.e. rectangles are stacked vertically.
#' @param delta space between rectangles, default: 0
#' @param reverse whether or not to reverse the stack order. The default is
#'   \code{FALSE}.
#' @param \dots not used
#' @export
#' @examples
#' rects <- new_rects(w = 1:3)
#' stacked_vertically <- stack(rects)
#' stacked_horizontally <- stack(rects, horizontal = TRUE)
#' plot(stacked_vertically, add = FALSE)
#' plot(stacked_horizontally, add = FALSE)
#' stacked_vertically_spaced <- stack(rects, delta = 0.1)
#' stacked_horizontally_spaced <- stack(rects, horizontal = TRUE, delta = 0.1)
#' plot(stacked_vertically_spaced, add = FALSE)
#' plot(stacked_horizontally_spaced, add = FALSE)
stack.rects <- function(rects, horizontal = FALSE, delta = 0, reverse = FALSE, ...)
{
  check_rects(rects)

  bar_heights <- rects[[ifelse(horizontal, "w", "h")]]

  positions <- stacked_positions(bar_heights, delta = delta, reverse = reverse)

  rects[[ifelse(horizontal, "llx", "lly")]] <- positions[, "lower"]

  rects
}

# stacked_positions ------------------------------------------------------------
stacked_positions <- function(x, delta = 0, reverse = FALSE)
{
  upper <- cumsum(x) + (seq_along(x) - 1L) * delta
  lower <- c(0, upper[-length(upper)] + delta)

  cbind(
    lower = if (reverse) -upper else lower,
    upper = if (reverse) -lower else upper
  )
}

# to_label ---------------------------------------------------------------------
to_label <- function(key, value)
{
  sprintf("%s: %0.1f", key, value)
}

# to_rect_args -----------------------------------------------------------------
to_rect_args <- function(rects)
{
  cols <- c("llx", "lly", "w", "h")
  data <- select_columns(rects, cols)

  coords <- data.frame(
    xleft = data$llx,
    ybottom = data$lly,
    xright = data$llx + data$w,
    ytop = data$lly + data$h
  )

  cbind(coords, rects[, setdiff(names(rects), cols), drop = FALSE])
}

# unlabel_and_dash -------------------------------------------------------------
unlabel_and_dash <- function(x)
{
  dplyr::mutate(x, lbl_text = "", lty = "dashed")
}
