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
get_mids <- function(rdf)
{
  data <- select_columns(rdf, c("llx", "lly", "w", "h"))

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
move <- function(x, ...)
{
  UseMethod("move")
}

# move.rects -------------------------------------------------------------------

#' Move Rectangles
#'
#' @param x A "rects" object
#' @param dx delta x
#' @param dy delta y
#' @param top top position
#' @param bottom bottom position
#' @param left left position
#' @param right right position
#' @param each logical indicating whether to move each rectangle or the group of
#'   rectangles
#' @param \dots additional arguments (currently not used)
#' @method move rects
#' @export
move.rects <- function(
    x,
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
  check_rects(rdf)

  #dx = 0; dy = 0; top = NULL; bottom = NULL; left = NULL; right = NULL
  is_set <- !sapply(FUN = is.null, list(
    top = top,
    bottom = bottom,
    left = left,
    right = right
  ))

  #rdf <- as_data_frame(r)

  if (is_set["top"]) {
    upper_y <- rdf$lly + rdf$h
    dy <- top - if (each) upper_y else max(upper_y)
  } else if (is_set["bottom"]) {
    lower_y <- rdf$lly
    dy <- bottom - if (each) lower_y else min(lower_y)
  }

  if (is_set["left"]) {
    left_x <- rdf$llx
    dx <- left - if (each) left_x else min(left_x)
  } else if (is_set["right"]) {
    right_x <- rdf$llx + rdf$w
    dx <- right - if (each) right_x else max(right_x)
  }

  rdf$llx <- rdf$llx + dx
  rdf$lly <- rdf$lly + dy

  rdf
}

# new_rects --------------------------------------------------------------------
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
#' @rdname separate.rects
separate <- function(x, ...)
{
  UseMethod("separate")
}

# separate.rects ---------------------------------------------------------------

#' Separate Rectangles
#'#'
#' @param x "rects" object
#' @param dx space in horizontal direction to be put in between the rectangles
#' @param dy space in vertical direction to be put in between the rectangles
#' @param \dots further arguments (currently not used)
#' @rdname separate.rects
#' @export
separate.rects <- function(x, dx = 0, dy = 0, ...)
{
  check_rects(x)
  mids <- get_mids(x)
  x$llx <- x$llx + (order(mids$x) - 1L) * dx
  x$lly <- x$lly + (order(mids$y) - 1L) * dy
  x
}

stack <- function(x, ...)
{
  UseMethod("stack")
}

# stack.rects ------------------------------------------------------------------
#' @export
stack.rects <- function(x, horizontal = FALSE, delta = 0, reverse = FALSE, ...)
{
  check_rects(x)

  x <- x[[ifelse(horizontal, "w", "h")]]

  positions <- stacked_positions(x, delta = delta, reverse = reverse)

  x[[ifelse(horizontal, "llx", "lly")]] <- positions[, "lower"]

  x
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
to_rect_args <- function(rdf)
{
  cols <- c("llx", "lly", "w", "h")
  data <- select_columns(rdf, cols)

  coords <- data.frame(
    xleft = data$llx,
    ybottom = data$lly,
    xright = data$llx + data$w,
    ytop = data$lly + data$h
  )

  cbind(coords, rdf[, setdiff(names(rdf), cols), drop = FALSE])
}

# unlabel_and_dash -------------------------------------------------------------
unlabel_and_dash <- function(x)
{
  dplyr::mutate(x, lbl_text = "", lty = "dashed")
}
