#' Add a "curve" layer to a Bokeh figure
#'
#' Draws a curve corresponding to a function over the interval \code{[from, to]}.
#' @param fig figure to modify
#' @param expr,from,to,n parameters sent to \code{\link[graphics]{curve}}
# template par-lineprops
# template par-legend
# template par-lnamegroup
# template dots-line
#' @examples
#' \donttest{
#' chippy <- function(x) sin(cos(x)*exp(-x/2))
#' figure(width = 800) %>%
#'   ly_curve(chippy, -8, 7, n = 2001)
#' }
#' @family layer functions
#' @export
ly_curve <- function(
  fig, expr, from = NULL, to = NULL, n = 101,
  width = 1, type = 1,
  color = "black", alpha = 1,
  hov_color = NULL, hov_alpha = NULL, ns_color = NULL, ns_alpha = NULL,
  legend = NULL, lname = NULL, lgroup = NULL,
  ...
) {

  xname <- "x"
  sexpr <- substitute(expr)
  if (is.name(sexpr)) {
    yname <- paste(deparse(sexpr), "(x)", sep = "")
    expr <- call(as.character(sexpr), as.name(xname))
  } else {
    yname <- deparse(sexpr)
    chk1 <- is.call(sexpr) || is.expression(sexpr)
    chk <- !(chk1 && xname %in% all.vars(sexpr))
    if (chk)
      stop(
        gettextf("'expr' must be a function, or a call or an expression containing '%s'",
          xname), domain = NA)
    expr <- sexpr
  }

  x <- seq.int(from, to, length.out = n)
  ll <- list(x = x)
  names(ll) <- xname
  y <- eval(expr, envir = ll, enclos = parent.frame())

  ly_lines(
    fig = fig,
    x = x, y = y,
    color = color, alpha = alpha, width = width, type = type,
    hov_color = hov_color, hov_alpha = hov_alpha,
    ns_color = ns_color, ns_alpha = ns_alpha,
    # hover = hover, TODO?
    legend = legend, lname = lname, lgroup = lgroup,
    xlab = xname, ylab = yname
  )
}

#' Add a "contour" layer to a Bokeh figure
#'
#' Computes and draws contour lines.
#' @param fig figure to modify
#' @param z a matrix containing the values to compute contour lines for
#' @param x,y locations of grid lines at which the values in \code{image} are measured (see \code{\link[grDevices]{contourLines}})
#' @param nlevels,levels parameters sent to \code{\link[grDevices]{contourLines}})
# template par-lineprops
# template par-lnamegroup
# template dots-line
#' @example man-roxygen/ex-image.R
#' @family layer functions
#' @export
ly_contour <- function(
  fig, z,
  x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, length.out = ncol(z)),
  nlevels = 10, levels = pretty(range(z, na.rm = TRUE), nlevels),
  color = "black", alpha = 1, width = 1, type = 1,
  hov_color = NULL, hov_alpha = NULL, ns_color = NULL, ns_alpha = NULL,
  lname = NULL, lgroup = NULL,
  ...
) {
  contr <- do.call(grDevices::contourLines,
    list(x = x, y = y, z = z, nlevels = nlevels, levels = levels))

  xs <- lapply(contr, "[[", 2)
  ys <- lapply(contr, "[[", 3)

  ly_multi_line(fig,
    xs = xs, ys = ys,
    type = type, width = width, color = color, alpha = alpha,
    # hover = hover, TODO?
    legend = legend,
    hov_color = hov_color, hov_alpha = hov_alpha, ns_color = ns_color, ns_alpha = ns_alpha,
    lgroup = lgroup, lname = lname
  )
}

#' Add an "abline" layer to a Bokeh figure
#'
#' Draws one or more straight lines.
#' @param fig figure to modify
#' @param a,b the intercept and slope of the line(s) to draw
#' @param v the x value(s) for vertical lines
#' @param h the y value(s) for horizontal lines
#' @param coef a vector of length two giving the intercept and slope
# template par-lineprops
# template par-legend
# template par-lnamegroup
# template dots-line
#' @example man-roxygen/ex-lines.R
#' @examples
#' # abline with mixed axes for h and v
#' figure() %>%
#'   ly_points(1:26, letters) %>%
#'   ly_abline(h = "j") %>%
#'   ly_abline(v = 10)
#'
#' # multiple hv lines
#' figure() %>%
#'   ly_points(1:10) %>%
#'   ly_abline(v = 1:10) %>%
#'   ly_abline(h = 1:10)
#'
#' # multiple ab lines
#' figure() %>%
#'   ly_points(0:10) %>%
#'   ly_abline(0, seq(0, 1, by = 0.1))
#' @family layer functions
#' @export
ly_abline <- function(
  fig, a = NULL, b = NULL, v = NULL, h = NULL, coef = NULL,
  color = "black", alpha = NULL, width = 1, type = 1,
  hov_color = NULL, hov_alpha = NULL, ns_color = NULL, ns_alpha = NULL,
  sel_color = NULL, sel_alpha = NULL,
  legend = NULL, lname = NULL, lgroup = NULL,
  ...) {

  # x_axis_type <- "numeric"
  # y_axis_type <- "numeric"
  # if (!is.null(h) || !is.null(v)) {
  #   x_axis_type <- fig$x$spec$x_axis_type
  #   y_axis_type <- fig$x$spec$y_axis_type
  # }

  # manage data
  if (!is.null(coef) || inherits(a, "lm")) {
    if (is.null(coef))
      coef <- a
    if (inherits(coef, "lm"))
      coef <- coef(coef)
    coef <- as.numeric(coef)
    a <- coef[1]
    b <- coef[2]
  }

  if (!is.null(a) && !is.null(b)) {
    nn <- max(c(length(a), length(b)))
    if (length(a) < nn)
      a <- rep(a, nn)[1:nn]
    if (length(b) < nn)
      b <- rep(b, nn)[1:nn]
    x0 <- rep(0, nn)
    y0 <- a
    x1 <- rep(1, nn)
    y1 <- b * x1 + a
  } else if (!is.null(h)) {
    if (inherits(h, c("Date", "POSIXt"))) {
      # y_axis_type <- "datetime"
      h <- to_epoch(h)
    }
    nn <- length(h)
    x0 <- rep(0, nn)
    y0 <- h
    x1 <- rep(1, nn)
    y1 <- h
  } else if (!is.null(v)) {
    if (inherits(v, c("Date", "POSIXt"))) {
      # x_axis_type <- "datetime"
      v <- to_epoch(v)
    }
    nn <- length(v)
    x0 <- v
    y0 <- rep(0, nn)
    x1 <- v
    y1 <- rep(1, nn)
  }

  defer_fn <- function(data, xlim, ylim) {
    if (length(data$x0) == 1) {
      if (data$x0 == "x0")
        return(data)
    } else if (length(data$x0) == 0) {
      return(data)
    }
    if (is.list(data$x0))
      data <- unlist(data, recursive = FALSE)
    if (all(data$x0 == data$x1)) {
      ## vertical lines
      lo <- head(ylim, 1)
      up <- tail(ylim, 1)
      if (is.character(lo)) {
        lo <- paste0(lo, ":0")
        up <- paste0(up, ":1")
      }
      data$y0 <- rep(lo, length(data$y0))
      data$y1 <- rep(up, length(data$y1))
    } else if (all(data$y0 == data$y1)) {
      ## horizontal line
      lo <- head(xlim, 1)
      up <- tail(xlim, 1)
      if (is.character(lo)) {
        lo <- paste0(lo, ":0")
        up <- paste0(up, ":1")
      }
      data$x0 <- rep(lo, length(data$x0))
      data$x1 <- rep(up, length(data$x1))
    } else {
      ## line
      b <- (data$y1 - data$y0) / (data$x1 - data$x0)
      a <- data$y1 - b * data$x1
      nn <- length(a)
      data$x0 <- rep(head(xlim, 1), nn)
      data$x1 <- rep(tail(xlim, 1), nn)
      data$y0 <- data$x0 * b + a
      data$y1 <- data$x1 * b + a
    }
    # now below wrap each result with list so json encoding is happy
    if (length(data$x0) == 1) {
      data$x0 <- list(data$x0)
      data$x1 <- list(data$x1)
      data$y0 <- list(data$y0)
      data$y1 <- list(data$y1)
    }
    data
  }

  ly_segments(fig,
    x0 = x0, y0 = y0, x1 = x1, y1 = y1,
    type = type, width = width,
    color = color, alpha = alpha,
    hov_color = hov_color, hov_alpha = hov_alpha,
    ns_color = ns_color, ns_alpha = ns_alpha,
    sel_color = sel_color, sel_alpha = sel_alpha,
    # hover = hover, url = url, TODO?
    legend = legend,
    lname = lname, lgroup = lgroup
  )
}
