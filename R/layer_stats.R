
#' Add a "hist" layer to a Bokeh figure
#' Draws a histogram
#' @param fig figure to modify
#' @param x,breaks,freq,include.lowest,right parameters passed to \code{\link[graphics]{hist}}
#' @param data an optional data frame, providing the source for x
#' @template par-coloralpha
#' @template par-lnamegroup
#' @template dots-fillline
#' @family layer functions
#' @export
ly_hist <- function(fig, x, data = NULL,
  breaks = "Sturges", freq = TRUE, include.lowest = TRUE, right = TRUE,
  color = NULL, alpha = 1,
  lname = NULL, lgroup = NULL, ...) {

  xname <- deparse(substitute(x))
  yname <- ifelse(freq, "Frequency", "Density")

  validate_fig(fig, "ly_hist")

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x     <- v_eval(substitute(x), data)
    # group <- v_eval(substitute(group), data)
  }

  lgroup <- get_lgroup(lgroup, fig)

  hh <- graphics::hist.default(x = x, breaks = breaks,
    include.lowest = include.lowest, right = right, plot = FALSE)

  args <- list(color = color, alpha = alpha, ...)

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE, fig$layers[[lgroup]])

  y <- if(freq) {
    hh$counts
  } else {
    hh$density
  }

  do.call(ly_rect, c(list(fig = fig,
    xleft = hh$breaks[-length(hh$breaks)],
    xright = hh$breaks[-1], ytop = y, ybottom = 0,
    xlab = xname, ylab = yname,
    lname = lname, lgroup = lgroup), args))
}


#' Add a "density" layer to a Bokeh figure
#' Draws a histogram
#' @param fig figure to modify
#' @param x,bw,adjust,kernel,weights,window,n,cut,na.rm parameters passed to \code{\link[stats]{density}}
#' @param data an optional data frame, providing the source for x
#' @template par-lineprops
#' @param legend text to display in the legend entry for the density line
#' @template par-lnamegroup
#' @template dots-line
#' @family layer functions
#' @export
ly_density <- function(fig, x, data = NULL, bw = "nrd0", adjust = 1,
  kernel = c("gaussian", "epanechnikov", "rectangular", "triangular",
    "biweight", "cosine", "optcosine"),
  weights = NULL, window = kernel, n = 512, cut = 3, na.rm = FALSE,
  color = "black", alpha = 1, width = 1, type = 1,
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_density")

  xname <- deparse(substitute(x))
  yname <- "Density"

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x     <- v_eval(substitute(x), data)
    # group <- v_eval(substitute(group), data)
  }

  xy_names <- get_xy_names(NULL, NULL, xname, yname, list(...))

  lgroup <- get_lgroup(lgroup, fig)

  if(!is.null(data))
    x <- eval(substitute(x), data)

  args <- list(color = color, alpha = alpha, width = width,
    type = type, ...)

  args <- update_line_opts(fig, args)

  dd <- stats::density.default(x = x, bw = bw, adjust = adjust, kernel = kernel, n = n, cut = 3, na.rm = na.rm)

  do.call(ly_lines, c(list(fig = fig, x = dd$x, y = dd$y, xlab = xname, ylab = yname), args))
}

# ly_rug


#' Add a "quantile" layer to a Bokeh figure
#' Draws quantiles
#' @param fig figure to modify
#' @param x numeric vector or field name of variable to compute sample quantiles for
#' @param group values or field name of a grouping variable to break quantile computations up by
#' @param data an optional data frame, providing the source for x
#' @param probs numeric vector of probabilities with values in \code{[0,1]} at which to compute quantiles - if \code{NULL}, every point of \code{x} is a quantile
#' @param distn quantile function to use on the x-axis (e.g. \code{\link[stats]{qnorm}}) - default is \code{\link[stats]{qunif}},
#' @param ncutoff if the length of \code{x} exceeds this value and \code{probs} is not specified, compute quantiles at \code{ncutoff} points
#' @template par-coloralpha
#' @template par-legend
#' @template par-lnamegroup
#' @template dots-fillline
#' @family layer functions
#' @export
ly_quantile <- function(fig, x, group = NULL, data = NULL,
  probs = NULL, distn = qunif, ncutoff = 200,
  color = NULL, alpha = 1,
  legend = TRUE, lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_quantile")

  xname <- "f-value"
  yname <- deparse(substitute(x))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x     <- v_eval(substitute(x), data)
    group <- v_eval(substitute(group), data)
  }

  lgroup <- get_lgroup(lgroup, fig)

  args <- list(color = color, alpha = alpha, ...)

  if(is.null(group))
    group <- rep(1, length(x))

  na_idx <- is.na(x)
  x <- x[!na_idx]
  group <- group[!na_idx]

  idx <- split(seq_along(x), group)

  ## quantile plot with no groups needs explicit legend
  ## but with groups, legend can simply be "TRUE" in which case
  ## an entry is automatically added for each group
  if(length(idx) == 1) {
    if(is.logical(legend))
      legend <- NULL
  }

  for(ii in idx) {
    if(length(ii) > 0) {
      if(is.null(probs)) {
        ## if the vector is too long, perhaps should default
        ## to some length, like 1000
        if(length(ii) > ncutoff) {
          cur_probs <- ppoints(ncutoff)
          qq <- quantile(x[ii], cur_probs, names = FALSE, na.rm = TRUE)
        } else {
          cur_probs <- ppoints(length(x[ii]))
          qq <- sort(x[ii])
        }
      } else {
        cur_probs <- probs
        qq <- quantile(x[ii], cur_probs, names = FALSE, na.rm = TRUE)
      }
      ff <- distn(cur_probs)

      cur_legend <- NULL
      if(is.logical(legend)) {
        if(legend)
          cur_legend <- group[[ii[1]]]
      } else {
        cur_legend <- legend
      }

      fig <- do.call(ly_points, c(list(fig = fig, x = ff, y = qq,
        xlab = xname, ylab = yname,
        lgroup = lgroup, legend = cur_legend), args))
    }
  }
  fig
}

#' Add a "boxplot" layer to a Bokeh figure
#' @param fig figure to modify
#' @param x either a numeric vector or a factor
#' @param y either a numeric vector or a factor
#' @param data an optional data frame, providing the source for x and y
#' @param coef see \code{\link[grDevices]{boxplot.stats}}
#' @template par-coloralpha
#' @template par-lnamegroup
#' @template dots-fillline
#' @family layer functions
#' @export
ly_boxplot <- function(fig, x, y = NULL, data = NULL,
  coef = 1.5,
  color = "blue", alpha = 1,
  lname = NULL, lgroup = NULL, ...) {

  validate_fig(fig, "ly_boxplot")

  xnm <- deparse(substitute(x))
  ynm <- deparse(substitute(y))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x <- v_eval(substitute(x), data)
    y <- v_eval(substitute(y), data)
    if(is.factor(x))
      x <- as.character(x)
    if(is.factor(y))
      y <- as.character(y)
  }

  ## translate different x, y types to vectors
  lgroup <- get_lgroup(lgroup, fig)

  ## deal with vector inputs from a data source
  if(!is.null(data)) {
    x <- eval(substitute(x), data)
    y <- eval(substitute(y), data)
    if(is.factor(x))
      x <- as.character(x)
    if(is.factor(y))
      y <- as.character(y)
  }

  args <- list(color = color, alpha = alpha, ...)

  args <- resolve_color_alpha(args, has_line = TRUE, has_fill = TRUE)

  fill_ind <- grepl("^fill_", names(args))

  if(is.null(y)) {
    xname <- " "
    yname <- xnm
    group <- rep(xname, length(x))
  } else {
    num_ind <- c(is.numeric(x), is.numeric(y))
    if(all(num_ind)) {
      message("both x and y are numeric -- choosing numeric variable based on which has the most unique values")
      if(length(unique(x)) > length(unique(y))) {
        xname <- ynm
        yname <- xnm
        group <- y
      } else {
        xname <- xnm
        yname <- ynm
        group <- x
        x <- y
      }
    } else if(num_ind[1]) {
      xname <- ynm
      yname <- xnm
      group <- y
    } else if(num_ind[2]) {
      xname <- xnm
      yname <- ynm
      group <- x
      x <- y
    } else {
      stop("At least one of 'x' or 'y' should be numeric for ly_boxplot.")
    }
  }

  idx <- split(seq_along(x), group)
  for(ii in seq_along(idx)) {
    bp <- boxplot.stats(x = x[idx[[ii]]], coef = coef)

    gp <- group[idx[[ii]][1]] ## doesn't work right now
    ## for lines and whiskers
    gpl <- paste(gp, ":0.4", sep = "")
    gpr <- paste(gp, ":0.6", sep = "")
    hgt1 <- bp$stats[3] - bp$stats[2]
    md1 <- hgt1 / 2 + bp$stats[2]
    hgt2 <- bp$stats[4] - bp$stats[3]
    md2 <- hgt2 / 2 + bp$stats[3]

    fig <- do.call(ly_crect, c(list(fig = fig, x = rep(gp, 2), y = c(md1, md2), width = 0.9, height = c(hgt1, hgt2), xlab = xname, ylab = yname), args))
    fig <- do.call(ly_segments, c(list(fig = fig, x0 = c(gp, gp, gpr, gpr), y0 = c(bp$stats[1], bp$stats[4], bp$stats[1], bp$stats[5]), x1 = c(gp, gp, gpl, gpl), y1 = c(bp$stats[2], bp$stats[5], bp$stats[1], bp$stats[5])), args[!fill_ind]))

    if(length(bp$out) > 0) {
      fig <- do.call(ly_points, c(list(fig = fig, x = rep(gp, length(bp$out)), y = bp$out, type = 1), args))
    }
  }

  fig
}

# ly_violin

# ly_bar

# ly_dotplot

# ly_rug

