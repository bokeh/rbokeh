#' @export
lay_hist <- function(fig, x, data = NULL, breaks = "Sturges", freq = TRUE,
  include.lowest = TRUE, right = TRUE, 
  density = NULL, angle = 45, warn.unused = FALSE,
  line_color = NULL, line_width = 1, line_alpha = 1,
  fill_color = NULL, fill_alpha = 1,
  ...) {

  xname <- deparse(substitute(x))
  yname <- ifelse(freq, "Frequency", "Density")

  if(!is.null(data))
    x <- eval(substitute(x), data)

  validateFig(fig, "lay_hist")

  hh <- graphics::hist.default(x = x, breaks = breaks, 
    freq = freq, include.lowest = include.lowest, right = right,
    density = density, angle = angle, col = col,
    warn.unused = warn.unused, plot = FALSE)

  opts <- c(list(line_color = line_color, 
    line_width = line_width, line_alpha = line_alpha,
    fill_color = fill_color, fill_alpha = fill_alpha), 
    list(...))

  if(is.null(line_color))
    opts$line_color <- getNextColor(fig)
  if(is.null(fill_color))
    opts$fill_color <- reduceSaturation(opts$line_color)

  y <- if(freq) {
    hh$counts
  } else {
    hh$density
  }

  do.call(lay_rect, c(list(fig = fig, xleft = hh$breaks[-length(hh$breaks)], xright = hh$breaks[-1], ytop = y, ybottom = 0, xlab = xname, ylab = yname), opts))
}

#' @export
lay_density <- function(fig, x, data = NULL, bw = "nrd0", adjust = 1, kernel = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"), weights = NULL, window = kernel, n = 512, cut = 3, na.rm = FALSE,
  line_color = "black", line_alpha = NULL, line_width = 1, line_dash = 1, line_join = 1, line_cap = "round", ...) {

  validateFig(fig, "lay_density")

  xname <- deparse(substitute(x))
  yname <- "Density"

  if(!is.null(data))
    x <- eval(substitute(x), data)

  opts <- c(list(line_color = line_color, 
    line_alpha = line_alpha, line_width = line_width, 
    line_dash = line_dash, line_join = line_join, 
    line_cap = line_cap), list(...))

  opts <- updateLineOpts(fig, opts)

  dd <- stats::density.default(x = x, bw = bw, adjust = adjust, kernel = kernel, n = n, cut = 3, na.rm = na.rm)

  do.call(lay_lines, c(list(fig = fig, x = dd$x, y = dd$y, xlab = xname, ylab = yname), opts))
}

# lay_rug

#' @export
lay_quantile <- function(fig, x, groups = NULL, data = NULL,
  probs = NULL, distn = qunif, line_color = NULL, line_alpha = 1, line_width = 1, fill_color = NULL, fill_alpha = NULL, ...) {

  validateFig(fig, "lay_quantile")

  opts <- c(list(line_color = line_color, line_alpha = line_alpha, 
    line_width = line_width, fill_color = fill_color, 
    fill_alpha = fill_alpha), list(...))

  if(!is.null(data)) {
    x      <- eval(substitute(x), data)
    groups <- eval(substitute(groups), data)
  }
  if(is.null(groups))
    groups <- rep(1, length(x))

  idx <- split(seq_along(x), groups)
  for(ii in idx) {
    if(is.null(probs)) {
      ## if the vector is too long, perhaps should default
      ## to some length, like 1000
      curProbs <- ppoints(length(x[ii]))
      qq <- sort(x[ii])      
    } else {
      curProbs <- probs
      qq <- quantile(x[ii], curProbs, names = FALSE)
    }
    ff <- distn(curProbs)

    fig <- do.call(lay_points, c(list(fig = fig, x = ff, y = qq), opts))
  }
  fig
}

#' @export
lay_boxplot <- function(fig, x, y = NULL, data = NULL, coef = 1.5, line_color = "black", line_alpha = 1, line_width = 2, fill_color = "lightblue", fill_alpha = 0.5, ...) {

  validateFig(fig, "lay_boxplot")

  xnm <- deparse(substitute(x))
  ynm <- deparse(substitute(y))

  ## deal with vector inputs from a data source
  if(!is.null(data)) {
    x <- eval(substitute(x), data)
    y <- eval(substitute(y), data)
    if(is.factor(x))
      x <- as.character(x)
    if(is.factor(y))
      y <- as.character(y)
  }

  opts <- c(list(line_color = line_color, line_alpha = line_alpha, 
    line_width = line_width, fill_color = fill_color, 
    fill_alpha = fill_alpha), list(...))

  fill_ind <- grepl("^fill_", names(opts))

  if(is.null(y)) {
    xname <- ""
    yname <- xnm
    group <- rep(xname, length(x))
  } else {
    numInd <- c(is.numeric(x), is.numeric(y))
    if(all(numInd)) {
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
    } else if(numInd[1]) {
      xname <- ynm
      yname <- xnm
      group <- y
    } else if(numInd[2]) {
      xname <- xnm
      yname <- ynm
      group <- x
      x <- y
    } else {
      stop("At least one of 'x' or 'y' should be numeric for lay_boxplot.")
    }
  }

  idx <- split(seq_along(x), group)
  for(ii in seq_along(idx)) {
    bp <- boxplot.stats(x = x[idx[[ii]]], coef = coef)

    gp <- group[[idx[[ii]][1]]] ## doesn't work right now
    ## for lines and whiskers
    gpl <- paste(gp, ":0.4", sep = "")
    gpr <- paste(gp, ":0.6", sep = "")
    hgt1 <- bp$stats[3] - bp$stats[2]
    md1 <- hgt1 / 2 + bp$stats[2]
    hgt2 <- bp$stats[4] - bp$stats[3]
    md2 <- hgt2 / 2 + bp$stats[3]

    fig <- do.call(lay_crect, c(list(fig = fig, x = rep(gp, 2), y = c(md1, md2), width = 0.9, height = c(hgt1, hgt2), xlab = xname, ylab = yname), opts))
    fig <- do.call(lay_segments, c(list(fig = fig, x0 = c(gp, gp, gpr, gpr), y0 = c(bp$stats[1], bp$stats[4], bp$stats[1], bp$stats[5]), x1 = c(gp, gp, gpl, gpl), y1 = c(bp$stats[2], bp$stats[5], bp$stats[1], bp$stats[5])), opts[!fill_ind]))

    if(length(bp$out) > 0) {
      fig <- do.call(lay_points, c(list(fig = fig, x = rep(gp, length(bp$out)), y = bp$out, type = 1), opts))
    }
  }

  fig
}

# lay_violin

# lay_bar

# lay_dotplot

# lay_rug

# lay_hexbin <- function(fig, x, y, data = NULL, shape = 1, xbins = 30) {
# }

