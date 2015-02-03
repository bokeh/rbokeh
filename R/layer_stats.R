#' @export
ly_hist <- function(fig, x, group = NULL, data = NULL,
  breaks = "Sturges", freq = TRUE, include.lowest = TRUE, right = TRUE,
  density = NULL, angle = 45, warn.unused = FALSE,
  color = NULL, alpha = NULL,
  line_color = NULL, line_width = 1, line_alpha = 1,
  fill_color = NULL, fill_alpha = NULL,
  lname = NULL, lgroup = NULL, ...) {

  xname <- deparse(substitute(x))
  yname <- ifelse(freq, "Frequency", "Density")

  validateFig(fig, "ly_hist")

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x     <- v_eval(substitute(x), data)
    # group <- v_eval(substitute(group), data)
  }

  xyNames <- getXYNames(NULL, NULL, xname, yname, list(...))

  lgroup <- getLgroup(lgroup, fig)

  hh <- graphics::hist.default(x = x, breaks = breaks,
    freq = freq, include.lowest = include.lowest, right = right,
    density = density, angle = angle, col = col,
    warn.unused = warn.unused, plot = FALSE)

  args <- list(color = color, alpha = alpha, line_color = line_color,
    line_width = line_width, line_alpha = line_alpha,
    fill_color = fill_color, fill_alpha = fill_alpha, ...)

  args <- resolveColorAlpha(args, hasLine = TRUE, hasFill = TRUE, fig$layers[[lgroup]])

  y <- if(freq) {
    hh$counts
  } else {
    hh$density
  }

  do.call(ly_rect, c(list(fig = fig,
    xleft = hh$breaks[-length(hh$breaks)],
    xright = hh$breaks[-1], ytop = y, ybottom = 0,
    xlab = xyNames$x, ylab = xyNames$y,
    lname = lname, lgroup = lgroup), args))
}

#' @export
ly_density <- function(fig, x, data = NULL, bw = "nrd0", adjust = 1,
  kernel = c("gaussian", "epanechnikov", "rectangular", "triangular",
    "biweight", "cosine", "optcosine"),
  weights = NULL, window = kernel, n = 512, cut = 3, na.rm = FALSE,
  color = "black", alpha = NULL, width = 1, type = 1,
  line_join = 1, line_cap = "round",
  legend = NULL, lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_density")

  xname <- deparse(substitute(x))
  yname <- "Density"

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x     <- v_eval(substitute(x), data)
    # group <- v_eval(substitute(group), data)
  }

  xyNames <- getXYNames(NULL, NULL, xname, yname, list(...))

  lgroup <- getLgroup(lgroup, fig)

  if(!is.null(data))
    x <- eval(substitute(x), data)

  args <- list(color = color, alpha = alpha, width = width,
    type = type, line_join = line_join, line_cap = line_cap, ...)

  args <- updateLineOpts(fig, args)

  dd <- stats::density.default(x = x, bw = bw, adjust = adjust, kernel = kernel, n = n, cut = 3, na.rm = na.rm)

  do.call(ly_line, c(list(fig = fig, x = dd$x, y = dd$y, xlab = xname, ylab = yname), args))
}

# ly_rug

#' @export
ly_quantile <- function(fig, x, group = NULL, data = NULL,
  probs = NULL, distn = qunif, ncutoff = 200,
  color = NULL, alpha = NULL,
  line_color = NULL, line_alpha = 1, line_width = 1,
  fill_color = NULL, fill_alpha = NULL,
  legend = TRUE,
  lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_quantile")

  xname <- "f-value"
  yname <- deparse(substitute(x))

  ## deal with possible named inputs from a data source
  if(!is.null(data)) {
    x     <- v_eval(substitute(x), data)
    group <- v_eval(substitute(group), data)
  }

  lgroup <- getLgroup(lgroup, fig)

  args <- list(color = color, alpha = alpha,
    line_color = line_color, line_alpha = line_alpha,
    line_width = line_width, fill_color = fill_color,
    fill_alpha = fill_alpha, ...)

  if(is.null(group))
    group <- rep(1, length(x))

  naIdx <- which(is.na(x))
  x <- x[-naIdx]
  group <- group[-naIdx]

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
          curProbs <- ppoints(ncutoff)
          qq <- quantile(x[ii], curProbs, names = FALSE, na.rm = TRUE)
        } else {
          curProbs <- ppoints(length(x[ii]))
          qq <- sort(x[ii])
        }
      } else {
        curProbs <- probs
        qq <- quantile(x[ii], curProbs, names = FALSE, na.rm = TRUE)
      }
      ff <- distn(curProbs)

      curLegend <- NULL
      if(is.logical(legend)) {
        if(legend)
          curLegend <- group[[ii[1]]]
      } else {
        curLegend <- legend
      }

      fig <- do.call(ly_point, c(list(fig = fig, x = ff, y = qq,
        xlab = xname, ylab = yname,
        lgroup = lgroup, legend = curLegend), args))
    }
  }
  fig
}

#' @export
ly_boxplot <- function(fig, x, y = NULL, data = NULL,
  coef = 1.5, line_color = "black",
  line_alpha = 1, line_width = 2,
  fill_color = "lightblue", fill_alpha = 0.5,
  lname = NULL, lgroup = NULL, ...) {

  validateFig(fig, "ly_boxplot")

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
  lgroup <- getLgroup(lgroup, fig)

  ## deal with vector inputs from a data source
  if(!is.null(data)) {
    x <- eval(substitute(x), data)
    y <- eval(substitute(y), data)
    if(is.factor(x))
      x <- as.character(x)
    if(is.factor(y))
      y <- as.character(y)
  }

  args <- list(line_color = line_color, line_alpha = line_alpha,
    line_width = line_width, fill_color = fill_color,
    fill_alpha = fill_alpha, ...)

  fill_ind <- grepl("^fill_", names(args))

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
      stop("At least one of 'x' or 'y' should be numeric for ly_boxplot.")
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

    fig <- do.call(ly_crect, c(list(fig = fig, x = rep(gp, 2), y = c(md1, md2), width = 0.9, height = c(hgt1, hgt2), xlab = xname, ylab = yname), args))
    fig <- do.call(ly_segments, c(list(fig = fig, x0 = c(gp, gp, gpr, gpr), y0 = c(bp$stats[1], bp$stats[4], bp$stats[1], bp$stats[5]), x1 = c(gp, gp, gpl, gpl), y1 = c(bp$stats[2], bp$stats[5], bp$stats[1], bp$stats[5])), args[!fill_ind]))

    if(length(bp$out) > 0) {
      fig <- do.call(ly_point, c(list(fig = fig, x = rep(gp, length(bp$out)), y = bp$out, type = 1), args))
    }
  }

  fig
}

# ly_violin

# ly_bar

# ly_dotplot

# ly_rug

