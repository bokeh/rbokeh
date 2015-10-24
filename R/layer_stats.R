
#' Add a "hist" layer to a Bokeh figure
#'
#' Draws a histogram
#' @param fig figure to modify
#' @param x either a vector to be passed to \code{\link[graphics]{hist}} or an object of class "histogram"
#' @param breaks,freq,include.lowest,right parameters passed to \code{\link[graphics]{hist}}
#' @param data an optional data frame, providing the source for x
#' @template par-coloralpha
#' @template par-lnamegroup
#' @template dots-fillline
#' @family layer functions
#' @export
ly_hist <- function(
  fig, x, data = NULL,
  breaks = "Sturges", freq = TRUE, include.lowest = TRUE, right = TRUE,
  color = NULL, alpha = 1,
  lname = NULL, lgroup = NULL, ...
) {

  validate_fig(fig, "ly_hist")

  args <- sub_names(fig, data,
    grab(
      x,
      color, # TODO If i supply color, it should stack or dodge by default
      alpha,
      # no legend?
      lname, lgroup,
      dots = lazy_dots(...)
    )
  )


  if(inherits(args$data$x, "histogram")) {
    hh <- args$data$x
    args$info$xName <- args$data$x$xname
  } else {
    # was moved to position of "y" as only "x" was supplied.  (inside sub_names)
    # moving values from "y" to "x"
    hh <- graphics::hist.default(x = args$data[[2]], breaks = breaks,
      include.lowest = include.lowest, right = right, plot = FALSE)
    args$info$xName <- args$info$yName
  }
  args$info$yName <- ifelse(freq, "Frequency", "Density")

  args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE, fig$x$spec$layers[[args$info$lgroup]], theme = fig$x$spec$theme)

  y <- if(freq) {
    hh$counts
  } else {
    hh$density
  }

  do.call(ly_rect, c(
    list(
      fig = fig,
      xleft = hh$breaks[-length(hh$breaks)],
      xright = hh$breaks[-1], ytop = y, ybottom = 0,
      xlab = args$info$xName, ylab = args$info$yName,
      lname = args$info$lname, lgroup = args$info$lgroup
    ),
    args$params
  ))
}


#' Add a "density" layer to a Bokeh figure
#'
#' Draws a kernel density estimate
#' @param fig figure to modify
#' @param x,bw,adjust,kernel,weights,window,n,cut,na.rm parameters passed to \code{\link[stats]{density}}
#' @param data an optional data frame, providing the source for x
#' @template par-lineprops
#' @param legend text to display in the legend entry for the density line
#' @template par-lnamegroup
#' @template dots-line
#' @family layer functions
#' @export
ly_density <- function(
  fig, x, data = NULL,
  bw = "nrd0", adjust = 1,
  kernel = c("gaussian", "epanechnikov", "rectangular", "triangular",
    "biweight", "cosine", "optcosine"),
  weights = NULL, window = kernel, n = 512, cut = 3, na.rm = FALSE,
  color = "black", alpha = 1, width = 1, type = 1,
  legend = NULL, lname = NULL, lgroup = NULL, ...
) {

  validate_fig(fig, "ly_density")

  args <- sub_names(fig, data,
    grab(
      x,
      color, # TODO If I supply color, it should stack or dodge by default
      alpha,
      width,
      type,
      legend, lname, lgroup,
      dots = lazy_dots(...)
    )
  )

  # data was moved to 'y' position as only 'x' was supplied to sub_names
  args$data$x <- args$data[[2]]; args$data[[2]] <- NULL
  args$info$xName <- args$info$yName
  args$info$yName <- "Density"


  ## b_eval will repeat these, but the line glyph doesn't like this
  if(length(unique(args$params$color)) == 1)
    args$params$color <- subset_with_attributes(args$params$color, 1)
  if(length(unique(args$params$type)) == 1)
    args$params$type <- subset_with_attributes(args$params$type, 1)
  if(length(unique(args$params$width)) == 1)
    args$params$width <- subset_with_attributes(args$params$width, 1)

  args$params <- resolve_line_args(fig, args$params)

  dd <- stats::density.default(x = args$data$x, bw = bw, adjust = adjust, kernel = kernel, n = n, cut = 3, na.rm = na.rm)

  do.call(ly_lines, c(
    list(
      fig = fig,
      x = dd$x, y = dd$y,
      xlab = args$info$xName, ylab = args$info$yName
    ), args$params)
  )
}

# ly_rug


#' Add a "quantile" layer to a Bokeh figure
#'
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
ly_quantile <- function(
  fig, x, group = NULL, data = NULL,
  probs = NULL, distn = qunif, ncutoff = 200,
  color = NULL, alpha = 1,
  legend = TRUE, lname = NULL, lgroup = NULL, ...
) {

  validate_fig(fig, "ly_quantile")

  args <- sub_names(fig, data,
    grab(
      x,
      group,
      color,
      alpha,
      legend, lname, lgroup,
      dots = lazy_dots(...)
    )
  )
  # sub_names moves data into 'y' position as only 'x' is supplied
  args$data$x <- args$data[[2]]
  args$info$xName <- "f-value"
  # args$info$yName <- deparse(substitute(x)) # already done!

  if(is.null(args$info$group)) {
    args$info$group <- rep(1, length(args$data$x))
  }

  na_idx <- is.na(args$data$x)
  args$data$x <- args$data$x[!na_idx]
  args$info$group <- args$info$group[!na_idx]

  idx <- split(seq_along(args$data$x), args$info$group)

  ## quantile plot with no groups needs explicit legend
  ## but with groups, legend can simply be "TRUE" in which case
  ## an entry is automatically added for each group
  if(length(idx) == 1) {
    if(is.logical(args$info$legend))
      args$info$legend <- NULL
  }

  for(ii in idx) {
    if(length(ii) > 0) {
      if(is.null(probs)) {
        ## if the vector is too long, perhaps should default
        ## to some length, like 1000
        if(length(ii) > ncutoff) {
          cur_probs <- ppoints(ncutoff)
          qq <- quantile(args$data$x[ii], cur_probs, names = FALSE, na.rm = TRUE)
        } else {
          cur_probs <- ppoints(length(args$data$x[ii]))
          qq <- sort(args$data$x[ii])
        }
      } else {
        cur_probs <- probs
        qq <- quantile(args$data$x[ii], cur_probs, names = FALSE, na.rm = TRUE)
      }
      ff <- distn(cur_probs)

      cur_legend <- NULL
      if(is.logical(args$info$legend)) {
        if(args$info$legend) {
          cur_legend <- args$info$group[[ii[1]]]
        }
      } else {
        cur_legend <- args$info$legend
      }

      fig <- do.call(ly_points, c(
        list(
          fig = fig, x = ff, y = qq,
          xlab = args$info$xName, ylab = args$info$yName,
          lgroup = args$info$lgroup, legend = cur_legend
        ),
        args$params
      ))
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
ly_boxplot <- function(
  fig, x, y = NULL, data = NULL,
  coef = 1.5,
  color = "blue", alpha = 1,
  lname = NULL, lgroup = NULL, ...
) {

  validate_fig(fig, "ly_boxplot")

  args <- sub_names(fig, data,
    grab(
      x, y,
      color,
      alpha,
      # legend, # no legend?
      lname, lgroup,
      dots = lazy_dots(...)
    )
  )

  if (is.factor(args$data$x)) {
    args$data$x <- as.character(args$data$x)
  }
  if (is.factor(args$data$y)) {
    args$data$y <- as.character(args$data$y)
  }

  args$params <- resolve_color_alpha(args$params, has_line = TRUE,
    has_fill = TRUE, theme = fig$x$spec$theme)

  fill_ind <- grepl("^fill_", names(args$params))

  # pull out x and y as they are used a lot
  x <- args$data$x
  y <- args$data$y

  if(is.null(y)) {
    xName <- " "
    yName <- args$info$xName
    group <- rep(xName, length(x))
  } else {
    num_ind <- c(is.numeric(x), is.numeric(y))
    if(all(num_ind)) {
      message("both x and y are numeric -- choosing numeric variable based on which has the most unique values")
      if(length(unique(x)) > length(unique(y))) {
        xName <- args$info$yName
        yName <- args$info$xName
        group <- y
      } else {
        xName <- args$info$xName
        yName <- args$info$yName
        group <- x
        x <- y
      }
    } else if(num_ind[1]) {
      xName <- args$info$yName
      yName <- args$info$xName
      group <- y
    } else if(num_ind[2]) {
      xName <- args$info$xName
      yName <- args$info$yName
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

    fig <- do.call(ly_crect, c(
      list(
        fig = fig, x = rep(gp, 2), y = c(md1, md2),
        width = 0.9, height = c(hgt1, hgt2),
        xlab = xName, ylab = yName
      ),
      args$params
    ))
    fig <- do.call(ly_segments, c(
      list(
        fig = fig,
        x0 = c(gp, gp, gpr, gpr),
        y0 = c(bp$stats[1], bp$stats[4], bp$stats[1], bp$stats[5]),
        x1 = c(gp, gp, gpl, gpl),
        y1 = c(bp$stats[2], bp$stats[5], bp$stats[1], bp$stats[5])
      ),
      args$params[!fill_ind])
    )

    if(length(bp$out) > 0) {
      fig <- do.call(ly_points, c(
        list(
          fig = fig,
          x = rep(gp, length(bp$out)), y = bp$out,
          type = 1
        ),
        args$params
      ))
    }
  }

  fig
}

# ly_violin

# ly_bar

# ly_dotplot

# ly_rug
