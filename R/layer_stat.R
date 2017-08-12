
# #' Add a "hist" layer to a Bokeh figure
# #'
# #' Draws a histogram
# #' @param fig figure to modify
# #' @param x either a vector to be passed to \code{\link[graphics]{hist}} or an object of class "histogram"
# #' @param breaks,freq,include.lowest,right parameters passed to \code{\link[graphics]{hist}}
# #' @param data an optional data frame, providing the source for x
# # template par-coloralpha
# # template par-lnamegroup
# # template dots-fillline
# #' @family layer functions
# #' @examples
# #' h <- figure(width = 600, height = 400) %>%
# #'   ly_hist(eruptions, data = faithful, breaks = 40, freq = FALSE) %>%
# #'   ly_density(eruptions, data = faithful)
# #' h
# #' @export
# ly_hist <- function(
#   fig, x, data = figure_data(fig),
#   breaks = "Sturges", freq = TRUE, include.lowest = TRUE, right = TRUE,
#   color = NULL, alpha = 1,
#   lname = NULL, lgroup = NULL, visible = TRUE, ...
# ) {

#   validate_fig(fig, "ly_hist")

#   args <- sub_names(fig, data,
#     grab(
#       x,
#       color, # TODO If i supply color, it should stack or dodge by default
#       alpha,
#       # no legend?
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...)
#     )
#   )

#   tryres <- try(identity(x), silent = TRUE)

#   if (inherits(tryres, "histogram")) {
#     hh <- x
#     args$info$x_name <- x$xname
#   } else {
#     # was moved to position of "y" as only "x" was supplied.  (inside sub_names)
#     # moving values from "y" to "x"
#     hh <- graphics::hist.default(x = args$data[[2]], breaks = breaks,
#       include.lowest = include.lowest, right = right, plot = FALSE)
#     args$info$x_name <- args$info$y_name
#   }
#   args$info$y_name <- ifelse(freq, "Frequency", "Density")

#   args$params <- resolve_color_alpha(args$params, has_line = TRUE, has_fill = TRUE,
#     fig$x$spec$layers[[args$info$lgroup]], theme = fig$x$spec$theme)

#   y <- if (freq) {
#     hh$counts
#   } else {
#     hh$density
#   }

#   do.call(ly_rect, c(
#     list(
#       fig = fig,
#       xleft = hh$breaks[-length(hh$breaks)],
#       xright = hh$breaks[-1], ytop = y, ybottom = 0,
#       xlab = args$info$x_name, ylab = args$info$y_name,
#       lname = args$info$lname, lgroup = args$info$lgroup
#     ),
#     args$params
#   ))
# }

# #' Add a "density" layer to a Bokeh figure
# #'
# #' Draws a kernel density estimate
# #' @param fig figure to modify
# #' @param x,bw,adjust,kernel,weights,window,n,cut,na.rm parameters passed to \code{\link[stats]{density}}
# #' @param data an optional data frame, providing the source for x
# # template par-lineprops
# #' @param legend text to display in the legend entry for the density line
# # template par-lnamegroup
# # template dots-line
# #' @family layer functions
# #' @examples
# #' h <- figure(width = 600, height = 400) %>%
# #'   ly_hist(eruptions, data = faithful, breaks = 40, freq = FALSE) %>%
# #'   ly_density(eruptions, data = faithful)
# #' h
# #' @export
# ly_density <- function(
#   fig, x, data = figure_data(fig),
#   bw = "nrd0", adjust = 1,
#   kernel = c("gaussian", "epanechnikov", "rectangular", "triangular",
#     "biweight", "cosine", "optcosine"),
#   weights = NULL, window = kernel, n = 512, cut = 3, na.rm = FALSE,
#   color = "black", alpha = 1, width = 1, type = 1,
#   legend = NULL, lname = NULL, lgroup = NULL, visible = TRUE,
#   ...
# ) {

#   validate_fig(fig, "ly_density")

#   args <- sub_names(fig, data,
#     grab(
#       x,
#       color, # TODO If I supply color, it should stack or dodge by default
#       alpha,
#       width,
#       type,
#       legend,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...)
#     )
#   )

#   # data was moved to 'y' position as only 'x' was supplied to sub_names
#   args$data$x <- args$data[[2]]; args$data[[2]] <- NULL
#   args$info$x_name <- args$info$y_name
#   args$info$y_name <- "Density"


#   ## b_eval will repeat these, but the line glyph doesn't like this
#   if (length(unique(args$params$color)) == 1)
#     args$params$color <- subset_with_attributes(args$params$color, 1)
#   if (length(unique(args$params$type)) == 1)
#     args$params$type <- subset_with_attributes(args$params$type, 1)
#   if (length(unique(args$params$width)) == 1)
#     args$params$width <- subset_with_attributes(args$params$width, 1)

#   args$params <- resolve_line_args(fig, args$params)

#   dd <- stats::density.default(x = args$data$x, bw = bw, adjust = adjust,
#     kernel = kernel, n = n, cut = 3, na.rm = na.rm)

#   do.call(ly_lines, c(
#     list(
#       fig = fig,
#       x = dd$x, y = dd$y,
#       xlab = args$info$x_name, ylab = args$info$y_name
#     ), args$params)
#   )
# }

# # ly_rug


# #' Add a "quantile" layer to a Bokeh figure
# #'
# #' Draws quantiles
# #' @param fig figure to modify
# #' @param x numeric vector or field name of variable to compute sample quantiles for
# #' @param group values or field name of a grouping variable to break quantile computations up by
# #' @param data an optional data frame, providing the source for x
# #' @param probs numeric vector of probabilities with values in \code{[0,1]} at which to compute quantiles - if \code{NULL}, every point of \code{x} is a quantile
# #' @param distn quantile function to use on the x-axis (e.g. \code{\link[stats]{qnorm}}) - default is \code{\link[stats]{qunif}},
# #' @param ncutoff if the length of \code{x} exceeds this value and \code{probs} is not specified, compute quantiles at \code{ncutoff} points
# # template par-coloralpha
# # template par-legend
# # template par-lnamegroup
# # template dots-fillline
# #' @family layer functions
# #' @examples
# #' figure(legend_location = "top_left") %>%
# #'   ly_quantile(Sepal.Length, group = Species, data = iris)
# #' @export
# ly_quantile <- function(
#   fig, x, group = NULL, data = figure_data(fig),
#   probs = NULL, distn = stats::qunif, ncutoff = 200,
#   color = NULL, alpha = 1,
#   legend = TRUE, lname = NULL, lgroup = NULL, visible = TRUE,
#   ...
# ) {

#   validate_fig(fig, "ly_quantile")

#   args <- sub_names(fig, data,
#     grab(
#       x,
#       group,
#       color,
#       alpha,
#       legend,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...)
#     )
#   )
#   # sub_names moves data into 'y' position as only 'x' is supplied
#   args$data$x <- args$data[[2]]
#   args$info$x_name <- "f-value"
#   # args$info$y_name <- deparse(substitute(x)) # already done!

#   if (is.null(args$info$group)) {
#     args$info$group <- rep(1, length(args$data$x))
#   }

#   na_idx <- is.na(args$data$x)
#   args$data$x <- args$data$x[!na_idx]
#   args$info$group <- args$info$group[!na_idx]

#   idx <- split(seq_along(args$data$x), args$info$group)

#   ## quantile plot with no groups needs explicit legend
#   ## but with groups, legend can simply be "TRUE" in which case
#   ## an entry is automatically added for each group
#   if (length(idx) == 1) {
#     if (is.logical(args$info$legend))
#       args$info$legend <- NULL
#   }

#   for (ii in idx) {
#     if (length(ii) > 0) {
#       if (is.null(probs)) {
#         ## if the vector is too long, perhaps should default
#         ## to some length, like 1000
#         if (length(ii) > ncutoff) {
#           cur_probs <- stats::ppoints(ncutoff)
#           qq <- stats::quantile(args$data$x[ii], cur_probs, names = FALSE, na.rm = TRUE)
#         } else {
#           cur_probs <- stats::ppoints(length(args$data$x[ii]))
#           qq <- sort(args$data$x[ii])
#         }
#       } else {
#         cur_probs <- probs
#         qq <- stats::quantile(args$data$x[ii], cur_probs, names = FALSE, na.rm = TRUE)
#       }
#       ff <- distn(cur_probs)

#       cur_legend <- NULL
#       if (is.logical(args$info$legend)) {
#         if (args$info$legend) {
#           cur_legend <- args$info$group[[ii[1]]]
#         }
#       } else {
#         cur_legend <- args$info$legend
#       }

#       fig <- do.call(ly_points, c(
#         list(
#           fig = fig, x = ff, y = qq,
#           xlab = args$info$x_name, ylab = args$info$y_name,
#           lgroup = args$info$lgroup, legend = cur_legend
#         ),
#         args$params
#       ))
#     }
#   }
#   fig
# }

# #' Add a "boxplot" layer to a Bokeh figure
# #' @param fig figure to modify
# #' @param x either a numeric vector or a factor
# #' @param y either a numeric vector or a factor
# #' @param data an optional data frame, providing the source for x and y
# #' @param width with of each box, a value between 0 (no width) and 1 (full width)
# #' @param coef see \code{\link[grDevices]{boxplot.stats}}
# #' @param outlier_glyph the glyph used to plot the outliers. If set to
# #'   \code{NA}, no outlier points are plotted. Run \code{point_types()} for
# #'   possible values.
# #' @param outlier_size the size of the glyph used to plot outliers. If set to
# #'   \code{NA}, no outlier points are plotted.
# # template par-coloralpha
# # template par-lnamegroup
# # template dots-fillline
# #' @family layer functions
# #' @examples
# #' figure(ylab = "Height (inches)", width = 600) %>%
# #'   ly_boxplot(voice.part, height, data = lattice::singer)
# #' @export
# ly_boxplot <- function(
#   fig, x, y = NULL, data = figure_data(fig),
#   width = 0.9, coef = 1.5,
#   color = "blue", alpha = 1,
#   outlier_glyph = 1, outlier_size = 10,
#   lname = NULL, lgroup = NULL, visible = TRUE,
#   ...
# ) {

#   validate_fig(fig, "ly_boxplot")

#   args <- sub_names(fig, data,
#     grab(
#       x, y,
#       color,
#       alpha,
#       # legend, # no legend?
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...)
#     )
#   )

#   if (missing(y)) {
#     args$data$x <- args$data$y
#     args$data$y <- NULL
#     args$info$x_name <- args$info$y_name
#     args$info$y_name <- NULL
#   }

#   if (is.factor(args$data$x)) {
#     args$data$x <- as.character(args$data$x)
#   }
#   if (is.factor(args$data$y)) {
#     args$data$y <- as.character(args$data$y)
#   }

#   args$params <- resolve_color_alpha(args$params, has_line = TRUE,
#     has_fill = TRUE, theme = fig$x$spec$theme)

#   # fill_ind <- grepl("^fill_", names(args$params))

#   # pull out x and y as they are used a lot
#   x <- args$data$x
#   y <- args$data$y

#   group_is_numeric <- FALSE

#   if (is.null(y)) {
#     x_name <- " "
#     y_name <- args$info$x_name
#     group <- rep(x_name, length(x))
#   } else {
#     num_ind <- c(is.numeric(x), is.numeric(y))
#     if (all(num_ind)) {
#       group_is_numeric <- TRUE
#       message(
#         "both x and y are numeric -- choosing numeric variable based on ",
#         "which has the most unique values")
#       if (length(unique(x)) > length(unique(y))) {
#         x_name <- args$info$y_name
#         y_name <- args$info$x_name
#         group <- as.character(y)
#       } else {
#         x_name <- args$info$x_name
#         y_name <- args$info$y_name
#         group <- as.character(x)
#         x <- y
#       }
#     } else if (num_ind[1]) {
#       x_name <- args$info$y_name
#       y_name <- args$info$x_name
#       group <- y
#     } else if (num_ind[2]) {
#       x_name <- args$info$x_name
#       y_name <- args$info$y_name
#       group <- x
#       x <- y
#     } else {
#       stop("At least one of 'x' or 'y' should be numeric for ly_boxplot.")
#     }
#   }

#   idx <- split(seq_along(x), group)
#   for (ii in seq_along(idx)) {
#     bp <- grDevices::boxplot.stats(x = x[idx[[ii]]], coef = coef)

#     gp <- group[idx[[ii]][1]] ## doesn't work right now
#     ## for lines and whiskers
#     gpl <- paste(gp, ":0.4", sep = "")
#     gpr <- paste(gp, ":0.6", sep = "")
#     hgt1 <- bp$stats[3] - bp$stats[2]
#     md1 <- hgt1 / 2 + bp$stats[2]
#     hgt2 <- bp$stats[4] - bp$stats[3]
#     md2 <- hgt2 / 2 + bp$stats[3]

#     fig <- ly_crect(
#       fig = fig, x = rep(gp, 2), y = c(md1, md2),
#       width = width, height = c(hgt1, hgt2),
#       xlab = x_name, ylab = y_name,
#       visible = args$params$visible,
#       line_color = args$params$line_color,
#       fill_color = args$params$fill_color,
#       line_alpha = args$params$line_alpha,
#       fill_alpha = args$params$fill_alpha)

#     fig <- ly_segments(
#       fig = fig,
#       x0 = c(gp, gp, gpr, gpr),
#       y0 = c(bp$stats[1], bp$stats[4], bp$stats[1], bp$stats[5]),
#       x1 = c(gp, gp, gpl, gpl),
#       y1 = c(bp$stats[2], bp$stats[5], bp$stats[1], bp$stats[5]),
#       xlab = x_name, ylab = y_name,
#       visible = args$params$visible,
#       line_color = args$params$line_color,
#       line_alpha = args$params$line_alpha)

#     if (length(bp$out) > 0 && !(is.na(outlier_size) || is.na(outlier_glyph))) {
#       fig <- ly_points(
#         fig = fig,
#         x = rep(gp, length(bp$out)), y = bp$out,
#         glyph = rep(outlier_glyph, length(bp$out)),
#         size = outlier_size,
#         xlab = x_name, ylab = y_name,
#         visible = args$params$visible,
#         line_color = args$params$line_color,
#         fill_color = args$params$fill_color,
#         line_alpha = args$params$line_alpha,
#         fill_alpha = args$params$fill_alpha)
#     }
#   }

#   if (group_is_numeric && !fig$x$spec$has_x_axis)
#     fig <- fig %>% x_range(as.character(sort(unique(as.numeric(group)))))

#   fig
# }

# #' Add a "barchart" layer to a Bokeh figure
# #'
# #' Draws a bar chart
# #' @param fig figure to modify
# #' @param x values or field name for x variable, or if NULL, x-axis will be counts of y
# #' @param y values or field name for y variable, or if NULL, y-axis will be counts of x
# #' @param data an optional data frame, providing the source for inputs x, y, and color properties
# # template par-coloralpha
# #' @param position either "stack", "fill", or "dodge" (see details)
# #' @param width with of each bar, a value between 0 (no width) and 1 (full width)
# #' @param hover logical - should a hover tool be added to show the value of each bar?
# #' @param origin,breaks,right,binwidth parameters to be used for binning x when it is continuous (not yet implemented)
# # template par-lnamegroup
# # template par-legend
# # template dots-fillline
# #' @details
# #' This function expects one of either x or y to be categorical and the other to be numeric or NULL.  The numeric variable is summed for each categorical variable and bars are plotted.  If no numeric variable is supplied, the unique values of the categorical variable will be tabulated.  Within each categorical variable, if color maps to another grouping variable then the bars are split up.  In this case, there are three ways to display the bars with the \code{position} argument.  The default, "stack" will stack the bars.  The "fill" choice will show the relative proportion for each group within each categorical variable level, stacking the bars.  The "dodge" choice will plot the bars for each level of the categorical variable side by side.
# #'
# #' @family layer functions
# #' @example man-roxygen/ex-bar.R
# #' @export
# ly_bar <- function(
#   fig, x = NULL, y = NULL, data = figure_data(fig),
#   color = NULL, alpha = 1,
#   position = c("stack", "fill", "dodge"), width = 0.9, hover = FALSE,
#   origin = NULL, breaks = NULL, right = FALSE, binwidth = NULL,
#   lname = NULL, lgroup = NULL, legend = NULL, visible = TRUE, ...
# ) {

#   position <- match.arg(position)

#   validate_fig(fig, "ly_bar")

#   args <- sub_names(fig, data,
#     grab(
#       x,
#       y,
#       color,
#       alpha,
#       position,
#       width,
#       origin,
#       breaks,
#       right,
#       binwidth,
#       # url, # no url
#       legend,
#       lname,
#       lgroup,
#       visible,
#       dots = lazy_dots(...)
#     )
#   )

#   # will give NULL if it was not a variable
#   colorname <- attr(args$params$color, "stringName")

#   # we'll do everything as if x is the factor
#   # but if the y variable was specified as the factor we'll swap back
#   swap_axes <- FALSE

#   x_miss <- missing(x)
#   y_miss <- missing(y)

#   if (x_miss && y_miss) {
#     stop("must specify at least one of 'x' or 'y' for ly_bar", call. = FALSE)
#   }

#   if (y_miss) {
#     # when y is missing, it automatically makes y->x and makes x a sequence
#     args$data$x <- args$data$y
#     args$data$y <- rep(1, length(args$data$x))
#     args$info$x_name <- attr(args$data$x, "stringName")
#     args$info$y_name <- "count"
#   } else if (x_miss) {
#     args$data$x <- rep(1, length(args$data$y))
#     args$info$x_name <- "count"
#   }

#   if (!is.numeric(args$data$x) && is.numeric(args$data$y)) {
#     # nothing  to do
#   } else if (is.numeric(args$data$x) && !is.numeric(args$data$y)) {
#     tmp <- args$data$x
#     args$data$x <- args$data$y
#     args$data$y <- tmp
#     swap_axes <- TRUE
#   } else {
#     stop("in ly_bar one of 'x' or 'y' must be numeric and the other not numeric", call. = FALSE)
#   }

#   if (is.null(args$params$color) || length(args$params$color) == 1) {
#     res <- stats::aggregate(y ~ x, data = args$data, sum)
#   } else {
#     data_and_color <- args$data
#     data_and_color$color <- args$params$color
#     res <- stats::aggregate(y ~ x + color, data = data_and_color, sum)
#     if (missing(legend)) {
#       args$info$legend <- TRUE
#     }
#   }

#   ## handle y values
#   ##---------------------------------------------------------

#   if (position == "stack") {
#     res <- do.call(rbind, by(res, res$x, function(a) {
#       a$ytop <- cumsum(a$y)
#       a$ybottom <- a$ytop - a$y
#       a
#     }))

#   } else if (position == "fill") {
#     res <- do.call(rbind, by(res, res$x, function(a) {
#       tmp <- a$y / sum(a$y)
#       a$p_ <- tmp
#       a$ytop <- cumsum(tmp)
#       a$ybottom <- a$ytop - tmp
#       a
#     }))

#   } else if (position == "dodge") {
#     res$ytop <- res$y
#     res$ybottom <- 0
#   }

#   ## handle x values
#   ##---------------------------------------------------------

#   if (position %in% c("stack", "fill")) {
#     res$xleft <- paste0(res$x, ":", 1 - width)
#     res$xright <- paste0(res$x, ":", width)
#   } else {
#     res <- do.call(rbind, by(res, res$x, function(a) {
#       nn <- nrow(a)
#       pts <- seq(1 - width, width, length = nn + 1)
#       a$xleft <- paste0(a$x, ":", utils::head(pts, nn))
#       a$xright <- paste0(a$x, ":", utils::tail(pts, nn))
#       a
#     }))
#   }

#   ind <- which(names(res) == "color")
#   if (length(ind) > 0) {
#     names(res)[ind] <- colorname
#   }

#   bad_param_names <- c("color", "origin", "breaks", "right", "binwidth", "position")
#   remaining_args <- args$params
#   remaining_args <- remaining_args[! (names(remaining_args) %in% bad_param_names)]

#   if (hover) {
#     hovdat <- data.frame(
#       variable = res$x,
#       value = res$y
#     )
#     if (position == "fill") {
#       hovdat$proportion <- res$p_
#     }
#     extra_names <- setdiff(names(res), c("x", "y", "ytop", "ybottom", "xleft", "xright"))
#     if (length(extra_names) > 0) {
#       hovdat <- cbind(hovdat, res[, extra_names, drop = FALSE])
#     }
#   } else {
#     hovdat <- NULL
#   }

#   # get rid of x and y as they are no longer needed
#   # and may conflict with xname, yname
#   res$x <- NULL
#   res$y <- NULL
#   res$p_ <- NULL

#   remaining_args$width <- NULL

#   # b_eval doesn't know how to deal with quoted inputs with a single-row data frame
#   # so instead tag the data so it knows the input is quoted
#   if (nrow(res) == 1)
#     class(res) <- c(class(res), "quoted")

#   color_value <- if (is.null(colorname)) args$params$color else colorname

#   if (swap_axes) {
#     ind <- match(c("ytop", "ybottom", "xright", "xleft"), names(res))
#     names(res)[ind] <- c("xright", "xleft", "ybottom", "ytop")
#   }

#   do.call(ly_rect, append(list(fig = fig,
#     xleft = "xleft", ybottom = "ybottom", xright = "xright", ytop = "ytop",
#     xlab = args$info$x_name, ylab = args$info$y_name,
#     data = res, hover = hovdat,
#     color = color_value,
#     lname = args$info$lname, lgroup = args$info$lgroup, legend = args$info$legend),
#     remaining_args), quote = TRUE) # quote = TRUE needed for lazy_
# }

# #' Add a "hexbin" layer to a Bokeh figure
# #' @param fig figure to modify
# #' @param x values or field name of center x coordinates to be binned
# #' @param y values or field name of center y coordinates to be binned
# #' @param data an optional data frame, providing the source for x and y
# #' @param xbins,shape,xbnds,ybnds parameters passed to \code{\link[hexbin]{hexbin}}
# #' @param style type of plotting for hexbins (see \code{\link[hexbin]{grid.hexagons}}) - "colorramp" and "lattice" are currently supported
# #' @param trans,inv transformation and inverse transformation function for the bin counts
# #' @param lname layer name
# #' @param palette name of color palette to use for color ramp (see \href{http://bokeh.pydata.org/en/latest/docs/reference/palettes.html}{here} for acceptable values)
# #' @param line logical - should hexagons have an outline?
# #' @param alpha the alpha transparency of the hexagons between 0 (transparent) and 1 (opaque)
# #' @param hover logical - should a hover tool be added to show the count in each hexagon?
# #' @param visible should the layer be visible?
# #' @examples
# #' \donttest{
# #' figure() %>% ly_hexbin(rnorm(10000), rnorm(10000))
# #' }
# #' @export
# ly_hexbin <- function(
#   fig, x, y = NULL, data = figure_data(fig),
#   xbins = 30, shape = 1, xbnds = NULL, ybnds = NULL,
#   style = "colorscale",
#   trans = NULL, inv = NULL, lname = NULL,
#   palette = "RdYlGn11", line = FALSE, alpha = 1,
#   hover = TRUE, visible = TRUE
# ) {

#   args <- sub_names(fig, data,
#     grab(
#       x,
#       y,
#       xbins,
#       shape,
#       # style,
#       # trans,
#       # inv,
#       # palette,
#       hover,
#       line,
#       alpha,
#       visible,
#       dots = lazy_dots()
#     ),
#     process_data_and_names = FALSE
#   )

#   minarea <- 0.04; maxarea <- 0.8; mincnt <- 1; maxcnt <- NULL

#   if (!inherits(args$data$x, "hexbin")) {
#     xy_names <- get_xy_names(args$data$x, args$data$y,
#       deparse(substitute(x)), deparse(substitute(y)), NULL)
#     xy <- get_xy_data(args$data$x, args$data$y)
#     args$data$x <- xy$x
#     args$data$y <- xy$y
#     args$info$x_name <- xy_names$x
#     args$info$y_name <- xy_names$y

#     hbd <- get_hexbin_data(x = xy$x, y = xy$y, xbins = xbins,
#       shape = shape, xbnds = xbnds, ybnds = ybnds)
#   } else {
#     args$info$x_name <- "x"
#     args$info$y_name <- "y"

#     hbd <- args$data$x
#   }

#   hbd <- get_from_hexbin(hbd, maxcnt = maxcnt,
#     mincnt = mincnt, trans = trans, inv = inv, style = style,
#     minarea = minarea, maxarea = maxarea)

#   if (is.character(palette)) {
#     if (valid_color(palette)) {
#       col <- palette
#     } else {
#       if (!palette %in% bk_gradient_palette_names)
#         stop(
#           "'palette' specified in ly_hexbin is not a valid color name or palette ",
#           "- see here: http://bokeh.pydata.org/en/latest/docs/reference/palettes.html",
#           call. = FALSE)
#       palette <- colorRampPalette(bk_gradient_palettes[[palette]])
#     }
#   }

#   if (is.function(palette)) {
#     colorcut <- seq(0, 1, length = 100)
#     # nc <- length(colorcut)
#     colgrp <- cut(hbd$rcnt, colorcut, labels = FALSE, include.lowest = TRUE)
#     clrs <- palette(length(colorcut) - 1)
#     col <- clrs[colgrp]
#   }

#   if (args$info$x_name == args$info$y_name) {
#     args$info$x_name <- paste(args$info$x_name, "(x)")
#     args$info$y_name <- paste(args$info$y_name, "(y)")
#   }
#   names(hbd$data)[1:2] <- c(args$info$x_name, args$info$y_name)

#   if (!line) {
#     line_color <- NA
#   } else {
#     # TODO
#     # this could be reached and never have been set
#     line_color <- col
#   }

#   if (is.logical(hover) && !hover)
#     hbd$data <- NULL

#   fig %>% ly_polygons(
#     xs = hbd$xs, ys = hbd$ys, color = NULL,
#     fill_color = col, alpha = NULL,
#     fill_alpha = args$params$alpha, line_color = line_color,
#     hover = hbd$data, xlab = args$info$x_name, ylab = args$info$y_name,
#     lname = lname
#   )
# }


# #' @importFrom hexbin hexbin
# #' @importFrom hexbin hcell2xy
# #' @importFrom hexbin hexcoords
# get_hexbin_data <- function(x, y, xbins = 30, shape = 1,
#   xbnds = range(x, na.rm = TRUE),
#   ybnds = range(y, na.rm = TRUE)) {

#   if (is.null(xbnds))
#     xbnds <- range(x, na.rm = TRUE)

#   if (is.null(ybnds))
#     ybnds <- range(y, na.rm = TRUE)

#   ind <- stats::complete.cases(x, y)
#   hexbin(x[ind], y[ind], shape = shape, xbins = xbins, xbnds = xbnds, ybnds = ybnds)
# }


# get_from_hexbin <- function(dat, maxcnt = NULL, mincnt = 1, trans = identity,
#   inv = identity, maxarea = 0.8, minarea = 0.04, style = style) {

#   cnt <- dat@count
#   xbins <- dat@xbins
#   shape <- dat@shape
#   tmp <- hcell2xy(dat)
#   if (is.null(maxcnt))
#     maxcnt <- max(dat@count)

#   ok <- cnt >= mincnt & cnt <= maxcnt

#   xnew <- tmp$x[ok]
#   ynew <- tmp$y[ok]
#   cnt <- cnt[ok]

#   sx <- xbins / diff(dat@xbnds)
#   sy <- (xbins * shape) / diff(dat@ybnds)

#   if (is.null(trans)) {
#      if (min(cnt, na.rm = TRUE) < 0) {
#         pcnt <- cnt + min(cnt)
#         rcnt <- {
#            if (maxcnt == mincnt) rep.int(1, length(cnt)) else (pcnt - mincnt) / (maxcnt - mincnt)
#         }
#      } else rcnt <- {
#         if (maxcnt == mincnt) rep.int(1, length(cnt)) else (cnt - mincnt) / (maxcnt - mincnt)
#      }
#   } else {
#      rcnt <- (trans(cnt) - trans(mincnt)) / (trans(maxcnt) - trans(mincnt))
#      if (any(is.na(rcnt))) stop("bad count transformation")
#   }

#   if (style == "lattice") {
#     area <- minarea + rcnt * (maxarea - minarea)
#     area <- pmin(area, maxarea)
#     radius <- sqrt(area)
#   } else {
#     radius <- rep(1, length(xnew))
#   }

#   inner <- 0.5
#   outer <- (2 * inner) / sqrt(3)
#   dx <- inner / sx
#   dy <- outer / (2 * sy)
#   # rad <- sqrt(dx^2 + dy^2)
#   hex_c <- hexcoords(dx, dy, sep = NULL)

#   xs <- lapply(seq_along(xnew), function(i)
#     hex_c$x * radius[i] + xnew[i])
#   ys <- lapply(seq_along(xnew), function(i)
#     hex_c$y * radius[i] + ynew[i])

#   list(xs = xs, ys = ys, data = data.frame(x = xnew, y = ynew, count = cnt), rcnt = rcnt)
# }

# # ly_violin

# # ly_dotplot

# # ly_rug
NULL
