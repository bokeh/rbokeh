#' Update the dimensions of a bokeh plot
#'
#' @param fig figure to resize
#' @param width desired width of figure, in pixels
#' @param height desired height of figure, in pixels
#' @export
update_size <- function(fig, width, height) {
  if(inherits(fig$x$spec, "BokehGridPlot")) {
    hmat <- fig$x$spec$hmat
    wmat <- fig$x$spec$wmat
    x_margin <- fig$x$spec$x_margin
    y_margin <- fig$x$spec$y_margin
    if(is.null(x_margin))
      x_margin <- 0
    if(is.null(y_margin))
      y_margin <- 0

    widths <- apply(wmat, 2, function(x) max(x, na.rm = TRUE))
    heights <- apply(hmat, 1, function(x) max(x, na.rm = TRUE))
    full_width <- sum(widths)
    full_height <- sum(heights)

    new_width_factor <- (width - x_margin - 46) / full_width
    new_height_factor <- (height - y_margin) / full_height

    new_wmat <- wmat * new_width_factor
    new_hmat <- hmat * new_height_factor
    new_wmat[,1] <- new_wmat[,1] + x_margin
    new_hmat[nrow(new_hmat),] <- new_hmat[nrow(new_hmat),] + y_margin

    fig$width <- sum(apply(new_wmat, 2, function(x) max(x, na.rm = TRUE))) + 46
    fig$height <- sum(apply(new_hmat, 1, function(x) max(x, na.rm = TRUE)))

    for(ii in seq_along(fig$x$spec$plot_refs)) {
      for(jj in seq_along(fig$x$spec$plot_refs[[ii]])) {
        if(!is.null(fig$x$spec$plot_refs[[ii]][[jj]])) {
          cur_id <- fig$x$spec$plot_refs[[ii]][[jj]]$id
          fig$x$spec$figs[[cur_id]]$width <- new_wmat[ii, jj]
          fig$x$spec$figs[[cur_id]]$height <- new_hmat[ii, jj]
          if(jj == 1) {
            fig$x$spec$figs[[cur_id]]$x$spec$model$plot$attributes$min_border_left <- x_margin
          }
          if(ii == length(fig$x$spec$plot_refs)) {
            fig$x$spec$figs[[cur_id]]$x$spec$model$plot$attributes$min_border_bottom <- y_margin
          }
        }
      }
    }
  } else if(inherits(fig$x$spec, "BokehFigure")) {
    fig$width <- width
    fig$height <- height
  }
  fig
}
