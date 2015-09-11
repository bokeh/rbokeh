rbokeh_prerender <- function(fig) {
  # dots <- list(...)
  # debug <- dots$debug
  # if(is.null(debug))
  #   debug <- FALSE

  if(fig$x$modeltype %in% c("Plot", "GMapPlot")) {
    if(length(fig$x$spec$layers) == 0 && fig$x$spec$model$plot$type != "GMapPlot") {
      message("This figure is empty...")
      return(NULL)
    } else {
      fig <- prepare_figure(fig)
    }
  } else if(fig$x$modeltype == "GridPlot") {
    fig <- prepare_gridplot(fig)
  } else {
    stop("Unsupported model type: ", fig$x$modeltype)
  }

  fig$x$all_models <- fig$x$spec$model
  fig$x$all_models <- remove_model_names(fig$x$all_models)
  fig$x$spec <- NULL
  fig$preRenderHook <- NULL

  ## we need to preserve "NaN" (which is NA in R) for bokeh to render properly
  attr(fig$x, "TOJSON_ARGS") <- list(na = "string")

  fig
}

