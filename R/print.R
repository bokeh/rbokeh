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

  sapply(fig$x$spec$model, function(x) {
    x$type
  })

  fig$x$docid <- "qwertqwert"

  fig$x$docs_json <- list(list(
    version = "0.11.0",
    title = "Bokeh Application",
    roots = list(
      root_ids = list(fig$x$modelid),
      references = fig$x$spec$model
  )))
  fig$x$docs_json[[1]]$roots$references <- remove_model_names(fig$x$docs_json[[1]]$roots$references)
  names(fig$x$docs_json) <- fig$x$docid
  fig$x$spec <- NULL
  fig$preRenderHook <- NULL

  # attr(fig$x, "TOJSON_ARGS") <- list(auto_unbox = FALSE)

  ## we need to preserve "NaN" (which is NA in R) for bokeh to render properly
  # attr(fig$x, "TOJSON_ARGS") <- list(na = "string")
  ## actually this causes problems with categorical and date data
  ## se we'll handle it in the js prior to rendering

  fig
}

