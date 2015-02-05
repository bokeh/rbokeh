#' Plot a Bokeh JSON specification
#'
#' Take a path to a Bokeh JSON plot specification file and render it in the browser.
#'
#' @import htmlwidgets
#' @importFrom RJSONIO fromJSON
#'
#' @export
bokeh_render_json <- function(json_file, width = NULL, height = NULL) {

  # extract the id
  tmp <- RJSONIO::fromJSON(json_file)
  nms <- sapply(tmp, function(x) { x$type })
  modeltype <- ifelse("GridPlot" %in% nms, "GridPlot", "Plot")
  id <- tmp[[which(nms %in% modeltype)[1]]]$id

  # forward options using x
  x = list(
    all_models = paste(readLines(json_file), collapse = "\n"),
    modeltype = modeltype,
    elementid = digest(Sys.time()),
    modelid = id
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'rbokeh',
    x,
    width = width,
    height = height,
    package = 'rbokeh'
  )
}
