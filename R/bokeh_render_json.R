#' Plot a Bokeh JSON specification
#'
#' Take a path to a Bokeh JSON plot specification file and render it in the browser.
#'
#' @note This is mainly useful for development / debugging purposes for reading in json created from another platform like Python, or to be used with tweaking json output from \code{\link{print_model_json}}.
#'
#' @param json_file path to json file
#'
#' @seealso \code{\link{print_model_json}}
#' @import htmlwidgets
#' @importFrom RJSONIO fromJSON
#'
#' @export
bokeh_render_json <- function(json_file) {

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
    modelid = id,
    isJSON = TRUE
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'rbokeh',
    x,
    width = NULL,
    height = NULL,
    package = 'rbokeh'
  )
}
