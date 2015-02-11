#' Print the JSON of a Bokeh figure
#' @param fig figure to print
#' @param prepare logical - should the figure be sent through preparations that need to be done prior to plotting (TRUE), or printed as-is (FALSE)
#' @param pretty parameter passed on to \code{\link[RJSONIO]{toJSON}}
#' @param file parameter passed on to \code{\link[base]{cat}}
#' @param pbcopy logical - if on OSX, should the results be passed to the clipboard (TRUE) instead of printed to the screen (FALSE)?
#' @examples
#' \donttest{
#' p <- figure() %>% ly_points(1:10) %>%
#'  tool_pan(dimensions = "height")
#' print_model_json(p)
#' }
#' @importFrom RJSONIO toJSON
#' @export
print_model_json <- function(fig, prepare = TRUE, pretty = TRUE, file = "", pbcopy = FALSE) {
  if(prepare) {
    if(inherits(fig, "BokehFigure")) {
      fig <- prepare_figure(fig)
    } else if(inherits(fig, "BokehGridPlot")) {
      fig <- prepare_gridplot(fig)
    }
  }

  if(pbcopy)
    file <- pipe("pbcopy")
  cat(toJSON(remove_model_names(fig$model), digits = 50, pretty = pretty), file = file)
}

base_model_object <- function(type, id) {
  list(
    model = list(
      type = type,
      id = id,
      attributes = list(
        id = id,
        tags = list(),
        doc = NULL
      )
    ),
    ref = list(
      type = type,
      id = id
    )
  )
}

#' @importFrom digest digest
gen_id <- function(obj, name = NULL) {
  digest(c(name, obj$time))
}

remove_model_names <- function(obj) {
  names(obj$plot$attributes$tools) <- NULL
  names(obj$plot$attributes$renderers) <- NULL
  names(obj) <- NULL
  obj
}

underscore2camel <- function(x) {
  x <- gsub("^([a-zA-Z])", "\\U\\1", x, perl = TRUE)
  gsub("_([a-zA-Z])", "\\U\\1", x, perl = TRUE)
}
