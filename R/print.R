#' Print a Bokeh Figure
#'
#' @param x Bokeh figure
#' @param \ldots further arguments - most importantly \code{debug = TRUE}
#' will print information about the figure in the javascript console in the
#' web browser
#'
#' @export
print.BokehFigure <- function(x, ...) {
  print(plot(x, y = NULL, ...))
}

#'@export
plot.BokehFigure <- function(x, y, ...) {
  dots <- list(...)
  debug <- dots$debug
  if(is.null(debug))
    debug <- FALSE

  if(length(x$layers) == 0) {
    message("This figure is empty...")
  } else {

    fig <- prepare_figure(x)

    id <- fig$model[[which(sapply(fig$model, function(x) x$type) == "Plot")]]$id

    ## create widget
    htmlwidgets::createWidget(
       name = 'rbokeh',
       list(
          r_debug = debug,
          all_models = get_json(fig$model),
          elementid = digest(Sys.time()),
          modeltype = fig$modeltype,
          modelid = id
       ),
       width = fig$width + 50,
       height = fig$height + 50,
       package = 'rbokeh'
    )
  }
}

# Reusable function for registering a set of methods with S3 manually. The
# methods argument is a list of character vectors, each of which has the form
# c(package, genname, class).
registerMethods <- function(methods) {
  lapply(methods, function(method) {
    pkg <- method[[1]]
    generic <- method[[2]]
    class <- method[[3]]
    func <- get(paste(generic, class, sep="."))
    if (pkg %in% loadedNamespaces()) {
      registerS3method(generic, class, func, envir = asNamespace(pkg))
    }
    setHook(
      packageEvent(pkg, "onLoad"),
      function(...) {
        registerS3method(generic, class, func, envir = asNamespace(pkg))
      }
    )
  })
}

knit_print.BokehFigure <- function(x, ..., options = NULL) {
  knitr::knit_print(htmlwidgets:::toHTML(plot(x), standalone = FALSE, knitrOptions = options), options = options,  ...)
}

