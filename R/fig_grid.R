
gridPlotModel <- function(id, plotRefs, toolEventRef, width, height) {
  res <- base_model_object("GridPlot", id)

  res$model$attributes$children <- plotRefs
  res$model$attributes$tool_events <- toolEventRef

  res$model$attributes$plot_width <- width
  res$model$attributes$plot_height <- height
  res$model$attributes$x_range <- NULL
  res$model$attributes$y_range <- NULL
  res$model$attributes$left <- list()
  res$model$attributes$below <- list()
  res$model$attributes$right <- list()
  res$model$attributes$above <- list()
  res$model$attributes$renderers <- list()
  res$model$attributes$extra_y_ranges <- structure(list(), .Names = character(0))
  res$model$attributes$extra_x_ranges <- structure(list(), .Names = character(0))

  res
}

## for linked pan / zoom, set each plot's Range1d to be the same
## and adjust limits

## a list of BokehFigure objects
## can be a list of lists in which case each element of the master list
## is interpreted as one row of the grid
## or it can be a single long list with additional arguments nrow, ncol, and byrow
#' @export
makeGrid <- function(objs, nrow = 1, ncol = 1, byrow = TRUE, same_y = FALSE, same_x = FALSE) {
  if(!is.list(objs))
    stop("'objs' must be a list")

  if(inherits(objs[[1]], "BokehFigure")) {
    ## list of BokehFigure objects
    ok <- sapply(objs, function(x) inherits(x, "BokehFigure"))
    if(!all(ok))
      stop("'objs' argument to makeGrid must be a list of BokehFigure objects or a list of lists of BokehFigure objects", call. = FALSE)

    nn <- length(objs)
    if((nrow * ncol) < nn) {
      if(byrow) {
        ncol <- ceiling(nn / nrow)
      } else {
        nrow <- ceiling(nn / ncol)
      }
    }

    tmp <- lapply(objs, function(x) {
      x$model$plot[c("type", "subtype", "id")]
    })

    plotRefs <- list()
    length(plotRefs) <- nrow
    for(ii in nrow) {
      curIdx <- ((ii - 1) * ncol + 1):(min(ii * ncol, nn))
      plotRefs[[ii]] <- tmp[curIdx]
    }
  } else {
    ## list of lists of BokehFigure objects
    ok <- sapply(objs, function(x) {
      all(sapply(x, function(y) inherits(y, "BokehFigure")))
    })
    if(!all(ok))
      stop("'objs' argument to makeGrid must be a list of BokehFigure objects or a list of lists of BokehFigure objects", call. = FALSE)

    ## get plot refs
    plotRefs <- lapply(objs, function(x) {
      lapply(x, function(y) {
        y$model$plot[c("type", "subtype", "id")]
      })
    })
    objs <- unlist(objs, recursive = FALSE)
  }

  structure(list(
    plotRefs = plotRefs,
    figs = objs,
    nrow = nrow, ncol = ncol), class = "BokehGridPlot")
}

# p1 <- figure() %>%
#   ly_point(Sepal.Length, Sepal.Width, data = iris,
#     color = Species, hover = list(Sepal.Length, Sepal.Width))

# p2 <- figure() %>%
#   ly_point(Petal.Length, Petal.Width, data = iris,
#     color = Species, hover = list(Sepal.Length, Sepal.Width))

# gd <- makeGrid(list(p1, p2))


## add a figure to a BokehGridPlot object/
## obj must be a BokehGridPlot and p must be a BokehFigure object
# addPlot <- function(obj, p, row = NULL, col = NULL, same_y = FALSE, same_x = FALSE) {
#   ## warn if overwriting a plot

# }

## run prepare on all figures in the grid
## merge axes and ranges if necessary
prepare_gridplot <- function(obj) {
  ## get overall width / height
  dims <- lapply(obj$figs, function(x) {
    list(
      id     = x$model$plot$id,
      width  = x$model$plot$attributes$plot_width,
      height = x$model$plot$attributes$plot_height
    )
  })
  names(dims) <- sapply(dims, function(x) x$id)

  wmat <- matrix(0, nrow = obj$nrow, ncol = obj$ncol)
  hmat <- matrix(0, nrow = obj$nrow, ncol = obj$ncol)
  for(ii in seq_along(obj$plotRefs)) {
    for(jj in seq_along(obj$plotRefs[[ii]])) {
      wmat[ii, jj] <- dims[[obj$plotRefs[[ii]][[jj]]$id]]$width
      hmat[ii, jj] <- dims[[obj$plotRefs[[ii]][[jj]]$id]]$height
    }
  }
  width <- sum(apply(wmat, 2, max))
  height <- sum(apply(hmat, 2, max))

  mod <- unlist(lapply(obj$figs, function(x) prepare_figure(x)$model), recursive = FALSE)

  id <- genId(list(time = Sys.time()), "GridPlot")
  tid <- genId(list(time = Sys.time()), c("GridPlot", "tool"))
  toolEvt <- toolEvents(tid)

  mod$GridPlot <- gridPlotModel(id, obj$plotRefs, toolEvt$ref, width, height)$model
  mod$toolEvt <- toolEvt$model
  names(mod) <- NULL

  list(model = mod, width = width, height = height, id = id)
}

