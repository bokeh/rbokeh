
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

## for linked brushing, set each plot's data sources to be the same (where possible)

## for linked brushing, set each plot's data to be the same

## a list of BokehFigure objects
## can be a list of lists in which case each element of the master list
## is interpreted as one row of the grid
## or it can be a single long list with additional arguments nrow, ncol, and byrow
#' @export
grid_plot <- function(objs, nrow = 1, ncol = 1, byrow = TRUE, same_axes = FALSE, link_data = FALSE) {
  if(length(same_axes) == 1) {
    same_x <- same_y <- same_axes
  } else {
    same_x <- same_axes[1]
    same_y <- same_axes[2]
  }

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


  ## deal with axes
  x_range <- y_range <- NULL
  if(same_x) {
    x_range <- getGridRanges(objs, "x")
    for(ii in seq_along(objs)) {
      objs[[ii]]$xlim <- x_range$range # prevents prepare_figure() from computing range
      objs[[ii]]$has_x_range <- TRUE # prevents prepare_figure() from adding range
      objs[[ii]]$model$plot$attributes$x_range <- x_range$mod$ref
    }
  }
  if(same_y) {
    y_range <- getGridRanges(objs, "y")
    for(ii in seq_along(objs)) {
      objs[[ii]]$ylim <- y_range$range # prevents prepare_figure() from computing range
      objs[[ii]]$has_y_range <- TRUE # prevents prepare_figure() from adding range
      objs[[ii]]$model$plot$attributes$y_range <- y_range$mod$ref
    }
  }

  structure(list(
    plotRefs = plotRefs,
    figs = objs,
    x_range = x_range$mod$model,
    y_range = y_range$mod$model,
    link_data = link_data,
    nrow = nrow, ncol = ncol), class = "BokehGridPlot")
}

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

  figs <- lapply(obj$figs, prepare_figure)

  dataMods <- list()
  ## deal with linked data
  if(obj$link_data) {
    ## find data signatures that match
    sigs <- do.call(c, lapply(figs, function(x)
      unique(do.call(c, lapply(x$dataSigs, function(y) y$sig)))))
    sigst <- table(sigs)
    idx <- which(sigst > 1)
    if(length(idx) > 0) {
      ## take each data source that has at least one match
      ## find figures with this data source
      ## merge the data sources
      ## and then point the glyphrenderers of each to this new data source
      for(sig in names(idx)) {
        hasData <- list()
        for(ii in seq_along(figs)) {
          for(jj in seq_along(figs[[ii]]$dataSigs)) {
            if(!is.null(figs[[ii]]$dataSigs[[jj]]$sig))
              if(figs[[ii]]$dataSigs[[jj]]$sig == sig)
                hasData[[length(hasData) + 1]] <- list(index = c(ii, jj), glrId = figs[[ii]]$dataSigs[[jj]]$glrId)
          }
        }
        dId <- genId(NULL, sig)
        newDataRef <- list(type = "ColumnDataSource", id = dId)
        hd1 <- hasData[[1]]$index
        gl1 <- hasData[[1]]$glrId
        ds1 <- figs[[hd1[1]]]$model[[gl1]]$attributes$data_source$id
        d1 <- figs[[hd1[1]]]$model[[ds1]]$attributes$data
        figs[[hd1[1]]]$model[[gl1]]$attributes$data_source <- newDataRef
        newData <- d1
        figs[[hd1[1]]]$model[[ds1]] <- NULL
        ## do the naive thing for now and don't check for identical columns
        for(ii in seq_along(hasData)[-1]) {
          hd <- hasData[[ii]]$index
          glr <- hasData[[ii]]$glrId
          ds <- figs[[hd[1]]]$model[[glr]]$attributes$data_source$id
          gl <- figs[[hd[1]]]$model[[glr]]$attributes$glyph$id
          nsgl <- figs[[hd[1]]]$model[[glr]]$attributes$nonselection_glyph$id
          d <- figs[[hd[1]]]$model[[ds]]$attributes$data
          mergeNames <- intersect(names(d), c("x", "y", "fill_color", "fill_alpha", "line_color", "line_width", "line_alpha"))
          newNames <- paste0(mergeNames, ii)
          d2 <- d[mergeNames]
          names(d2) <- newNames
          newData <- c(newData, d2, d[setdiff(names(d), c(newNames, names(newData)))])
          ## update references
          upd <- figs[[hd[1]]]$model[[gl]]$attributes[mergeNames]
          for(nm in names(upd)) {
            if(!is.null(upd[[nm]]$field))
              upd[[nm]]$field <- paste0(upd[[nm]]$field, ii)
          }
          figs[[hd[1]]]$model[[gl]]$attributes[mergeNames] <- upd

          upd <- figs[[hd[1]]]$model[[nsgl]]$attributes[mergeNames]
          for(nm in names(upd)) {
            if(!is.null(upd[[nm]]$field))
              upd[[nm]]$field <- paste0(upd[[nm]]$field, ii)
          }
          figs[[hd[1]]]$model[[nsgl]]$attributes[mergeNames] <- upd

          figs[[hd[1]]]$model[[glr]]$attributes$data_source <- newDataRef
          figs[[hd[1]]]$model[[ds]] <- NULL
        }
        ## add this data source
        dataMods[[sig]] <- dataModel(newData, dId)
      }
    } else {
      message("'link_data' was set to TRUE, but none of the figures in the grid have the same data source.")
    }
  }

  mod <- unlist(lapply(figs, function(x) remove_model_names(x$model)), recursive = FALSE)

  id <- genId(list(time = Sys.time()), "GridPlot")
  tid <- genId(list(time = Sys.time()), c("GridPlot", "tool"))
  toolEvt <- toolEvents(tid)

  mod$GridPlot <- gridPlotModel(id, obj$plotRefs, toolEvt$ref, width, height)$model
  mod$toolEvt <- toolEvt$model
  mod$x_range <- obj$x_range
  mod$y_range <- obj$y_range

  mod$toolEvt <- toolEvt$model

  for(md in dataMods) {
    mod[[md$ref$id]] <- md$model
  }

  names(mod) <- NULL

  list(model = mod, width = width, height = height, id = id)
}

getGridRanges <- function(objs, which = "x") {
  w1 <- paste0("glyph", toupper(which), "Ranges")
  w2 <- paste0(which, "AxisType")
  ranges <- unlist(lapply(objs, function(x) x[[w1]]), recursive = FALSE)
  rng <- getAllGlyphRange(ranges, objs[[1]]$padding_factor, objs[[1]][[w2]])
  id <- genId(NULL, c(which, "GridRange"))
  list(range = rng, mod = rangeModel(ifelse(is.numeric(rng), "Range1d", "FactorRange"), id, rng))
}
