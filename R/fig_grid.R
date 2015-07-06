
#' Create a Bokeh grid plot from a list of Bokeh figures
#' @param figs list of Bokeh figures - see details for what is acceptable
#' @param width width of the entire grid plot in pixels - if \code{NULL}, the sum of the grid widths of columns will be used - if not \code{NULL}, the widths of the plots will be proportionately shrunk to meet the specified width
#' @param height height of the entire grid plot in pixels - if \code{NULL}, the sum of the grid heights of rows will be used - if not \code{NULL}, the heights of the plots will be proportionately shrunk to meet the specified height
#' @param nrow number of rows in the grid
#' @param ncol number of columns in the grid
#' @param byrow populate the grid by row according to the order of figure elements supplied in \code{params}
#' @param same_axes logical or vector of two logicals specifying whether the x and/or y axis limits should be the same for each plot in the grid
#' @param link_data logical - should an attempt be made to join the data sources of each plot for linked brushing? (see details)
#' @example man-roxygen/ex-grid.R
#' @details The \code{figs} parameter can either be a list of figures or a list of lists of figures.  If the latter, the list structure will determine the layout, with each super-list of figures defining a single row of the grid.  If the former, the parameters \code{nrow} and \code{ncol} and \code{byrow} are used to determine the layout.  The grid is from top to bottom left to right.
#'
#' If \code{link_data} is \code{TRUE}, then an effort will be made to link all data sources that are common among the different figures in the plot.  Note that at this point, only data sources that are specified in the \code{data} argument to the different layer functions are checked.
#' @export
grid_plot <- function(figs, width = NULL, height = NULL, nrow = 1, ncol = 1, byrow = TRUE, same_axes = FALSE, link_data = FALSE) {

  if(length(same_axes) == 1) {
    same_x <- same_y <- same_axes
  } else {
    same_x <- same_axes[1]
    same_y <- same_axes[2]
  }

  if(!is.list(figs))
    stop("'figs' must be a list")

  figs <- unname(figs)

  if(inherits(figs[[1]]$x$spec, "BokehFigure")) {
    ## list of BokehFigure objects
    ok <- sapply(figs, function(fig) inherits(fig$x$spec, "BokehFigure"))
    if(!all(ok))
      stop("'figs' argument to makeGrid must be a list of BokehFigure objects or a list of lists of BokehFigure objects", call. = FALSE)

    nn <- length(figs)
    if(missing(ncol))
      ncol <- ceiling(nn / nrow)
    if(missing(nrow))
      nrow <- ceiling(nn / ncol)

    if((nrow * ncol) < nn) {
      if(byrow) {
        ncol <- ceiling(nn / nrow)
      } else {
        nrow <- ceiling(nn / ncol)
      }
    }

    ## hold plot references for each plot
    tmp <- lapply(figs, function(fig)
      fig$x$spec$model$plot[c("type", "subtype", "id")])

    ## arrange plot references for gridplot
    idx <- seq_along(tmp)
    idx <- c(idx, rep(NA, nrow * ncol - length(idx)))
    idxm <- matrix(idx, nrow = nrow, ncol = ncol, byrow = byrow)

    plot_refs <- vector("list", nrow)
    for(ii in seq_len(nrow(idxm))) {
      cur_idx <- idxm[ii,]
      cur_idx <- cur_idx[!is.na(cur_idx)]
      plot_refs[[ii]] <- tmp[cur_idx]
    }
  } else {
    ## list of lists of BokehFigure objects
    ok <- sapply(figs, function(x) {
      all(sapply(x, function(y) inherits(y$x$spec, "BokehFigure")))
    })
    if(!all(ok))
      stop("'figs' argument to makeGrid must be a list of BokehFigure objects or a list of lists of BokehFigure objects", call. = FALSE)

    ## get plot refs
    plot_refs <- lapply(figs, function(x) {
      lapply(x, function(y) {
        y$x$spec$model$plot[c("type", "subtype", "id")]
      })
    })
    figs <- unlist(figs, recursive = FALSE)
    nrow <- length(plot_refs)
    ncol <- max(sapply(plot_refs, length))
  }

  ## deal with axes
  x_range <- y_range <- NULL
  if(same_x) {
    x_range <- get_grid_ranges(figs, "x")
    for(ii in seq_along(figs)) {
      figs[[ii]]$x$spec$xlim <- x_range$range # prevents prepare_figure() from computing range
      figs[[ii]]$x$spec$has_x_range <- TRUE # prevents prepare_figure() from adding range
      figs[[ii]]$x$spec$model$plot$attributes$x_range <- x_range$mod$ref
    }
  }
  if(same_y) {
    y_range <- get_grid_ranges(figs, "y")
    for(ii in seq_along(figs)) {
      figs[[ii]]$x$spec$ylim <- y_range$range # prevents prepare_figure() from computing range
      figs[[ii]]$x$spec$has_y_range <- TRUE # prevents prepare_figure() from adding range
      figs[[ii]]$x$spec$model$plot$attributes$y_range <- y_range$mod$ref
    }
  }

  spec <- structure(list(
    plot_refs = plot_refs,
    figs = figs,
    x_range = x_range$mod$model,
    y_range = y_range$mod$model,
    link_data = link_data,
    nrow = nrow, ncol = ncol), class = "BokehGridPlot")

  obj <- htmlwidgets::createWidget(
    name = 'rbokeh',
    x = list(
      spec = spec,
      elementid = digest(Sys.time()),
      modeltype = "GridPlot",
      modelid = "asdf"
    ),
    preRenderHook = rbokeh_prerender,
    width = width,
    height = height,
    package = 'rbokeh'
  )

  ## get overall width / height
  dims <- lapply(obj$x$spec$figs, function(x) {
    list(
      id     = x$x$spec$model$plot$id,
      width  = x$x$spec$model$plot$attributes$plot_width,
      height = x$x$spec$model$plot$attributes$plot_height
    )
  })
  names(dims) <- sapply(dims, function(x) x$id)

  wmat <- matrix(0, nrow = obj$x$spec$nrow, ncol = obj$x$spec$ncol)
  hmat <- matrix(0, nrow = obj$x$spec$nrow, ncol = obj$x$spec$ncol)
  for(ii in seq_along(obj$x$spec$plot_refs)) {
    for(jj in seq_along(obj$x$spec$plot_refs[[ii]])) {
      wmat[ii, jj] <- dims[[obj$x$spec$plot_refs[[ii]][[jj]]$id]]$width
      hmat[ii, jj] <- dims[[obj$x$spec$plot_refs[[ii]][[jj]]$id]]$height
    }
  }

  width <- sum(apply(wmat, 2, max)) + 46
  height <- sum(apply(hmat, 1, max))

  update_fig_dims <- FALSE
  if(is.null(obj$width)) {
    obj$width <- width
  } else {
    # shrink/expand widths
    wmat <- wmat * obj$width / width
    update_fig_dims <- TRUE
  }

  if(is.null(obj$height)) {
    obj$height <- height
  } else {
    # shrink/expand heights
    hmat <- hmat * obj$height / height
    update_fig_dims <- TRUE
  }

  names(obj$x$spec$figs) <- sapply(obj$x$spec$figs, function(x) x$x$spec$model$plot$id)
  if(update_fig_dims) {
    for(ii in seq_along(obj$x$spec$plot_refs)) {
      for(jj in seq_along(obj$x$spec$plot_refs[[ii]])) {
        cur_id <- obj$x$spec$plot_refs[[ii]][[jj]]$id
        obj$x$spec$figs[[cur_id]]$width <- wmat[ii, jj]
        obj$x$spec$figs[[cur_id]]$height <- hmat[ii, jj]
      }
    }
  }

  # set attributes to help set the padding for each individual panel
  for(ii in seq_along(obj$x$spec$figs)) {
    obj$x$spec$figs[[ii]]$x$spec$model$plot$attributes$toolbar_location <- "None"
    obj$x$spec$figs[[ii]]$x$parenttype <- "GridPlot"
  }

  obj
}

grid_plot_model <- function(id, plot_refs, tool_event_ref, width, height) {
  res <- base_model_object("GridPlot", id)

  res$model$attributes$children <- plot_refs
  res$model$attributes$tool_events <- tool_event_ref

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

## add a figure to a BokehGridPlot object/
## obj must be a BokehGridPlot and p must be a BokehFigure object
# add_plot <- function(obj, p, row = NULL, col = NULL, same_y = FALSE, same_x = FALSE) {
#   ## warn if overwriting a plot

# }


## for linked pan / zoom, set each plot's Range1d to be the same
## and adjust limits
## for linked brushing, set each plot's data sources to be the same (where possible)

## run prepare on all figures in the grid
## merge axes and ranges if necessary
prepare_gridplot <- function(obj) {
  figs <- lapply(obj$x$spec$figs, prepare_figure)

  data_mods <- list()
  ## deal with linked data
  if(obj$x$spec$link_data) {
    ## find data signatures that match
    sigs <- do.call(c, lapply(figs, function(x)
      unique(do.call(c, lapply(x$x$spec$data_sigs, function(y) y$sig)))))
    sigst <- table(sigs)
    idx <- which(sigst > 1)
    if(length(idx) > 0) {
      ## take each data source that has at least one match
      ## find figures with this data source
      ## merge the data sources
      ## and then point the glyphrenderers of each to this new data source
      for(sig in names(idx)) {
        has_data <- list()
        for(ii in seq_along(figs)) {
          for(jj in seq_along(figs[[ii]]$x$spec$data_sigs)) {
            if(!is.null(figs[[ii]]$x$spec$data_sigs[[jj]]$sig))
              if(figs[[ii]]$x$spec$data_sigs[[jj]]$sig == sig)
                has_data[[length(has_data) + 1]] <- list(index = c(ii, jj), glr_id = figs[[ii]]$x$spec$data_sigs[[jj]]$glr_id)
          }
        }
        d_id <- gen_id(NULL, sig)
        new_data_ref <- list(type = "ColumnDataSource", id = d_id)
        hd1 <- has_data[[1]]$index
        gl1 <- has_data[[1]]$glr_id
        ds1 <- figs[[hd1[1]]]$x$spec$model[[gl1]]$attributes$data_source$id
        d1 <- figs[[hd1[1]]]$x$spec$model[[ds1]]$attributes$data
        figs[[hd1[1]]]$x$spec$model[[gl1]]$attributes$data_source <- new_data_ref
        new_data <- d1
        figs[[hd1[1]]]$x$spec$model[[ds1]] <- NULL
        ## do the naive thing for now and don't check for identical columns
        for(ii in seq_along(has_data)[-1]) {
          hd <- has_data[[ii]]$index
          glr <- has_data[[ii]]$glr_id
          ds <- figs[[hd[1]]]$x$spec$model[[glr]]$attributes$data_source$id
          gl <- figs[[hd[1]]]$x$spec$model[[glr]]$attributes$glyph$id
          nsgl <- figs[[hd[1]]]$x$spec$model[[glr]]$attributes$nonselection_glyph$id
          d <- figs[[hd[1]]]$x$spec$model[[ds]]$attributes$data
          merge_names <- intersect(names(d), c("x", "y", "fill_color", "fill_alpha", "line_color", "line_width", "line_alpha"))
          new_names <- paste0(merge_names, ii)
          d2 <- d[merge_names]
          names(d2) <- new_names
          new_data <- c(new_data, d2, d[setdiff(names(d), c(new_names, names(new_data)))])
          ## update references
          upd <- figs[[hd[1]]]$x$spec$model[[gl]]$attributes[merge_names]
          for(nm in names(upd)) {
            if(!is.null(upd[[nm]]$field))
              upd[[nm]]$field <- paste0(upd[[nm]]$field, ii)
          }
          figs[[hd[1]]]$x$spec$model[[gl]]$attributes[merge_names] <- upd

          upd <- figs[[hd[1]]]$x$spec$model[[nsgl]]$attributes[merge_names]
          for(nm in names(upd)) {
            if(!is.null(upd[[nm]]$field))
              upd[[nm]]$field <- paste0(upd[[nm]]$field, ii)
          }
          figs[[hd[1]]]$x$spec$model[[nsgl]]$attributes[merge_names] <- upd

          figs[[hd[1]]]$x$spec$model[[glr]]$attributes$data_source <- new_data_ref
          figs[[hd[1]]]$x$spec$model[[ds]] <- NULL
        }
        ## add this data source
        data_mods[[sig]] <- data_model(new_data, d_id)
      }
    } else {
      message("'link_data' was set to TRUE, but none of the figures in the grid have the same data source.")
    }
  }

  mod <- unlist(lapply(figs, function(fig) remove_model_names(fig$x$spec$model)), recursive = FALSE)

  id <- gen_id(list(x = list(spec = list(time = Sys.time()))), "GridPlot")

  tid <- gen_id(list(x = list(spec = list(time = Sys.time()))), c("GridPlot", "tool"))
  tool_evt <- tool_events(tid)

  mod$grid_plot <- grid_plot_model(id, obj$x$spec$plot_refs, tool_evt$ref, obj$width, obj$height)$model
  mod$tool_evt <- tool_evt$model
  mod$x_range <- obj$x$spec$x_range
  mod$y_range <- obj$x$spec$y_range

  mod$tool_evt <- tool_evt$model

  for(md in data_mods) {
    mod[[md$ref$id]] <- md$model
  }

  names(mod) <- NULL

  obj$x$padding <- list(type = "gridplot")
  obj$x$spec$model <- mod
  obj$x$modelid <- id

  obj
}

get_grid_ranges <- function(objs, which = "x") {
  w1 <- paste0("glyph_", which, "_ranges")
  w2 <- paste0(which, "_axis_type")
  ranges <- unlist(lapply(objs, function(x) x$x$spec[[w1]]), recursive = FALSE)
  rng <- get_all_glyph_range(ranges, objs[[1]]$x$spec$padding_factor, objs[[1]]$x$spec[[w2]])
  id <- gen_id(objs[[1]]$x$spec, c(which, "GridRange"))
  list(range = rng, mod = range_model(ifelse(is.numeric(rng), "Range1d", "FactorRange"), id, rng))
}
