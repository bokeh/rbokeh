
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

    tmp <- lapply(objs, function(x) {
      x$model$plot[c("type", "subtype", "id")]
    })

    plot_refs <- vector("list", nrow)
    for(ii in seq_len(nrow)) {
      cur_idx <- ((ii - 1) * ncol + 1):(min(ii * ncol, nn))
      plot_refs[[ii]] <- tmp[cur_idx]
    }
  } else {
    ## list of lists of BokehFigure objects
    ok <- sapply(objs, function(x) {
      all(sapply(x, function(y) inherits(y, "BokehFigure")))
    })
    if(!all(ok))
      stop("'objs' argument to makeGrid must be a list of BokehFigure objects or a list of lists of BokehFigure objects", call. = FALSE)

    ## get plot refs
    plot_refs <- lapply(objs, function(x) {
      lapply(x, function(y) {
        y$model$plot[c("type", "subtype", "id")]
      })
    })
    objs <- unlist(objs, recursive = FALSE)
  }


  ## deal with axes
  x_range <- y_range <- NULL
  if(same_x) {
    x_range <- get_grid_ranges(objs, "x")
    for(ii in seq_along(objs)) {
      objs[[ii]]$xlim <- x_range$range # prevents prepare_figure() from computing range
      objs[[ii]]$has_x_range <- TRUE # prevents prepare_figure() from adding range
      objs[[ii]]$model$plot$attributes$x_range <- x_range$mod$ref
    }
  }
  if(same_y) {
    y_range <- get_grid_ranges(objs, "y")
    for(ii in seq_along(objs)) {
      objs[[ii]]$ylim <- y_range$range # prevents prepare_figure() from computing range
      objs[[ii]]$has_y_range <- TRUE # prevents prepare_figure() from adding range
      objs[[ii]]$model$plot$attributes$y_range <- y_range$mod$ref
    }
  }

  structure(list(
    plot_refs = plot_refs,
    figs = objs,
    x_range = x_range$mod$model,
    y_range = y_range$mod$model,
    link_data = link_data,
    nrow = nrow, ncol = ncol), class = "BokehGridPlot")
}

## add a figure to a BokehGridPlot object/
## obj must be a BokehGridPlot and p must be a BokehFigure object
# add_plot <- function(obj, p, row = NULL, col = NULL, same_y = FALSE, same_x = FALSE) {
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
  for(ii in seq_along(obj$plot_refs)) {
    for(jj in seq_along(obj$plot_refs[[ii]])) {
      wmat[ii, jj] <- dims[[obj$plot_refs[[ii]][[jj]]$id]]$width
      hmat[ii, jj] <- dims[[obj$plot_refs[[ii]][[jj]]$id]]$height
    }
  }
  width <- sum(apply(wmat, 2, max))
  height <- sum(apply(hmat, 2, max))

  figs <- lapply(obj$figs, prepare_figure)

  data_mods <- list()
  ## deal with linked data
  if(obj$link_data) {
    ## find data signatures that match
    sigs <- do.call(c, lapply(figs, function(x)
      unique(do.call(c, lapply(x$data_sigs, function(y) y$sig)))))
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
          for(jj in seq_along(figs[[ii]]$data_sigs)) {
            if(!is.null(figs[[ii]]$data_sigs[[jj]]$sig))
              if(figs[[ii]]$data_sigs[[jj]]$sig == sig)
                has_data[[length(has_data) + 1]] <- list(index = c(ii, jj), glr_id = figs[[ii]]$data_sigs[[jj]]$glr_id)
          }
        }
        d_id <- gen_id(NULL, sig)
        new_data_ref <- list(type = "ColumnDataSource", id = d_id)
        hd1 <- has_data[[1]]$index
        gl1 <- has_data[[1]]$glr_id
        ds1 <- figs[[hd1[1]]]$model[[gl1]]$attributes$data_source$id
        d1 <- figs[[hd1[1]]]$model[[ds1]]$attributes$data
        figs[[hd1[1]]]$model[[gl1]]$attributes$data_source <- new_data_ref
        new_data <- d1
        figs[[hd1[1]]]$model[[ds1]] <- NULL
        ## do the naive thing for now and don't check for identical columns
        for(ii in seq_along(has_data)[-1]) {
          hd <- has_data[[ii]]$index
          glr <- has_data[[ii]]$glr_id
          ds <- figs[[hd[1]]]$model[[glr]]$attributes$data_source$id
          gl <- figs[[hd[1]]]$model[[glr]]$attributes$glyph$id
          nsgl <- figs[[hd[1]]]$model[[glr]]$attributes$nonselection_glyph$id
          d <- figs[[hd[1]]]$model[[ds]]$attributes$data
          merge_names <- intersect(names(d), c("x", "y", "fill_color", "fill_alpha", "line_color", "line_width", "line_alpha"))
          new_names <- paste0(merge_names, ii)
          d2 <- d[merge_names]
          names(d2) <- new_names
          new_data <- c(new_data, d2, d[setdiff(names(d), c(new_names, names(new_data)))])
          ## update references
          upd <- figs[[hd[1]]]$model[[gl]]$attributes[merge_names]
          for(nm in names(upd)) {
            if(!is.null(upd[[nm]]$field))
              upd[[nm]]$field <- paste0(upd[[nm]]$field, ii)
          }
          figs[[hd[1]]]$model[[gl]]$attributes[merge_names] <- upd

          upd <- figs[[hd[1]]]$model[[nsgl]]$attributes[merge_names]
          for(nm in names(upd)) {
            if(!is.null(upd[[nm]]$field))
              upd[[nm]]$field <- paste0(upd[[nm]]$field, ii)
          }
          figs[[hd[1]]]$model[[nsgl]]$attributes[merge_names] <- upd

          figs[[hd[1]]]$model[[glr]]$attributes$data_source <- new_data_ref
          figs[[hd[1]]]$model[[ds]] <- NULL
        }
        ## add this data source
        data_mods[[sig]] <- data_model(new_data, d_id)
      }
    } else {
      message("'link_data' was set to TRUE, but none of the figures in the grid have the same data source.")
    }
  }

  mod <- unlist(lapply(figs, function(x) remove_model_names(x$model)), recursive = FALSE)

  id <- gen_id(list(time = Sys.time()), "GridPlot")
  tid <- gen_id(list(time = Sys.time()), c("GridPlot", "tool"))
  tool_evt <- tool_events(tid)

  mod$grid_plot <- grid_plot_model(id, obj$plot_refs, tool_evt$ref, width, height)$model
  mod$tool_evt <- tool_evt$model
  mod$x_range <- obj$x_range
  mod$y_range <- obj$y_range

  mod$tool_evt <- tool_evt$model

  for(md in data_mods) {
    mod[[md$ref$id]] <- md$model
  }

  names(mod) <- NULL

  list(model = mod, width = width, height = height, id = id)
}

get_grid_ranges <- function(objs, which = "x") {
  w1 <- paste0("glyph_", which, "_ranges")
  w2 <- paste0(which, "_axis_type")
  ranges <- unlist(lapply(objs, function(x) x[[w1]]), recursive = FALSE)
  rng <- get_all_glyph_range(ranges, objs[[1]]$padding_factor, objs[[1]][[w2]])
  id <- gen_id(NULL, c(which, "GridRange"))
  list(range = rng, mod = range_model(ifelse(is.numeric(rng), "Range1d", "FactorRange"), id, rng))
}
