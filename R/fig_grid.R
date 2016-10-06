#' Create a Bokeh grid plot from a list of Bokeh figures
#' @param figs list of Bokeh figures - see details for what is acceptable
#' @param width width of the entire grid plot in pixels - if \code{NULL}, the sum of the grid widths of columns will be used - if not \code{NULL}, the widths of the plots will be proportionately shrunk to meet the specified width
#' @param height height of the entire grid plot in pixels - if \code{NULL}, the sum of the grid heights of rows will be used - if not \code{NULL}, the heights of the plots will be proportionately shrunk to meet the specified height
#' @param nrow number of rows in the grid
#' @param ncol number of columns in the grid
#' @param byrow populate the grid by row according to the order of figure elements supplied in \code{params}
#' @param xlim the extent of the plotting area in the x-dimension to be applied to every panel (original individual panel limits will be honored if not specified).
#' @param ylim the extent of the plotting area in the y-dimension to be applied to every panel (original individual panel limits will be honored if not specified).
#' @param logo ('normal', 'grey') What version of the Bokeh logo to display on the toolbar. If set to NULL, no logo will be displayed.
#' @param same_axes logical or vector of two logicals specifying whether the x and/or y axis limits should be the same for each plot in the grid
#' @param simplify_axes logical or vector of logicals specifying whether to simply the x and/or y axes (only show the axes along the bottom and left sides of the grid) - only valid if \code{same_axes} is \code{TRUE} for the axis
#' @param x_margin,y_margin specify the margin space in pixels to be left for axes when using \code{simplify_axes=TRUE}
#' @param link_data logical - should an attempt be made to join the data sources of each plot for linked brushing? (see details)
#' @example man-roxygen/ex-grid.R
#' @details The \code{figs} parameter can either be a list of figures or a list of lists of figures.  If the latter, the list structure will determine the layout, with each super-list of figures defining a single row of the grid.  If the former, the parameters \code{nrow} and \code{ncol} and \code{byrow} are used to determine the layout.  The grid is from top to bottom left to right.
#'
#' If \code{link_data} is \code{TRUE}, then an effort will be made to link all data sources that are common among the different figures in the plot.  Note that at this point, only data sources that are specified in the \code{data} argument to the different layer functions are checked.
#' @export
grid_plot <- function(figs, width = NULL, height = NULL,
  nrow = 1, ncol = 1, byrow = TRUE,
  xlim = NULL, ylim = NULL, logo = NULL,
  same_axes = FALSE, simplify_axes = TRUE,
  y_margin = NULL, x_margin = NULL, link_data = FALSE) {

  tb_pars <- list(logo = logo)
  tb_pars <- handle_extra_pars(tb_pars, grid_plot_par_validator_map)

  if (length(same_axes) == 1) {
    same_x <- same_y <- same_axes
  } else {
    same_x <- same_axes[1]
    same_y <- same_axes[2]
  }

  if (length(simplify_axes) == 1) {
    simplify_x <- simplify_y <- simplify_axes
  } else {
    simplify_x <- simplify_axes[1]
    simplify_y <- simplify_axes[2]
  }

  if (!is.list(figs))
    stop("'figs' must be a list")

  is_fig_list <- sapply(figs, function(x)
    inherits(x$x$spec, "BokehFigure") || is.null(x))

  if (any(is_fig_list)) {
    ## list of BokehFigure objects

    if (!all(is_fig_list))
      stop(paste("'figs' argument to makeGrid must be a list of BokehFigure objects",
        "or a list of lists of BokehFigure objects"), call. = FALSE)

    nn <- length(figs)
    if (missing(ncol))
      ncol <- ceiling(nn / nrow)
    if (missing(nrow))
      nrow <- ceiling(nn / ncol)

    if ((nrow * ncol) < nn) { # nolint
      if (byrow) {
        ncol <- ceiling(nn / nrow)
      } else {
        nrow <- ceiling(nn / ncol)
      }
    }

    ## hold plot references for each plot
    tmp <- lapply(figs, function(fig)
      fig$x$spec$model$plot[c("type", "subtype", "id")])

    ## arrange plot references for gridplot
    idx <- rep(NA, length(tmp))
    cur_fig <- 1
    for (ii in seq_along(idx)) {
      if (!is.null(figs[[ii]])) {
        idx[ii] <- cur_fig
        cur_fig <- cur_fig + 1
      }
    }
    idx <- c(idx, rep(NA, (nrow * ncol) - length(idx)))
    tmp[sapply(tmp, is.null)] <- NULL
    idxm <- matrix(idx, nrow = nrow, ncol = ncol, byrow = byrow)

    plot_refs <- vector("list", nrow)
    for (ii in seq_len(nrow(idxm))) {
      plot_refs[[ii]] <- vector("list", ncol)
      for (jj in seq_len(ncol(idxm)))
        plot_refs[[ii]][[jj]] <- tmp[[idxm[ii, jj]]]
    }
  } else {
    ## list of lists of BokehFigure objects
    ok <- sapply(figs, function(x) {
      all(sapply(x, function(y)
        inherits(y$x$spec, "BokehFigure") || is.null(y)))
    })
    if (!all(ok))
      stop(paste("'figs' argument to makeGrid must be a list of BokehFigure objects",
        "or a list of lists of BokehFigure objects"), call. = FALSE)

    ## get plot refs
    plot_refs <- lapply(figs, function(x) {
      lapply(x, function(y) {
        y$x$spec$model$plot[c("type", "subtype", "id")]
      })
    })
    nrow <- length(plot_refs)
    ncol <- max(sapply(plot_refs, length))
    idxm <- matrix(nrow = nrow, ncol = ncol, data = NA)
    cur_fig <- 1
    for (ii in seq_along(figs)) {
      for (jj in seq_along(figs[[ii]])) {
        if (!is.null(figs[[ii]][[jj]])) {
          idxm[ii, jj] <- cur_fig
          cur_fig <- cur_fig + 1
        }
      }
    }
    figs <- unlist(figs, recursive = FALSE)
  }
  figs[sapply(figs, is.null)] <- NULL

  ## give panels names if figs is named list
  if (!is.null(names(figs))) {
    fig_names <- names(figs)
    for (ii in seq_along(figs)) {
      figs[[ii]] <- add_title(figs[[ii]], fig_names[ii])
    }
  } else {
    for (ii in seq_along(figs)) {
      figs[[ii]]$x$spec$model$plot$attributes$title <- NULL
    }
  }

  ## deal with axes
  x_range <- y_range <- NULL
  if (same_x) {
    x_range <- get_grid_ranges(figs, "x")
    for (ii in seq_along(figs)) {
      figs[[ii]]$x$spec$xlim <- x_range$range
      figs[[ii]]$x$spec$has_x_range <- TRUE # prevents prepare_figure() from adding range
      figs[[ii]]$x$spec$model$plot$attributes$x_range <- x_range$mod$ref
    }
    if (simplify_x) {
      idxs <- as.vector(idxm[-nrow(idxm), ])
      idxs <- idxs[!is.na(idxs)]
      for (ii in idxs) {
        if (figs[[ii]]$x$spec$has_x_axis) {
          figs[[ii]]$x$spec$model$x_axis$attributes$visible <- FALSE
        } else {
          figs[[ii]] <- figs[[ii]] %>% x_axis(visible = FALSE)
        }
      }
      if (is.null(x_margin))
        x_margin <- 70
    }
  }
  if (same_y) {
    y_range <- get_grid_ranges(figs, "y")
    for (ii in seq_along(figs)) {
      figs[[ii]]$x$spec$ylim <- y_range$range
      figs[[ii]]$x$spec$has_y_range <- TRUE # prevents prepare_figure() from adding range
      figs[[ii]]$x$spec$model$plot$attributes$y_range <- y_range$mod$ref
    }
    if (simplify_y) {
      idxs <- as.vector(idxm[, -1])
      idxs <- idxs[!is.na(idxs)]
      for (ii in idxs) {
        if (figs[[ii]]$x$spec$has_y_axis) {
          figs[[ii]]$x$spec$model$y_axis$attributes$visible <- FALSE
        } else {
          figs[[ii]] <- figs[[ii]] %>% y_axis(visible = FALSE)
        }
      }
      if (is.null(y_margin))
        y_margin <- 45
    }
  }

  if (!is.null(xlim)) {
    id <- gen_id(figs[[1]], c("x", "GridRange"))
    x_range <- list(range = xlim,
      mod = range_model(ifelse(is.numeric(xlim), "Range1d", "FactorRange"), id, xlim))
    for (ii in seq_along(figs)) {
      figs[[ii]]$x$spec$xlim <- xlim
      figs[[ii]]$x$spec$has_x_range <- TRUE
      figs[[ii]]$x$spec$model$plot$attributes$x_range <- x_range$mod$ref
    }
  }

  if (!is.null(ylim)) {
    id <- gen_id(figs[[1]], c("y", "GridRange"))
    y_range <- list(range = ylim,
      mod = range_model(ifelse(is.numeric(ylim), "Range1d", "FactorRange"), id, ylim))
    for (ii in seq_along(figs)) {
      figs[[ii]]$x$spec$ylim <- ylim
      figs[[ii]]$x$spec$has_y_range <- TRUE
      figs[[ii]]$x$spec$model$plot$attributes$y_range <- y_range$mod$ref
    }
  }

  # hide individual toolbars in each figure
  # and remove sizing_mode from each
  for (ii in seq_along(figs)) {
    figs[[ii]]$x$spec$model$plot$attributes["toolbar_location"] <- list(NULL)
    figs[[ii]]$x$spec$model$plot$attributes$sizing_mode <- NULL
  }

  # this will hold all of the models for new elements we need to add
  layoutspec <- list()

  # set up master column for plots
  mcid <- gen_id(figs[[1]], "MasterColumn")
  mcmod <- base_model_object("Column", mcid)

  # spacer_refs is used for dimensions later on
  spacer_id_mat <- matrix(nrow = nrow(idxm), ncol = ncol(idxm), data = NA)

  # set up row and column models pointing to each figure
  rrefs <- list()
  for (row in seq_len(nrow(idxm))) {
    refs <- list()
    for (col in seq_along(idxm[row, ])) {
      idx <- idxm[row, col]
      if (is.na(idx)) {
        spid <- gen_id(figs[[idx]], paste0("spacer", row, "_", col))
        spmod <- base_model_object("Spacer", spid)
        layoutspec[[spid]] <- spmod$model
        refs[[col]] <- spmod$ref
        spacer_id_mat[row, col] <- spmod$ref$id
      } else {
        refs[[col]] <- figs[[idx]]$x$spec$ref
      }
    }
    rid <- gen_id(figs[[1]], paste0("Row", row))
    rowmod <- base_model_object("Row", rid)
    rowmod$model$attributes$children <- refs
    rowmod$model$attributes$id <- NULL
    rowmod$model$attributes$tags <- NULL
    layoutspec[[rid]] <- rowmod$model
    rrefs[[row]] <- rowmod$ref
  }

  mcmod$model$attributes$children <- rrefs
  mcmod$model$attributes$id <- NULL
  mcmod$model$attributes$tags <- NULL
  layoutspec$mastercolumn <- mcmod$model

  # set up a ToolbarBox for the whole grid plot
  tbbid <- gen_id(figs[[1]], "ToolbarBox")
  tbbmod <- base_model_object("ToolbarBox", tbbid)
  tbbmod$model$attributes$sizing_mode <- "scale_width"
  tbbmod$model$attributes$toolbar_location <- "above"
  # tbbmod$model$attributes$toolbar_location <- tb_pars$toolbar_location
  if ("logo" %in% names(tb_pars))
    tbbmod$model$attributes["logo"] <- list(tb_pars$logo)

  # gather all tools from all figures
  alltools <- unname(unlist(lapply(figs,
    function(a) {
      unname(a$x$spec$model$toolbar$attributes$tools)
    }
  ), recursive = FALSE))
  tbbmod$model$attributes$tools <- alltools
  tbbmod$model$attributes$sizing_mode <- "scale_width"
  layoutspec$toolbarbox <- tbbmod$model

  # set up root column
  rcid <- gen_id(list(x = list(spec = list(time = Sys.time()))), "root_column")
  root_column <- base_model_object("Column", rcid)
  root_column$model$attributes$children <- list(tbbmod$ref, mcmod$ref)
  root_column$model$attributes$id <- NULL
  root_column$model$attributes$tags <- NULL
  layoutspec$root <- root_column$model

  spec <- structure(list(
    layout = layoutspec,
    plot_refs = plot_refs,
    spacer_id_mat = spacer_id_mat,
    figs = figs,
    x_range = x_range$mod$model,
    y_range = y_range$mod$model,
    link_data = link_data,
    x_margin = x_margin, y_margin = y_margin,
    nrow = nrow, ncol = ncol), class = "BokehGridPlot")

  obj <- htmlwidgets::createWidget(
    name = "rbokeh",
    x = list(
      spec = spec,
      elementid = digest(Sys.time()),
      modeltype = "GridPlot",
      modelid = rcid,
      docid = digest::digest(paste("rbokehgridplot", Sys.time())),
      docs_json = list(list(
        version = get_bokeh_version(),
        title = "Bokeh GridPlot",
        roots = list(
          root_ids = list(rcid),
          references = NULL
      )))
    ),
    sizingPolicy = htmlwidgets::sizingPolicy(defaultWidth = 800, defaultHeight = 800),
    preRenderHook = rbokeh_prerender,
    width = width,
    height = height,
    package = "rbokeh"
  )
  names(obj$x$docs_json) <- obj$x$docid

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
  for (ii in seq_along(obj$x$spec$plot_refs)) {
    for (jj in seq_along(obj$x$spec$plot_refs[[ii]])) {
      if (is.null(obj$x$spec$plot_refs[[ii]][[jj]])) {
        wmat[ii, jj] <- NA
        hmat[ii, jj] <- NA
      } else {
        ww <- dims[[obj$x$spec$plot_refs[[ii]][[jj]]$id]]$width
        if (is.null(ww))
          ww <- 800 / ncol(wmat)
        wmat[ii, jj] <- ww
        hh <- dims[[obj$x$spec$plot_refs[[ii]][[jj]]$id]]$height
        if (is.null(hh))
          hh <- 800 / nrow(hmat)
        hmat[ii, jj] <- hh
      }
    }
  }

  # fill in blank dimensions with row or column max
  cmax <- apply(wmat, 2, max, na.rm = TRUE)
  rmax <- apply(hmat, 1, max, na.rm = TRUE)
  wna <- which(is.na(wmat), arr.ind = TRUE)
  if (length(wna) > 0) {
    for (ii in seq_len(nrow(wna))) {
      wmat[wna[ii, 1], wna[ii, 2]] <- cmax[wna[ii, 2]]
    }
  }
  hna <- which(is.na(hmat), arr.ind = TRUE)
  if (length(hna) > 0) {
    for (ii in seq_len(nrow(hna))) {
      hmat[hna[ii, 1], hna[ii, 2]] <- rmax[hna[ii, 1]]
    }
  }

  obj$x$spec$wmat <- wmat
  obj$x$spec$hmat <- hmat

  obj_width <- sum(apply(wmat, 2, function(x) max(x, na.rm = TRUE)))
  obj_height <- sum(apply(hmat, 1, function(x) max(x, na.rm = TRUE)))

  if (is.null(obj$width))
    obj$width <- obj_width

  if (is.null(obj$height))
    obj$height <- obj_height

  names(obj$x$spec$figs) <- sapply(obj$x$spec$figs, function(x) x$x$spec$model$plot$id)

  # # set attributes to help set the padding for each individual panel
  # for (ii in seq_along(obj$x$spec$figs)) {
  #   obj$x$spec$figs[[ii]]$x$spec$model$plot$attributes["toolbar_location"] <- list(NULL)
  #   obj$x$spec$figs[[ii]]$x$parenttype <- "GridPlot"
  # }

  obj
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
  obj <- update_grid_sizes(obj)
  figs <- lapply(obj$x$spec$figs, prepare_figure)

  data_mods <- list()
  ## deal with linked data
  if (obj$x$spec$link_data) {
    ## find data signatures that match
    sigs <- do.call(c, lapply(figs, function(x)
      unique(do.call(c, lapply(x$x$spec$data_sigs, function(y) y$sig)))))
    sigst <- table(sigs)
    idx <- which(sigst > 1)
    if (length(idx) > 0) {
      ## take each data source that has at least one match
      ## find figures with this data source
      ## merge the data sources
      ## and then point the glyphrenderers of each to this new data source
      for (sig in names(idx)) {
        has_data <- list()
        for (ii in seq_along(figs)) {
          for (jj in seq_along(figs[[ii]]$x$spec$data_sigs)) {
            if (!is.null(figs[[ii]]$x$spec$data_sigs[[jj]]$sig))
              if (figs[[ii]]$x$spec$data_sigs[[jj]]$sig == sig)
                has_data[[length(has_data) + 1]] <- list(index = c(ii, jj),
                  glr_id = figs[[ii]]$x$spec$data_sigs[[jj]]$glr_id)
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
        for (ii in seq_along(has_data)[-1]) {
          hd <- has_data[[ii]]$index
          glr <- has_data[[ii]]$glr_id
          ds <- figs[[hd[1]]]$x$spec$model[[glr]]$attributes$data_source$id
          gl <- figs[[hd[1]]]$x$spec$model[[glr]]$attributes$glyph$id
          nsgl <- figs[[hd[1]]]$x$spec$model[[glr]]$attributes$nonselection_glyph$id
          d <- figs[[hd[1]]]$x$spec$model[[ds]]$attributes$data
          merge_names <- intersect(names(d),
            c("x", "y", "fill_color", "fill_alpha", "line_color",
              "line_width", "line_alpha"))
          new_names <- paste0(merge_names, ii)
          d2 <- d[merge_names]
          names(d2) <- new_names
          new_data <- c(new_data, d2, d[setdiff(names(d), c(new_names, names(new_data)))])
          ## update references
          upd <- figs[[hd[1]]]$x$spec$model[[gl]]$attributes[merge_names]
          for (nm in names(upd)) {
            if (!is.null(upd[[nm]]$field))
              upd[[nm]]$field <- paste0(upd[[nm]]$field, ii)
          }
          figs[[hd[1]]]$x$spec$model[[gl]]$attributes[merge_names] <- upd

          upd <- figs[[hd[1]]]$x$spec$model[[nsgl]]$attributes[merge_names]
          for (nm in names(upd)) {
            if (!is.null(upd[[nm]]$field))
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
      message(paste("'link_data' was set to TRUE, but none of the figures",
        "in the grid have the same data source."))
    }
  }

  mod <- unlist(lapply(figs, function(fig) remove_model_names(fig$x$spec$model)), recursive = FALSE)

  xxrange <- obj$x$spec$x_range
  if (!is.null(xxrange)) {
    xxrange <- list(xxrange)
  }
  yyrange <- obj$x$spec$y_range
  if (!is.null(yyrange)) {
    yyrange <- list(yyrange)
  }

  dmods <- unname(lapply(data_mods, function(x) x$model))
  if (length(dmods) == 0)
    dmods <- NULL

  mod <- c(mod, remove_model_names(obj$x$spec$layout), xxrange, yyrange, dmods)

  names(mod) <- NULL

  obj$x$padding <- list(type = "gridplot")
  obj$x$spec$model <- mod
  # obj$x$modelid <- id

  obj
}

get_grid_ranges <- function(objs, which = "x") {
  w1 <- paste0("glyph_", which, "_ranges")
  w2 <- paste0(which, "_axis_type")
  ranges <- unlist(lapply(objs, function(x) x$x$spec[[w1]]), recursive = FALSE)
  rng <- get_all_glyph_range(ranges,
    objs[[1]]$x$spec$padding_factor, objs[[1]]$x$spec[[w2]])
  id <- gen_id(objs[[1]], c(which, "GridRange"))
  list(range = rng,
    mod = range_model(ifelse(is.numeric(rng), "Range1d", "FactorRange"), id, rng))
}

update_grid_sizes <- function(obj) {
  width <- obj$width
  height <- obj$height
  hmat <- obj$x$spec$hmat
  wmat <- obj$x$spec$wmat
  x_margin <- obj$x$spec$x_margin
  y_margin <- obj$x$spec$y_margin
  if (is.null(x_margin))
    x_margin <- 0
  if (is.null(y_margin))
    y_margin <- 0

  widths <- apply(wmat, 2, function(x) max(x, na.rm = TRUE))
  heights <- apply(hmat, 1, function(x) max(x, na.rm = TRUE))
  full_width <- sum(widths)
  full_height <- sum(heights)

  new_width_factor <- (width - y_margin) / full_width
  new_height_factor <- (height - x_margin - 30) / full_height

  new_wmat <- wmat * new_width_factor
  new_hmat <- hmat * new_height_factor
  new_wmat[, 1] <- new_wmat[, 1] + y_margin
  new_hmat[nrow(new_hmat), ] <- new_hmat[nrow(new_hmat), ] + x_margin

  obj$width <- sum(apply(new_wmat, 2, function(x) max(x, na.rm = TRUE))) + 30
  obj$height <- sum(apply(new_hmat, 1, function(x) max(x, na.rm = TRUE)))

  for (ii in seq_along(obj$x$spec$plot_refs)) {
    for (jj in seq_along(obj$x$spec$plot_refs[[ii]])) {
      if (!is.null(obj$x$spec$plot_refs[[ii]][[jj]])) {
        cur_id <- obj$x$spec$plot_refs[[ii]][[jj]]$id
        obj$x$spec$figs[[cur_id]]$width <- new_wmat[ii, jj]
        obj$x$spec$figs[[cur_id]]$height <- new_hmat[ii, jj]
        if (jj == 1) {
          obj$x$spec$figs[[cur_id]]$x$spec$model$plot$attributes$min_border_left <- y_margin
        }
        if (ii == length(obj$x$spec$plot_refs)) {
          obj$x$spec$figs[[cur_id]]$x$spec$model$plot$attributes$min_border_bottom <- x_margin
        }
      } else {
        spid <- obj$x$spec$spacer_id_mat[ii, jj]
        obj$x$spec$layout[[spid]]$attributes$width <- new_wmat[ii, jj]
        obj$x$spec$layout[[spid]]$attributes$height <- new_hmat[ii, jj]
      }
    }
  }
  obj
}

grid_plot_par_validator_map <- list(
  "toolbar_location" = "toolbar_location",
  "logo" =  "logo"
)
