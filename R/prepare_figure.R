prepare_figure <- function(x) {

  if (is.null(x$theme))
    x$theme <- bk_default_theme()

  # keep a list of data sources so we don't duplicate data
  data_sources <- list()

  for (lnm in names(x$layers)) {
    # lnm <- names(x$layers)[1]

    # this holds the arguments and data that were specified for this layer
    ly <- x$layers[[lnm]]

    if (ly$type == "glyph") {
      # this will hold all the model instances for the layer
      cur_ly <- list(transforms = list())

      # handle ajax data input
      if (inherits(ly$data, "ajax_data")) {
        ly$ajax_data <- ly$data
        ly$data <- get_ajax_sample(ly$data)
        # need to set the "column_names" attribute
        ly$ajax_data$column_names <- names(ly$data)
        snm <- sanitize(ly$ajax_data$column_names)
        if (!all(snm == ly$ajax_data$column_names))
          stop("Column names from AJAX data source have special characters (.)... ",
            "Bokeh expects clean column names.")
      }

      # hover needs special handling
      cur_hov <- ly$hover
      ly$hover <- NULL

      ## set up parameters to pass to Bokeh models
      ##---------------------------------------------------------
      # - evaluate quosures
      # - set up column data source (TODO: deal with single-row df)
      #   the goal is for the data source to remain as unchanged as possible
      # - add mappers when necessary
      #   a attribute needs to be mapped if it refers to a column in the data
      quo_idx <- which(sapply(ly, rlang::is_quosure))
      vals <- lapply(names(quo_idx), function(attr_nm) {
        rlang::eval_tidy(ly[[attr_nm]], ly$data)
      })
      names(vals) <- names(quo_idx)
      # handle special cases of specifications (x specified but not y)
      if ("x" %in% names(vals) && "y" %in% names(vals) && is.null(vals$y)) {
        if (stats::is.ts(vals$x)) {
          vals$y <- as.vector(vals$x)
          vals$x <- as.vector(stats::time(vals$x))
        } else if (is.list(vals$x)) {
          vals$x <- vals$x[[1]]
          vals$y <- vals$x[[2]]
        } else {
          vals$y <- vals$x
          vals$x <- seq_along(vals$x)
        }
      }

      # get fingerprint of every column of supplied data
      # and use this to build up the dataset for the ColumnDataSource
      col_dig <- unlist(lapply(ly$data, digest::digest))
      col_nm <- names(ly$data)
      cols_to_use <- NULL

      ## get set up for glyphs
      ##---------------------------------------------------------

      cur_glyph_props <- NULL
      if (valid_glyph(ly$model)) {
        cur_glyph_props <- marker_dict[[as.character(ly$model)]]
        ly$model <- cur_glyph_props$glyph
      }

      cur_model <- utils::getFromNamespace(ly$model, "rbokeh")
      par_nms <- get_init_formals(cur_model)
      can_be_vector <- get_can_be_vector(ly$model)

      ## loop over each attribute and construct appropriate models
      for (attr_nm in names(vals)) {
        val <- vals[[attr_nm]]

        # if this value was specified using spec(), extract that info
        spc <- attr(val, "spec")
        if (is.null(spc))
          spc <- list()
          # spc <- list(units = "screen")
        attr(val, "spec") <- NULL

        # keep track of axis ranges and types
        for (xy in c("x", "y")) {
          nms_chk <- c("x", "xs", "left", "right")
          if (xy == "y")
            nms_chk <- c("y", "ys", "bottom", "top")
          if (attr_nm %in% nms_chk) {
            type <- get_glyph_axis_type(val, x$pars$axes$type[[xy]])
            if (is.null(x$pars$axes$type[[xy]])) {
              x$pars$axes$type[[xy]] <- type
            } else {
              if (x$pars$axes$type[[xy]] != type)
                stop("Layer has incompatible x-axis type from previous layer.", call. = FALSE)
            }
            if (is.null(x$pars$axes$range[[xy]]))
              x$pars$axes$range[[xy]] <- get_glyph_range(val,
                prev_range = x$pars$axes$range[[xy]], axis_type = x$pars$axes$type[[xy]])
            if (is.null(x$pars$axes$lab[[xy]])) {
              lab_nm <- try(quo_name(ly[[attr_nm]]), silent = TRUE)
              if (inherits(lab_nm, "try-error"))
                lab_nm <- xy
              x$pars$axes$lab[[xy]] <- lab_nm
            }
          }
        }

        if (inherits(val, c("Date", "POSIXt")))
          val <- to_epoch(val)

        if (is.null(val)) {
          ly[attr_nm] <- list(NULL)
        } else if (attr_nm == "line_dash") {
          if (val %in% lty_names)
            val <- lty_dict[[val]]
          ly[[attr_nm]] <- val
          # ly[[attr_nm]] <- list(
          #   value = val,
          #   units = spc$units
          # )
        } else if (!attr_nm %in% can_be_vector) {
          if (length(val) > 1)
            message("Attribute '", attr_nm, "' must be a scalar... ",
              "Only using first observation.")
          ly[[attr_nm]] <- list(value = val[1])
        } else {
          # check to see if attrs are part of data
          # if they are, change the value to a field pointing to the column
          # if they aren't, add it to the data
          as_is <- FALSE
          if (inherits(val, "as_is") || length(val) == 1) {
            as_is <- TRUE
            class(val) <- setdiff(class(val), "as_is")
          }
          dval <- digest::digest(val)
          if (dval %in% col_dig) {
            nm <- col_nm[col_dig == dval]
            # deal with multiple column matches
            if (length(nm) > 1) {
              qnm <- quo_name(ly[[attr_nm]])
              # if the expression supplied directly maps to a variable name, use it
              if (qnm %in% col_nm) {
                nm <- qnm
              } else {
                nm <- nm[1]
              }
            }
            ly[[attr_nm]] <- list(field = sanitize(nm))
          } else if (length(val) == 1) {
            ly[[attr_nm]] <- val
          } else if (length(val) > 1) {
            nm <- paste0("col", length(ly$data) + 1, "___")
            ly$data[[nm]] <- val
            if (!is.data.frame(ly$data))
              ly$data <- as.data.frame(ly$data)
            ly[[attr_nm]] <- list(field = sanitize(nm))
          }

          ## take care of attribute mappings
          ##---------------------------------------------------------

          # we will always map attrs unless they are declared "asis"
          if (!as_is) {
            if (!is.null(spc$transform)) {
              if (!is.null(spc$domain) || !is.null(spc$range))
                message("Ignoring spec domain and range because a custom transform ",
                  "was specified.")
              cjs <- do.call(CustomJSTransform$new, spc$transform)
              cjs_id <- digest::digest(list(val, spc$transform, attr_nm))
              cur_ly[[cjs_id]] <- cjs
              ly[[attr_nm]] <- list(
                field = sanitize(nm),
                # units = spc$units,
                transform = cjs$get_instance())
            } else if (grepl("color", attr_nm)) {
              ## handle color mappings
              ##---------------------------------------------------------

              if (is.numeric(val)) {
                ## map continuous variable to discrete color palette
                ##---------------------------------------------------------

                if (is.null(spc$domain)) {
                  # get number of intervals from the theme
                  n_intervals <- x$theme$continuous$color_n_intervals
                  spc$domain <- pretty(val, n_intervals)
                }
                if (is.null(spc$range)) {
                  spc$range <- x$theme$continuous$color(length(spc$domain) - 1)
                }
                cjs <- js_transform_color_num(spc)
              } else {
                ## map discrete variable to discrete color palette
                ##---------------------------------------------------------

                if (is.null(spc$domain)) {
                  # get domain from data
                  if (is.factor(val)) {
                    spc$domain <- levels(val)
                  } else {
                    spc$domain <- unique(val)
                  }
                }
                if (is.null(spc$range)) {
                  spc$range <- x$theme$discrete$color(length(spc$domain))
                }
                if (is.null(spc$unknown)) {
                  spc$unknown <- x$theme$discrete$color_unknown
                }
                ## for consistency, we use d3 transform instead of ColorMapper
                cjs <- js_transform_color_cat(spc)
              }
              cjs_id <- digest::digest(list(val, attr_nm))
              cur_ly[[cjs_id]] <- cjs
              ly[[attr_nm]] <- list(
                field = sanitize(nm),
                transform = cjs$get_instance())
            } else if (grepl("size", attr_nm)) {
              ## handle size mappings
              ##---------------------------------------------------------

              if (is.numeric(val)) {
                ## map continuous variable to discrete size
                ##---------------------------------------------------------

                if (is.null(spc$domain)) {
                  # get domain from data
                  spc$domain <- range(val, na.rm = TRUE)
                }
                if (is.null(spc$range)) {
                  sz <- x$theme$continuous$size
                  spc$range <- c(sz$min, sz$max)
                  spc$exponent <- sz$exponent
                }
                cjs <- js_transform_size_num(spc)
                cjs_id <- digest::digest(list(val, attr_nm))
                cur_ly[[cjs_id]] <- cjs
                ly[[attr_nm]] <- list(
                  # units = "screen",
                  field = sanitize(nm),
                  transform = cjs$get_instance())
  # } else {
  #   if (is.factor(val)) {
  #     fcts <- levels(val)
  #   } else {
  #     fcts <- unique(val)
  #   }
  #   rng <- seq(5, 30, length = length(fcts))
  #   fcts_str <- paste0(fcts, ": ", rng, collapse = ", ")
  #   cjs <- CustomJSTransform$new(
  #     func = paste0(
  # "
  # var fcts = {", fcts_str, "};
  # return fcts[x];
  # "),
  #     v_func = paste0(
  # "
  # var fcts = {", fcts_str, "};
  # new_xs = new Array(xs.length);
  # for(i = 0; i < xs.length; i++) {
  # new_xs[i] = fcts[xs[i]];
  # }
  # return new_xs;
  # "))
  # }
                # TODO: figure out legend for these...
                cjs_id <- digest::digest(list(val, attr_nm))
                cur_ly[[cjs_id]] <- cjs
                ly[[attr_nm]] <- list(
                  # units = "screen",
                  field = sanitize(nm),
                  transform = cjs$get_instance())
              }
            }
          }
          # do this at the end because names can change in the mappers
          cols_to_use <- unique(c(cols_to_use, nm))
        }
      }

      ly <- ly[!sapply(ly, is.null)]

      ## process hover
      ##---------------------------------------------------------

      cur_hov_str <- as.character(cur_hov)
      cur_hov <- rlang::eval_tidy(cur_hov, ly$data)
      if (!is.null(cur_hov)) {
        pars <- list()
        if (inherits(cur_hov, "hover_spec")) {
          pars <- cur_hov
          pars$data <- rlang::eval_tidy(pars$data, ly$data)
        } else {
          pars$data <- cur_hov
        }
        pars$plot <- x$mods$plot$get_instance()
        if (is.character(pars$callback) && length(pars$callback) == 1) {
          cur_ly$transforms$hover <- CustomJS$new(code = pars$callback)
          pars$callback <- cur_ly$transforms$hover$get_instance()
        }

        if (is.data.frame(pars$data) || is.list(pars$data)) {
          hnm <- names(pars$data)
          if (length(hnm) == 0)
            hnm <- rep("", length(pars$data))
          dnm <- hnm
          for (ii in seq_along(pars$data)) {
            dval <- digest::digest(pars$data[[ii]])
            if (dval %in% col_dig) {
              nm <- col_nm[col_dig == dval]
              # deal with multiple column matches
              if (length(nm) > 1) {
                # see if we can find these names in the string expression
                nm_match <- which(sapply(nm, function(x) any(grepl(x, cur_hov_str))))
                if (length(nm_match) == 0) {
                  nm <- nm[1]
                } else {
                  nm <- nm[nm_match[1]]
                }
              }
              dnm[ii] <- nm
            } else {
              dnm[ii] <- paste0("col", length(ly$data) + 1, "___")
              ly$data[[dnm[ii]]] <- pars$data[[ii]]
            }
            cols_to_use <- unique(c(cols_to_use, dnm[ii]))
            if (hnm[ii] == "")
              hnm[ii] <- dnm[ii]
          }

          pars$tooltips <- lapply(seq_along(hnm), function(ii) {
            list(hnm[ii], paste0("@", sanitize(dnm[ii])))
          })
          pars$data <- NULL
          cur_ly$hover <- do.call(HoverTool$new, pars)
        } else if (is.character(pars$data) && length(pars$data) == 1) {
          if (grepl("@", pars$data)) {
            tmp_split <- strsplit(pars$data, "@")[[1]][-1]
            tmp_names <- gsub("^([a-zA-Z0-9_\\.]+).*", "\\1", tmp_split)
            for (nm in tmp_names) {
              # TODO: should probably apply something to escape grep chars in "nm"
              pars$data <- gsub(nm, sanitize(nm), pars$data)
              if (nm %in% names(ly$data))
                cols_to_use <- unique(c(cols_to_use, nm))
              # TODO: warn if @ is not available in data?
            }
          }
          pars$tooltips <- pars$data
          pars$data <- NULL
          cur_ly$hover <- do.call(HoverTool$new, pars)
        } else if (is.null(pars$data)) {
          pars$data <- NULL
          pars$tooltips <- NA
          cur_ly$hover <- do.call(HoverTool$new, pars)
        } else {
          message("Could not recognize hover specification... Ignoring.")
        }
      }

      ## data source
      ##---------------------------------------------------------

      if (ly$use_all_data)
        cols_to_use <- names(ly$data)

      dat <- as.list(ly$data)[cols_to_use]
      names(dat) <- sanitize(names(dat))

      if (!is.null(ly$ajax_data)) {
        # we can't use an Ajax data source if R has modified the columns
        if (!all(cols_to_use %in% ly$ajax_data$column_names)) {
          stop("Cannot use an Ajax data source with variables updated by R.", call. = FALSE)
        }
        # need to set an empty "data" attribute
        dat <- lapply(ly$ajax_data$column_names, function(a) list())
        names(dat) <- ly$ajax_data$column_names
        ly$ajax_data$data <- dat
        cur_ly$data_source <- do.call(AjaxDataSource$new, ly$ajax_data)
      } else {
        cur_ly$data_source <- ColumnDataSource$new(
          column_names = cols_to_use,
          data = lapply(dat, I), # I() makes correct JSON serialization in 1-row case
          name = paste(lnm, "data", sep = "_")
        )
      }

      # we don't want to duplicate data sources
      # compare props supplied to data source (except ID)
      # if there's already a match, use that data source and remove current one
      ds_dgst <- digest::digest(cur_ly$data_source$get_all_props(id = FALSE))
      if (ds_dgst %in% names(data_sources)) {
        cur_data_source_instance <- data_sources[[ds_dgst]]
        cur_ly$data_source <- NULL
      } else {
        cur_data_source_instance <- cur_ly$data_source$get_instance()
        data_sources[[ds_dgst]] <- cur_data_source_instance
      }

      if (is.null(ly$size))
        ly$size <- 10 # TODO: get from theme?

      ## glyph
      ##---------------------------------------------------------

      # resolve color -> line_color and fill_color
      if (is.null(ly$color))
        ly$color <- x$theme$discrete$color(1)
      if (is.null(ly$line_color))
        ly$line_color <- ly$color
      if (is.null(ly$fill_color))
        ly$fill_color <- ly$color

      # resolve alpha -> line_alphg and fill_alpha
      if (is.null(ly$alpha))
        ly$alpha <- 1 # TODO: get this from theme?
      if (is.null(ly$line_alpha))
        ly$line_alpha <- ly$alpha
      if (is.null(ly$fill_alpha))
        ly$fill_alpha <- ly$alpha * 0.5 # TODO: get this from theme? (fill_alpha_factor?)

      ly <- fix_glyph_color_alpha(ly, cur_glyph_props)

      cur_ly$glyph <- do.call(cur_model$new, c(ly[intersect(names(ly), par_nms)],
        list(name = paste(lnm, "glyph", sep = "_"))))

      ## non-selection glyph
      ##---------------------------------------------------------

      # resolve color -> line_color and fill_color
      if (is.null(ly$ns_color))
        ly$ns_color <- "#bbbbbb" # TODO: make this part of theme (nonselection_color)
      if (is.null(ly$ns_line_color))
        ly$ns_line_color <- ly$ns_color
      if (is.null(ly$ns_fill_color))
        ly$ns_fill_color <- ly$ns_color

      # resolve alpha -> line_alphg and fill_alpha
      if (is.null(ly$ns_alpha))
        ly$ns_alpha <- 0.75 # TODO: get this from theme?
      if (is.null(ly$ns_line_alpha))
        ly$ns_line_alpha <- ly$ns_alpha
      if (is.null(ly$ns_fill_alpha))
        ly$ns_fill_alpha <- ly$ns_alpha * 0.5 # TODO: get this from theme? (fill_alpha_factor?)

      ly2 <- get_alt_glyph_layer(ly, "ns_")
      ly2 <- fix_glyph_color_alpha(ly2, cur_glyph_props)

      cur_ly$ns_glyph <- do.call(cur_model$new, c(ly2[intersect(names(ly2), par_nms)],
        list(name = paste(lnm, "ns_glyph", sep = "_"))))

      ## hover glyph
      ##---------------------------------------------------------

      # resolve color -> line_color and fill_color
      if (!is.null(ly$hov_color) && is.null(ly$hov_line_color))
        ly$hov_line_color <- ly$hov_color
      if (!is.null(ly$hov_color) && is.null(ly$hov_fill_color))
        ly$hov_fill_color <- ly$hov_color
      if (is.null(ly$hov_line_color))
        ly$hov_line_color <- ly$line_color
      if (is.null(ly$hov_fill_color))
        ly$hov_fill_color <- ly$fill_color

      # resolve alpha -> line_alpha and fill_alpha
      if (!is.null(ly$hov_alpha) && is.null(ly$hov_line_alpha))
        ly$hov_line_alpha <- ly$hov_alpha
      if (!is.null(ly$hov_alpha) && is.null(ly$hov_fill_alpha))
        ly$hov_fill_alpha <- ly$hov_alpha * 0.5
      if (is.null(ly$hov_line_alpha))
        ly$hov_line_alpha <- 1 # TODO: get this from theme?
      if (is.null(ly$hov_fill_alpha))
        ly$hov_fill_alpha <- 1 # TODO: get this from theme?

      ly2 <- get_alt_glyph_layer(ly, "hov_")
      ly2 <- fix_glyph_color_alpha(ly2, cur_glyph_props)

      cur_ly$hov_glyph <- do.call(cur_model$new, c(ly2[intersect(names(ly2), par_nms)],
        list(name = paste(lnm, "hov_glyph", sep = "_"))))

      ## selection glyph
      ##---------------------------------------------------------

      # resolve color -> line_color and fill_color
      if (!is.null(ly$sel_color) && is.null(ly$sel_line_color))
        ly$sel_line_color <- ly$sel_color
      if (!is.null(ly$sel_color) && is.null(ly$sel_fill_color))
        ly$sel_fill_color <- ly$sel_color
      if (is.null(ly$sel_line_color))
        ly$sel_line_color <- ly$line_color
      if (is.null(ly$sel_fill_color))
        ly$sel_fill_color <- ly$fill_color

      # resolve alpha -> line_alphg and fill_alpha
      if (!is.null(ly$sel_alpha) && is.null(ly$sel_line_alpha))
        ly$sel_line_alpha <- ly$sel_alpha
      if (!is.null(ly$sel_alpha) && is.null(ly$sel_fill_alpha))
        ly$sel_fill_alpha <- ly$sel_alpha * 0.5
      if (is.null(ly$sel_line_alpha))
        ly$sel_line_alpha <- ly$line_alpha
      if (is.null(ly$sel_fill_alpha))
        ly$sel_fill_alpha <- ly$fill_alpha

      ly2 <- get_alt_glyph_layer(ly, "sel_")
      ly2 <- fix_glyph_color_alpha(ly2, cur_glyph_props)

      cur_ly$sel_glyph <- do.call(cur_model$new, c(ly2[intersect(names(ly2), par_nms)],
        list(name = paste(lnm, "sel_glyph", sep = "_"))))

      ## glyph renderer
      ##---------------------------------------------------------

      cur_ly$glyph_renderer <- GlyphRenderer$new(
        data_source = cur_data_source_instance,
        glyph = cur_ly$glyph$get_instance(),
        nonselection_glyph = cur_ly$ns_glyph$get_instance(),
        selection_glyph = cur_ly$sel_glyph$get_instance(),
        hover_glyph = cur_ly$hov_glyph$get_instance(),
        name = paste(lnm, "glyph_rend", sep = "_")
      )

      if (!is.null(cur_ly$hover))
        cur_ly$hover$set_prop("renderers", list(cur_ly$glyph_renderer$get_instance()))

      ## add renderer to legend item
      ##---------------------------------------------------------

      if (!is.null(cur_ly$legend_item))
        cur_ly$legend_item$set_prop("renderers",
          list(cur_ly$glyph_renderer$get_instance()))

      x$mods$layers[[lnm]] <- cur_ly
    } else if (ly$type == "annotation") {

      ## deal with annotation layers
      ##---------------------------------------------------------

      if (!is.null(ly$data))
        names(ly$data) <- sanitize(names(ly$data))
      can_be_vector <- get_can_be_vector(ly$model)
      col_dig <- unlist(lapply(ly$data, digest::digest))
      col_nm <- names(ly$data)

      # track whether the data is actually used in any of the quosures
      # it may not be used since data is inherited from the global figure object
      # TODO: do this for glyphs as well...
      data_needed <- FALSE

      for (attr_nm in names(ly)) {
        if (rlang::is_quosure(ly[[attr_nm]])) {
          # if evaluating without the data doesn't work, we need the data
          tmp <- try(rlang::eval_tidy(ly[[attr_nm]]), silent = TRUE)
          if (inherits(tmp, "try-error"))
            data_needed <- TRUE
          val <- rlang::eval_tidy(ly[[attr_nm]], ly$data)
        } else {
          val <- ly[[attr_nm]]
        }

        if (is.null(val)) {
          ly[attr_nm] <- list(NULL)
        } else if (length(val) == 1) {
          ly[attr_nm] <- list(val)
        } else if (attr_nm %in% can_be_vector) {
          dval <- digest::digest(val)
          if (dval %in% col_dig) {
            nm <- col_nm[col_dig == dval]
            # deal with multiple column matches
            if (length(nm) > 1) {
              qnm <- quo_name(ly[[attr_nm]])
              # if the expression supplied directly maps to a variable name, use it
              if (qnm %in% col_nm) {
                nm <- qnm
              } else {
                nm <- nm[1]
              }
            }
          } else {
            nm <- paste0("col", length(ly$data) + 1, "___")
            ly$data[[nm]] <- val
            if (!is.data.frame(ly$data))
              ly$data <- as.data.frame(ly$data)
          }
          ly[[attr_nm]] <- list(field = nm)
        } else {
          ly[attr_nm] <- list(val)
        }
      }

      # resolve color -> line_color and fill_color
      if (is.null(ly$color))
        ly$color <- x$theme$discrete$color(1)
      if (is.null(ly$line_color))
        ly$line_color <- ly$color
      if (is.null(ly$fill_color))
        ly$fill_color <- ly$color

      # resolve alpha -> line_alphg and fill_alpha
      if (is.null(ly$alpha))
        ly$alpha <- 1 # TODO: get this from theme?
      if (is.null(ly$line_alpha))
        ly$line_alpha <- ly$alpha
      if (is.null(ly$fill_alpha))
        ly$fill_alpha <- ly$alpha * 0.5 # TODO: get this from theme? (fill_alpha_factor?)

      for (arrvar in c("start", "end", "upper_head", "lower_head")) {
        if (!is.null(ly[[arrvar]])) {
          arr_mod <- get_arrow_mod(ly[[arrvar]], x$theme)
          cur_ly[[arr_mod$get_prop("id")]] <- arr_mod
          ly[[arrvar]] <- arr_mod$get_instance()
        }
      }

      if (!is.null(ly$data) && data_needed) {
        cur_ly$data_source <- ColumnDataSource$new(
          column_names = names(ly$data),
          data = lapply(ly$data, I), # I() makes correct JSON serialization in 1-row case
          name = paste(lnm, "data", sep = "_")
        )
        ly$source <- cur_ly$data_source$get_instance()
      }

      ly <- ly[!sapply(ly, is.null)]
      cur_mod <- utils::getFromNamespace(ly$model, "rbokeh")
      par_nms <- intersect(names(ly), get_init_formals(cur_mod))

      cur_ly$glyph_renderer <- do.call(cur_mod$new, c(ly[par_nms],
        list(level = "annotation", plot = x$mods$plot$get_instance(),
          name = paste(lnm, "glyph", sep = "_"))))

      x$mods$layers[[lnm]] <- cur_ly
    }
  }

  ## add model instances as arguments to callbacks
  ##---------------------------------------------------------

  mod_args <- lapply(x$mods$layers, function(a) {
    res <- lapply(a, function(b) {
      if (inherits(b, "Model") && !is.null(b$get_prop("name")))
        return(b$get_instance())
    })
    res_nms <- sapply(a, function(b) {
      if (inherits(b, "Model") && !is.null(b$get_prop("name")))
        return(b$get_prop("name"))
      return("")
    })
    names(res) <- res_nms
    res <- res[!sapply(res, is.null)]
  })
  mod_args <- unlist(unname(mod_args), recursive = FALSE)

  for (ii in seq_along(x$mods$layers)) {
    for (jj in seq_along(x$mods$layers[[ii]]$transforms))
    x$mods$layers[[ii]]$transforms[[jj]]$set_prop("args", mod_args)
  }

  ## add necessary models to get a plot
  ##---------------------------------------------------------

  x <- add_axes(x)
  x$mods <- add_title(x$mods, x$pars$title)
  x$mods <- add_tools(x$mods)
  x$mods <- add_legend(x$mods)

  for (whch in names(x$pars$axes$args))
    x$mods$plot$set_prop(whch, list(x$mods$axes[[whch]]$axis$get_instance()))

  xrng <- x$mods$axes[c("below", "above")][[1]]$range$get_instance()
  yrng <- x$mods$axes[c("left", "right")][[1]]$range$get_instance()
  x$mods$plot$set_prop("x_range", xrng)
  x$mods$plot$set_prop("y_range", yrng)

  xscl <- x$mods$axes[c("below", "above")][[1]]$scale$get_instance()
  yscl <- x$mods$axes[c("left", "right")][[1]]$scale$get_instance()
  x$mods$plot$set_prop("x_scale", xscl)
  x$mods$plot$set_prop("y_scale", yscl)

  x$mods$plot$set_prop("renderers", get_renderers(x$mods))
  if (!is.null(x$mods$title))
    x$mods$plot$set_prop("title", x$mods$title$get_instance())
  if (!is.null(x$mods$tool_events))
    x$mods$plot$set_prop("tool_events", x$mods$tool_events$get_instance())
  if (!is.null(x$mods$toolbar))
    x$mods$plot$set_prop("toolbar", x$mods$toolbar$get_instance())

  x$mods
}

## js mapper transforms
##---------------------------------------------------------

js_transform_color_num <- function(spc) {
  pal_str <- paste(spc$range, collapse = "', '")
  intr_str <- paste(spc$domain, collapse = ", ")
  args <- custom_js_transform(
    func = glue::glue("
      return color(x);
    "),
    global = glue::glue("
      var color = RBK.scaleThreshold()
        .domain([{intr_str}])
        .range(['{pal_str}']);")
  )
  do.call(CustomJSTransform$new, args)
}

js_transform_color_cat <- function(spc) {
  dom_str <- paste(spc$domain, collapse = "', '")
  pal_str <- paste(spc$range, collapse = "', '")
  args <- custom_js_transform(
    func = glue::glue("
      return color(x);
    "),
    global = glue::glue("
      var color = RBK.scaleOrdinal()
        .domain(['{dom_str}'])
        .range(['{pal_str}'])
        .unknown(['{spc$unknown}']);")
  )
  do.call(CustomJSTransform$new, args)
}

js_transform_size_num <- function(spc) {
  dom_str <- paste(spc$domain, collapse = ", ")
  pal_str <- paste(spc$range, collapse = ", ")
  args <- custom_js_transform(
    func = glue::glue("
      return size(x);
    "),
    global = glue::glue("
      var size = RBK.scalePow()
        .exponent({spc$exponent})
        .domain([{dom_str}])
        .range([{pal_str}]);")
  )
  do.call(CustomJSTransform$new, args)
}

# f <- function(x, dmn, rng)
#   rng[1] + (rng[2] - rng[1]) * (x - dmn[1]) / (dmn[2] - dmn[1])
# f(100, dmn, rng)
# f(200, dmn, rng)


## before using d3 scales:
# var intervals = [{intr_str}];
# var pal = ['{pal_str}'];
# var n = pal.length;
# var ii = 0;
# while (ii < n) {{
#   if (x > intervals[ii] && x <= intervals[ii + 1]) {{
#     return pal[ii];
#   }}
#   ii++;
# }};
# return '#eeeeee'

## using colormapper:
# cm_id <- digest::digest(list(val, "color"))
# cm <- cur_ly[[cm_id]]
# if (is.null(cm)) {
#   cm <- CategoricalColorMapper$new(
#     factors = spc$domain,
#     palette = spc$range
#   )
#   cur_ly[[cm_id]] <- cm
#   if (ly$legend)
#     cur_ly$legend_item <- LegendItem$new(
#       label = list(field = sanitize(nm)))
# }
# ly[[attr_nm]] <- list(
#   field = nm,
#   transform = cm$get_instance())


## ColorBar stuff (This was used for mapping continuous
##   attrs to properties but was replaced with binning and
##   inclusion in the legend. Salvage work here so we can
##   use it with the Image glyph)
##---------------------------------------------------------

# TODO: support log color mapper (via spec())
# cm <- LinearColorMapper$new(
#   low = min(val, na.rm = TRUE),
#   high = max(val, na.rm = TRUE),
#   # low_color = ,
#   # high_color = ,
#   # nan_color = ,
#   palette = bk_gradient_palettes$PuBuGn5
#   # x$theme$continuous...
# )

# # LogTickFormatter
# # LogTicker

# tckf <- BasicTickFormatter$new()
# tck <- BasicTicker$new()
# cb <- ColorBar$new(
#   color_mapper = cm$get_instance(),
#   formatter = tckf$get_instance(),
#   ticker = tck$get_instance(),
#   plot = x$mods$plot$get_instance())
# # label_standoff = 12
# # location = c(0, 0)

# # TODO: what if a right axis is being used?
# # TODO: ideally continuous, categorical mappers all in same legend
# #   not a color bar on the right and a legend inside, etc.
# x$mods$plot$set_prop("right", list(cb$get_instance()))
# cur_ly[[cm_id]] <- cm
# if (is.null(x$mods$color_bar)) {
#   x$mods$color_bar <- list(
#     cb = cb,
#     cb_tckf = tckf,
#     cb_tck = tck
#   )
# } else {
#   message("Currently only one color bar (continuous color mapping) is ",
#     "supported per plot. Ignoring additional continuous color mapping.")
# }



# get_next_layer_name <- function(obj) {
#   nms <- names(obj$x$mods$layers)
#   nms <- nms[grepl("^l[0-9]+", nms)]
#   if (length(nms) == 0)
#     return ("l1")
#   val <- as.integer(gsub("l(.*)", "\\1", nms))
#   paste0("l", max(val) + 1)
# }

add_axes <- function(x) {
  for (whch in names(x$pars$axes$args))
    x <- add_axis(x, whch)

  x
}

get_axis_type <- function(x, whch) {
  xy <- ifelse(whch %in% c("below", "above"), "x", "y")
  x$pars$axes$type[[xy]]
}

add_axis <- function(x, whch) {
  type <- get_axis_type(x, whch)
  xy <- ifelse(whch %in% c("below", "above"), "x", "y")
  args <- x$pars$axes$args[[whch]]

  if (!is.list(args$grid))
    args$grid <- list(visible = FALSE)

  # grid and axis need plot instances
  args$grid$plot <- x$mods$plot$get_instance()
  args$axis$plot <- x$mods$plot$get_instance()
  args$grid$dimension <- as.integer(whch %in% c("left", "right"))
  if (is.null(args$axis$axis_label))
    args$axis$axis_label <- x$pars$axes$lab[[xy]]

  if (type == "numeric") {
    drng <- x$pars$axes$range[[xy]]
    drng <- drng + c(-1, 1) * 0.07 * diff(drng) #TODO: make 0.07 padding factor part of theme
    if (is.null(args$range$start))
      args$range$start <- drng[1]
    if (is.null(args$range$end))
      args$range$end <- drng[2]
    # rng <- do.call(DataRange1d$new, args$range)
      rng <- DataRange1d$new()

    if (x$pars$axes$log[[xy]]) {
      if (is.null(args$ticker$model))
        args$ticker$model <- "LogTicker"
      if (is.null(args$tickformatter$model))
        args$tickformatter$model <- "LogTickFormatter"
      if (is.null(args$axis$model))
        args$axis$model <- "LogAxis"
      scl <- LogScale$new()
    } else {
      if (is.null(args$ticker$model))
        args$ticker$model <- "BasicTicker"
      if (is.null(args$tickformatter$model))
        args$tickformatter$model <- "BasicTickFormatter"
      if (is.null(args$axis$model))
        args$axis$model <- "LinearAxis"
      scl <- LinearScale$new()
    }
  } else if (type == "categorical") {
    if (is.null(args$ticker$model))
      args$ticker$model <- "CategoricalTicker"
    if (is.null(args$tickformatter$model))
      args$tickformatter$model <- "CategoricalTickFormatter"
    if (is.null(args$axis$model))
      args$axis$model <- "CategoricalAxis"

    if (is.null(args$range$factors))
      args$range$factors <- x$pars$axes$range[[xy]]
    rng <- do.call(FactorRange$new, args$range)
    scl <- CategoricalScale$new()
  } else if (type == "datetime") {
    if (is.null(args$ticker$model))
      args$ticker$model <- "DatetimeTicker"
    if (is.null(args$tickformatter$model))
      args$tickformatter$model <- "DatetimeTickFormatter"
    if (is.null(args$axis$model))
      args$axis$model <- "DatetimeAxis"

    drng <- x$pars$axes$range[[xy]]
    drng <- drng + c(-1, 1) * 0.07 * diff(drng) #TODO: make 0.07 padding factor part of theme
    if (is.null(args$range$start))
      args$range$start <- drng[1]
    if (is.null(args$range$end))
      args$range$end <- drng[2]
    rng <- do.call(DataRange1d$new, args$range)

    # what about MonthsTicker, DaysTicker, etc.?
    scl <- LinearScale$new()
  }

  tck_mod <- getFromNamespace(args$ticker$model, "rbokeh")
  args$ticker$model <- NULL
  tck <- do.call(tck_mod$new, args$ticker)

  args$grid$ticker <- tck$get_instance()
  grd <- do.call(Grid$new, args$grid)

  tf_mod <- getFromNamespace(args$tickformatter$model, "rbokeh")
  args$tickformatter$model <- NULL
  tf <- do.call(tf_mod$new, args$tickformatter)

  args$axis$formatter <- tf$get_instance()
  args$axis$ticker <- tck$get_instance()
  ax_mod <- getFromNamespace(args$axis$model, "rbokeh")
  args$axis$model <- NULL
  axs <- do.call(ax_mod$new, args$axis)

  if (is.null(x$mods$axes))
    x$mods$axes <- list()

  x$mods$axes[[whch]] <- list(
    ticker = tck,
    grid = grd,
    tick_formatter = tf,
    axis = axs,
    range = rng,
    scale = scl
  )

  x
}

add_title <- function(mods, args, title = NULL) {
  if (is.character(args$text)) {
    ttl <- do.call(Title$new, args)
    mods$title <- ttl
  }

  mods
}

add_tools <- function(mods) {
  ba <- BoxAnnotation$new(
    bottom_units = "screen",
    fill_alpha = 0.5,
    fill_color = "lightgrey",
    left_units = "screen",
    level = "overlay",
    line_alpha = 1,
    line_color = "black",
    line_dash = c(4, 4),
    line_width = 2,
    render_mode = "css",
    right_units = "screen",
    top_units = "screen"
  )

  bzt <- BoxZoomTool$new(
    overlay = ba$get_instance(),
    plot = mods$plot$get_instance()
  )

  bst <- BoxSelectTool$new(
    overlay = ba$get_instance(),
    plot = mods$plot$get_instance()
  )

  rst <- ResetTool$new(plot = mods$plot$get_instance())
  hlp <- HelpTool$new(plot = mods$plot$get_instance())
  pnt <- PanTool$new(plot = mods$plot$get_instance())
  wzt <- WheelZoomTool$new(plot = mods$plot$get_instance())
  svt <- SaveTool$new(plot = mods$plot$get_instance())

  mods$tools <- list(
    box_zoom = bzt,
    box_select = bst,
    reset = rst,
    help = hlp,
    pan = pnt,
    wheel_zoom = wzt,
    save = svt
  )

  te <- ToolEvents$new()

  tool_instances <- list(
    bzt$get_instance(),
    bst$get_instance(),
    rst$get_instance(),
    hlp$get_instance(),
    pnt$get_instance(),
    wzt$get_instance(),
    svt$get_instance())

  # add any hover instances to the list
  hov_instances <- unname(lapply(mods$layers, function(x) {
    if (!is.null(x$hover))
      return(x$hover$get_instance())
  }))
  hov_instances <- hov_instances[!sapply(hov_instances, is.null)]
  tool_instances <- c(tool_instances, hov_instances)

  tb <- Toolbar$new(
    active_drag = "auto",
    active_scroll = "auto",
    active_tap = "auto",
    tools = tool_instances
  )

  mods$box_zoom_ann <- ba
  mods$tool_events <- te
  mods$toolbar <- tb

  mods
}

add_legend <- function(mods) {
  lgnd_items <- unname(lapply(mods$layers, function(x) {
    if (is.null(x$legend_item))
      return(NULL)
    x$legend_item$get_instance()
  }))
  lgnd_items[sapply(lgnd_items, is.null)] <- NULL

  if (length(lgnd_items) > 0) {
    lgnd <- Legend$new(
      items = lgnd_items,
      plot = mods$plot$get_instance()
    )
    mods$legend <- lgnd
  }

  mods
}

get_renderers <- function(mods) {
  glrs <- unname(lapply(mods$layers, function(x) x$glyph_renderer$get_instance()))

  res <- c(glrs,
    unlist(unname(
      lapply(mods$axes, function(a) {
        res <- list(a$axis$get_instance())
        if (!is.null(a$grid))
          res <- c(res, list(a$grid$get_instance()))
        res
      })
    ), recursive = FALSE))

  if (!is.null(mods$legend))
    res[[length(res) + 1]] <- mods$legend$get_instance()

  if (!is.null(mods$box_zoom_ann))
    res[[length(res) + 1]] <- mods$box_zoom_ann$get_instance()

  if (!is.null(mods$color_bar$cb))
    res[[length(res) + 1]] <- mods$color_bar$cb$get_instance()

  res
}

## Note: old continuous color mapper
## This commented code is how it can be done with creating a new variable
## and using ColorMapper. However, we want to avoid adding new columns to
## the data at all costs so we'll make our own transform.
# val <- cut(val, intervals, include.lowest = TRUE)
# nm <- paste0("col", length(ly$data) + 1, "___")
# ly$data[[nm]] <- val
# fcts <- levels(val)
# palette <- x$theme$continuous$color(length(fcts))
## so we create a categorical color mapper instead using CustomJSTransform
