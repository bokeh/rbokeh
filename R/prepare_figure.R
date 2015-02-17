## get a figure ready to print
## this includes adding any elements that have not yet been explicitly added
## such as axes and ranges
## also, all deferred glyphs are resolved
##   (glyphs that need to know the final bounds of the figure)
## also, all attribute mappings are resolved and automatic legend entries are added
prepare_figure <- function(fig) {
  legend <- list()

  ## resolve attribute mappings
  for(ly in fig$layers) {
    if(!is.null(ly$maps)) {
      for(nm in names(ly$maps)) {
        map_item <- ly$maps[[nm]]
        if(is.numeric(map_item$domain)) {
          intervals <- pretty(map_item$domain, 6)
          nl <- length(intervals) - 1
          map_item$domain <- intervals
          map_item$labels <- levels(cut(map_item$domain, intervals, include.lowest = TRUE))
          map_item$values <- (head(intervals, nl) + tail(intervals, nl)) / 2
        } else {
          map_item$labels <- map_item$domain
          map_item$values <- map_item$domain
        }
        ## map the glyphs attributes
        for(entry in map_item$map_entries) {
          did <- fig$model[[entry$id]]$attributes$data_source$id
          gl <- fig$model[[entry$id]]$attributes$glyph
          nsglid <- fig$model[[entry$id]]$attributes$nonselection_glyph$id
          data_attr_names <- names(fig$model[[did]]$attributes$data)
          glyph_attr_names <- names(fig$model[[gl$id]]$attributes)
          for(attr in entry$map_args) {
            ## handle glyph type
            if(attr == "glyph") {
              new_type <- underscore2camel(get_theme_value(underscore2camel(map_item$domain), gl$type, attr))
              ## should check things in resolve_glyph_props() with new glyph
              fig$model[[entry$id]]$attributes$glyph$type <- new_type
              fig$model[[gl$id]]$type <- new_type
              ## fix it in the non-selection glyph too
              if(!is.null(nsglid)) {
                fig$model[[entry$id]]$attributes$nonselection_glyph$type <- new_type
                fig$model[[nsglid]]$type <- new_type
              }
            } else {
              if(attr %in% data_attr_names) {
                cur_dat <- fig$model[[did]]$attributes$data[[attr]]
                fig$model[[did]]$attributes$data[[attr]] <- get_theme_value(map_item$domain, cur_dat, attr)
              } else if(attr %in% glyph_attr_names) {
                if(attr == "line_dash") {
                  cur_dat <- fig$model[[gl$id]]$attributes[[attr]]
                  fig$model[[gl$id]]$attributes[[attr]] <- get_theme_value(map_item$domain, cur_dat, attr)
                } else {
                  cur_dat <- fig$model[[gl$id]]$attributes[[attr]]$value
                  fig$model[[gl$id]]$attributes[[attr]]$value <- get_theme_value(map_item$domain, cur_dat, attr)
                }
              }
            }
          }
        }

        ## add legend glyphs and build legend element
        if(is.null(ly$do_legend)) {
          for(ii in seq_along(map_item$labels)) {
            cur_val <- map_item$values[[ii]]
            cur_lab <- map_item$labels[[ii]]
            lgnd_id <- paste(nm, cur_lab, sep = "_")
            legend[[lgnd_id]] <- list(list(cur_lab, list()))

            for(glph in map_item$legend_glyphs) {
              for(mrg in glph$map_args)
                glph$args[[mrg]] <- get_theme_value(map_item$domain, cur_val, mrg)
              # render legend glyph
              spec <- c(glph$args, list(x = "x", y = "y"))
              lgroup <- paste("legend_", nm, "_", cur_lab, sep = "")
              lname <- glph$args$glyph
              glr_id <- gen_id(fig, c("glyph_renderer", lgroup, lname))
              # make it so legend glyph doesn't show up on page
              oox <- ifelse(fig$x_axis_type == "categorical", "", NA)
              ooy <- ifelse(fig$y_axis_type == "categorical", "", NA)
              if(!is.null(spec$size))
                spec$size <- NA
              if(!is.null(spec$radius))
                spec$radius <- NA
              fig <- fig %>% add_layer(spec = spec, dat = data.frame(x = c(oox, oox), y = c(ooy, ooy)), lname = lname, lgroup = lgroup)

              # add reference to glyph to legend object
              nn <- length(legend[[lgnd_id]][[1]][[2]]) + 1
              legend[[lgnd_id]][[1]][[2]][[nn]] <- list(type = "GlyphRenderer", id = glr_id)
            }
          }
        }
      }
    }
  }

  ## deal with common legend, if any
  if(length(fig$common_legend) > 0) {
    for(lg in fig$common_legend) {
      lgroup <- paste("common_legend", lg$name, sep = "_")
      legend[[lgroup]] <- list(list(lg$name, list()))
      for(lg_args in lg$args) {
        spec <- c(lg_args, list(x = "x", y = "y"))
        lname <- lg_args$glyph
        glr_id <- gen_id(fig, c("glyph_renderer", lgroup, lname))
        # make it so legend glyph doesn't show up on page
        oox <- ifelse(fig$x_axis_type == "categorical", "", NA)
        ooy <- ifelse(fig$y_axis_type == "categorical", "", NA)
        if(!is.null(spec$size))
          spec$size <- NA
        if(!is.null(spec$radius))
          spec$radius <- NA
        fig <- fig %>% add_layer(spec = spec, dat = data.frame(x = c(oox, oox), y = c(ooy, ooy)), lname = lname, lgroup = lgroup)

        # add reference to glyph to legend object
        nn <- length(legend[[lgroup]][[1]][[2]]) + 1
        legend[[lgroup]][[1]][[2]][[nn]] <- list(type = "GlyphRenderer", id = glr_id)
      }
    }
  }

  if(length(legend) > 0)
    fig <- fig %>% add_legend(unname(unlist(legend, recursive = FALSE)))

  ## see if there is a log axis so we can compute padding appropriately
  ## log axis is only available if explicitly specified through x_axis()
  ## or y_axis(), so at this point, *_mapper_type should be defined
  x_log <- y_log <- FALSE
  if(!is.null(fig$model$plot$attributes$x_mapper_type))
    x_log <- TRUE
  if(!is.null(fig$model$plot$attributes$y_mapper_type))
    y_log <- TRUE

  ## set xlim and ylim if not set
  if(length(fig$xlim) == 0) {
    message("xlim not specified explicitly... calculating...")
    xrange <- get_all_glyph_range(fig$glyph_x_ranges, fig$padding_factor, fig$x_axis_type, x_log)
  } else {
    xrange <- fig$xlim
  }

  if(length(fig$ylim) == 0) {
    message("ylim not specified explicitly... calculating...")
    yrange <- get_all_glyph_range(fig$glyph_y_ranges, fig$padding_factor, fig$y_axis_type, y_log)
  } else {
    yrange <- fig$ylim
  }

  if(!fig$has_x_range)
    fig <- fig %>% x_range(xrange)

  if(!fig$has_y_range)
    fig <- fig %>% y_range(yrange)

  if(!fig$has_x_axis) {
    if(is.null(fig$xlab)) {
      this_xlab <- "x"
    } else {
      this_xlab <- fig$xlab
    }
    if(fig$xaxes == FALSE) {
      fig <- fig %>% x_axis(this_xlab, grid = fig$xgrid, visible = FALSE)
    } else {
      fig <- fig %>% x_axis(this_xlab, grid = fig$xgrid, position = fig$xaxes)
    }
  }

  if(!fig$has_y_axis) {
    if(is.null(fig$ylab)) {
      this_ylab <- "y"
    } else {
      this_ylab <- fig$ylab
    }
    if(fig$yaxes == FALSE) {
      fig <- fig %>% y_axis(this_ylab, grid = fig$ygrid, visible = FALSE)
    } else {
      fig <- fig %>% y_axis(this_ylab, grid = fig$ygrid, position = fig$yaxes)
    }
  }

  ## see if we need to execute any deferred functions
  if(length(fig$glyph_defer) > 0) {
    for(dfr in fig$glyph_defer) {
      tmp_spec <- dfr$fn(dfr$spec, xrange, yrange)
      tmp_data <- dfr$fn(dfr$data, xrange, yrange)
      fig <- fig %>% add_layer(tmp_spec, tmp_data, dfr$lname, dfr$lgroup)
    }
  }

  fig$width <- fig$model$plot$attributes$plot_width
  fig$height <- fig$model$plot$attributes$plot_height
  fig$id <- fig$model$plot$id

  fig
}
