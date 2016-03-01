## get a figure ready to print
## this includes adding any elements that have not yet been explicitly added
## such as axes and ranges
## also, all deferred glyphs are resolved
##   (glyphs that need to know the final bounds of the figure)
## also, all attribute mappings are resolved and automatic legend entries are added
prepare_figure <- function(fig) {
  legend <- list()

  if(is.null(fig$x$spec$theme))
    fig$x$spec$theme <- bk_default_theme()

  ## resolve attribute mappings
  ## step through each layer
  for(ly in fig$x$spec$layers) {
    if(!is.null(ly$maps)) {
      ## step through each mapped attribute
      mapnames <- names(ly$maps)
      for(nm in mapnames) {
        map_item <- ly$maps[[nm]]
        if(is.numeric(map_item$domain)) {
          # the continuous domain cuts should be specifiable
          # and the default number of cuts should be a theme thing
          intervals <- pretty(map_item$domain, 6)
          nl <- length(intervals) - 1
          map_item$domain <- intervals
          map_item$labels <- levels(cut(map_item$domain, intervals, include.lowest = TRUE))
          map_item$values <- (utils::head(intervals, nl) + utils::tail(intervals, nl)) / 2
        } else {
          ## categorical domain
          map_item$labels <- map_item$domain
          map_item$values <- map_item$domain
        }
        ## map the glyphs attributes
        for(entry in map_item$map_entries) {
          did <- fig$x$spec$model[[entry$id]]$attributes$data_source$id
          gl <- fig$x$spec$model[[entry$id]]$attributes$glyph
          nsglid <- fig$x$spec$model[[entry$id]]$attributes$nonselection_glyph$id
          hovglid <- fig$x$spec$model[[entry$id]]$attributes$hover_glyph$id
          data_attr_names <- names(fig$x$spec$model[[did]]$attributes$data)
          glyph_attr_names <- names(fig$x$spec$model[[gl$id]]$attributes)
          for(attr in entry$map_args) {
            ## handle glyph type
            if(attr == "glyph") {
              new_type <- underscore2camel(get_theme_value(underscore2camel(map_item$domain), gl$type, attr,
                fig$x$spec$theme))
              ## should check things in resolve_glyph_props() with new glyph
              fig$x$spec$model[[entry$id]]$attributes$glyph$type <- new_type
              fig$x$spec$model[[gl$id]]$type <- new_type
              ## fix it in the non-selection glyph too
              if(!is.null(nsglid)) {
                fig$x$spec$model[[entry$id]]$attributes$nonselection_glyph$type <- new_type
                fig$x$spec$model[[nsglid]]$type <- new_type
              }
              if(!is.null(hovglid)) {
                fig$x$spec$model[[entry$id]]$attributes$hover_glyph$type <- new_type
                fig$x$spec$model[[hovglid]]$type <- new_type
              }
            } else {
              if(attr %in% data_attr_names) {
                cur_dat <- fig$x$spec$model[[did]]$attributes$data[[attr]]
                fig$x$spec$model[[did]]$attributes$data[[attr]] <- get_theme_value(map_item$domain, cur_dat, attr,
                  fig$x$spec$theme)
              } else if(attr %in% glyph_attr_names) {
                if(attr == "line_dash") {
                  cur_dat <- fig$x$spec$model[[gl$id]]$attributes[[attr]]
                  fig$x$spec$model[[gl$id]]$attributes[[attr]] <- get_theme_value(map_item$domain, cur_dat, attr,
                    fig$x$spec$theme)
                  fig$x$spec$model[[hovglid]]$attributes[[attr]] <- get_theme_value(map_item$domain, cur_dat, attr,
                    fig$x$spec$theme)
                } else {
                  cur_dat <- fig$x$spec$model[[gl$id]]$attributes[[attr]]$value
                  fig$x$spec$model[[gl$id]]$attributes[[attr]]$value <- get_theme_value(map_item$domain, cur_dat, attr,
                    fig$x$spec$theme)
                  fig$x$spec$model[[hovglid]]$attributes[[attr]]$value <- get_theme_value(map_item$domain, cur_dat, attr,
                    fig$x$spec$theme)
                }
              }
            }
          }
        }

        ## add legend glyphs and build legend element
        if(is.null(ly$do_legend)) {
          ## add a header to this entry of the legend
          # ## spacer if it's not the first legend group
          # if(nm != mapnames[1])
          #   legend[[paste0("lgnd_spacer_", nm)]] <- list(list("", list()))
          nm <- gsub("^\"|\"$", "", nm)
          legend[[paste0("lgnd_header_", nm)]] <- list(list(nm, list()))
          for(ii in seq_along(map_item$labels)) {
            cur_val <- map_item$values[[ii]]
            cur_lab <- map_item$labels[[ii]]
            lgnd_id <- paste(nm, cur_lab, sep = "_")
            legend[[lgnd_id]] <- list(list(paste0(" ", cur_lab), list()))

            for(glph in map_item$legend_glyphs) {
              for(mrg in glph$map_args)
                glph$args[[mrg]] <- get_theme_value(map_item$domain, cur_val, mrg, fig$x$spec$theme)
              # render legend glyph
              spec <- c(glph$args, list(x = "x", y = "y"))
              lgroup <- paste("__legend_", nm, "_", cur_lab, sep = "")
              lname <- glph$args$glyph
              glr_id <- gen_id(fig, c("glyph_renderer", lgroup, lname))
              # make it so legend glyph doesn't show up on page
              oox <- ifelse(fig$x$spec$x_axis_type == "categorical", "", NA)
              ooy <- ifelse(fig$x$spec$y_axis_type == "categorical", "", NA)
              if(!is.null(spec$size))
                spec$size <- 0
              if(!is.null(spec$radius))
                spec$radius <- 0
              if(is.null(spec$glyph))
                spec$glyph <- "Circle"
              fig <- fig %>% add_layer(spec = spec,
                dat = data.frame(x = c(oox, oox), y = c(ooy, ooy)),
                lname = lname, lgroup = lgroup)

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
  if(length(fig$x$spec$common_legend) > 0) {
    for(lg in fig$x$spec$common_legend) {
      lgroup <- paste("common_legend", lg$name, sep = "_")
      legend[[lgroup]] <- list(list(lg$name, list()))
      for(lg_args in lg$args) {
        spec <- c(lg_args, list(x = "x", y = "y"))
        lname <- lg_args$glyph
        glr_id <- gen_id(fig, c("glyph_renderer", lgroup, lname))
        # make it so legend glyph doesn't show up on page
        oox <- ifelse(fig$x$spec$x_axis_type == "categorical", "", NA)
        ooy <- ifelse(fig$x$spec$y_axis_type == "categorical", "", NA)
        if(!is.null(spec$size))
          spec$size <- 0
        if(!is.null(spec$radius))
          spec$radius <- 0

        fig <- fig %>% add_layer(spec = spec,
          dat = data.frame(x = c(oox, oox), y = c(ooy, ooy)),
          lname = lname, lgroup = lgroup)

        # add reference to glyph to legend object
        nn <- length(legend[[lgroup]][[1]][[2]]) + 1
        legend[[lgroup]][[1]][[2]][[nn]] <- list(type = "GlyphRenderer", id = glr_id)
      }
    }
  }

  if(length(legend) > 0 && !is.null(fig$x$spec$legend_attrs))
    fig <- fig %>%
      add_legend(unname(unlist(legend, recursive = FALSE)),
        fig$x$spec$legend_attrs)

  ## see if there is a log axis so we can compute padding appropriately
  ## log axis is only available if explicitly specified through x_axis()
  ## or y_axis(), so at this point, *_mapper_type should be defined
  x_log <- y_log <- FALSE
  if(!is.null(fig$x$spec$model$plot$attributes$x_mapper_type))
    x_log <- TRUE
  if(!is.null(fig$x$spec$model$plot$attributes$y_mapper_type))
    y_log <- TRUE

  ## set xlim and ylim if not set
  if(length(fig$x$spec$xlim) == 0) {
    # message("xlim not specified explicitly... calculating...")
    xrange <- get_all_glyph_range(fig$x$spec$glyph_x_ranges, fig$x$spec$padding_factor, fig$x$spec$x_axis_type, x_log)
  } else {
    xrange <- fig$x$spec$xlim
    if(inherits(xrange, c("Date", "POSIXt")))
      xrange <- to_epoch(xrange)
  }

  if(length(fig$x$spec$ylim) == 0) {
    # message("ylim not specified explicitly... calculating...")
    yrange <- get_all_glyph_range(fig$x$spec$glyph_y_ranges, fig$x$spec$padding_factor, fig$x$spec$y_axis_type, y_log)
  } else {
    yrange <- fig$x$spec$ylim
    if(inherits(yrange, c("Date", "POSIXt")))
      yrange <- to_epoch(yrange)
  }

  if(!fig$x$spec$has_x_range)
    fig <- fig %>% x_range(xrange)

  if(!fig$x$spec$has_y_range)
    fig <- fig %>% y_range(yrange)

  if(!fig$x$spec$has_x_axis) {
    if(is.null(fig$x$spec$xlab)) {
      this_xlab <- "x"
    } else {
      this_xlab <- fig$x$spec$xlab
    }
    if(fig$x$spec$xaxes == FALSE) {
      fig <- fig %>% x_axis(this_xlab, grid = fig$x$spec$xgrid, visible = FALSE)
    } else {
      fig <- fig %>% x_axis(this_xlab, grid = fig$x$spec$xgrid, position = fig$x$spec$xaxes)
    }
  }

  if(!fig$x$spec$has_y_axis) {
    if(is.null(fig$x$spec$ylab)) {
      this_ylab <- "y"
    } else {
      this_ylab <- fig$x$spec$ylab
    }
    if(fig$x$spec$yaxes == FALSE) {
      fig <- fig %>% y_axis(this_ylab, grid = fig$x$spec$ygrid, visible = FALSE)
    } else {
      fig <- fig %>% y_axis(this_ylab, grid = fig$x$spec$ygrid, position = fig$x$spec$yaxes)
    }
  }

  ## see if we need to execute any deferred functions
  if(length(fig$x$spec$glyph_defer) > 0) {
    for(dfr in fig$x$spec$glyph_defer) {
      tmp_spec <- dfr$fn(dfr$spec, xrange, yrange)
      tmp_data <- dfr$fn(dfr$data, xrange, yrange)
      fig <- fig %>% add_layer(tmp_spec, tmp_data, dfr$lname, dfr$lgroup)
    }
  }

  fig$x$spec$width <- fig$x$spec$model$plot$attributes$plot_width
  fig$x$spec$height <- fig$x$spec$model$plot$attributes$plot_height
  fig$x$spec$id <- fig$x$spec$model$plot$id

  # handle toolbar padding
  if(!is.null(fig$x$parenttype)) {
    # parenttype is set when plot is part of GridPlot, which has different padding
    x_pad <- y_pad <- 4
  } else {
    x_pad <- y_pad <- 10
  }
  tb <- fig$x$spec$model$plot$attributes$toolbar_location
  if(is.null(tb)) {
    if(!"toolbar_location" %in% names(fig$x$spec$model$plot$attributes))
      x_pad <- 46
  } else if(tb %in% c("above", "below")) {
    y_pad <- 46
  } else if(tb %in% c("left", "right")) {
    x_pad <- 46
  }

  fig$x$padding <- list(type = "figure", y_pad = y_pad, x_pad = x_pad)
  fig$x$spec$model$plot$attributes$plot_width <- fig$width - y_pad
  fig$x$spec$model$plot$attributes$plot_height <- fig$height - x_pad

  # handle plot/axis/grid/legend themes
  if(!is.null(fig$x$spec$theme$plot))
    fig <- fig %>% theme_plot(pars = fig$x$spec$theme$plot)
  if(!is.null(fig$x$spec$theme$axis))
    fig <- fig %>% theme_axis(c("x", "y"), pars = fig$x$spec$theme$axis)
  if(!is.null(fig$x$spec$theme$grid))
    fig <- fig %>% theme_grid(c("x", "y"), pars = fig$x$spec$theme$grid)
  if(!is.null(fig$x$spec$theme$legend))
    fig <- fig %>% theme_legend(pars = fig$x$spec$theme$legend)

  fig
}
