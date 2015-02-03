## get a figure ready to print
## this includes adding any elements that have not yet been explicitly added
## such as axes and ranges
## also, all deferred glyphs are resolved
##   (glyphs that need to know the final bounds of the figure)
## also, all aesthetic mappings are resolved and automatic legend entries are added
prepare_figure <- function(fig) {
  legend <- list()

  ## resolve aesthetic mappings
  for(ly in fig$layers) {
    if(!is.null(ly$maps)) {
      for(nm in names(ly$maps)) {
        mapItem <- ly$maps[[nm]]
        if(is.numeric(mapItem$domain)) {
          intervals <- pretty(mapItem$domain, 6)
          nl <- length(intervals) - 1
          mapItem$domain <- intervals
          mapItem$labels <- levels(cut(mapItem$domain, intervals, include.lowest = TRUE))
          mapItem$values <- (head(intervals, nl) + tail(intervals, nl)) / 2
        } else {
          mapItem$labels <- mapItem$domain
          mapItem$values <- mapItem$domain
        }
        ## map the glyphs attributes
        for(entry in mapItem$mapEntries) {
          did <- fig$model[[entry$id]]$attributes$data_source$id
          gl <- fig$model[[entry$id]]$attributes$glyph
          nsglid <- fig$model[[entry$id]]$attributes$nonselection_glyph$id
          dataAttrNames <- names(fig$model[[did]]$attributes$data)
          glyphAttrNames <- names(fig$model[[gl$id]]$attributes)
          for(attr in entry$mapArgs) {
            ## handle glyph type
            if(attr == "glyph") {
              newType <- underscore2camel(getThemeValue(underscore2camel(mapItem$domain), gl$type, attr))
              ## should check things in resolveGlyphProps() with new glyph
              fig$model[[entry$id]]$attributes$glyph$type <- newType
              fig$model[[gl$id]]$type <- newType
              ## fix it in the non-selection glyph too
              if(!is.null(nsglid)) {
                fig$model[[entry$id]]$attributes$nonselection_glyph$type <- newType
                fig$model[[nsglid]]$type <- newType
              }
            } else {
              if(attr %in% dataAttrNames) {
                curDat <- fig$model[[did]]$attributes$data[[attr]]
                fig$model[[did]]$attributes$data[[attr]] <- getThemeValue(mapItem$domain, curDat, attr)
              } else if(attr %in% glyphAttrNames) {
                if(attr == "line_dash") {
                  curDat <- fig$model[[gl$id]]$attributes[[attr]]
                  fig$model[[gl$id]]$attributes[[attr]] <- getThemeValue(mapItem$domain, curDat, attr)
                } else {
                  curDat <- fig$model[[gl$id]]$attributes[[attr]]$value
                  fig$model[[gl$id]]$attributes[[attr]]$value <- getThemeValue(mapItem$domain, curDat, attr)
                }
              }
            }
          }
        }

        ## add legend glyphs and build legend element
        for(ii in seq_along(mapItem$labels)) {
          curVal <- mapItem$values[[ii]]
          curLab <- mapItem$labels[[ii]]
          lgndId <- paste(nm, curLab, sep = "_")
          legend[[lgndId]] <- list(list(curLab, list()))

          for(glph in mapItem$legendGlyphs) {
            for(mrg in glph$mapArgs)
              glph$args[[mrg]] <- getThemeValue(mapItem$domain, curVal, mrg)
            # render legend glyph
            spec <- c(glph$args, list(x = "x", y = "y"))
            lgroup <- paste("legend_", nm, "_", curLab, sep = "")
            lname <- glph$args$glyph
            glrId <- genId(fig, c("glyphRenderer", lgroup, lname))
            # make it so legend glyph doesn't show up on page
            oo <- NA
            if(!is.null(spec$size))
              spec$size <- NA
            if(!is.null(spec$radius))
              spec$radius <- NA
            fig <- fig %>% addLayer(spec = spec, dat = data.frame(x = c(oo, oo), y = c(oo, oo)), lname = lname, lgroup = lgroup)

            # add reference to glyph to legend object
            nn <- length(legend[[lgndId]][[1]][[2]]) + 1
            legend[[lgndId]][[1]][[2]][[nn]] <- list(type = "GlyphRenderer", id = glrId)
          }
        }
      }
    }
  }

  ## deal with common legend, if any
  if(length(fig$commonLegend) > 0) {
    for(lg in fig$commonLegend) {
      lgroup <- paste("common_legend", lg$name, sep = "_")
      legend[[lgroup]] <- list(list(lg$name, list()))
      for(lgArgs in lg$args) {
        spec <- c(lgArgs, list(x = "x", y = "y"))
        lname <- lgArgs$glyph
        glrId <- genId(fig, c("glyphRenderer", lgroup, lname))
        # make it so legend glyph doesn't show up on page
        oo <- NA
        if(!is.null(spec$size))
          spec$size <- NA
        if(!is.null(spec$radius))
          spec$radius <- NA
        fig <- fig %>% addLayer(spec = spec, dat = data.frame(x = c(oo, oo), y = c(oo, oo)), lname = lname, lgroup = lgroup)

        # add reference to glyph to legend object
        nn <- length(legend[[lgroup]][[1]][[2]]) + 1
        legend[[lgroup]][[1]][[2]][[nn]] <- list(type = "GlyphRenderer", id = glrId)
      }
    }
  }

  if(length(legend) > 0)
    fig <- fig %>% addLegend(unname(unlist(legend, recursive = FALSE)))

  ## see if there is a log axis so we can compute padding appropriately
  ## log axis is only available if explicitly specified through x_axis()
  ## or y_axis(), so at this point, *_mapper_type should be defined
  xLog <- yLog <- FALSE
  if(!is.null(fig$model$plot$attributes$x_mapper_type))
    xLog <- TRUE
  if(!is.null(fig$model$plot$attributes$y_mapper_type))
    yLog <- TRUE

  ## set xlim and ylim if not set
  if(length(fig$xlim) == 0) {
    message("xlim not specified explicitly... calculating...")
    xrange <- getAllGlyphRange(fig$glyphXRanges, fig$padding_factor, fig$xAxisType, xLog)
  } else {
    xrange <- fig$xlim
  }

  if(length(fig$ylim) == 0) {
    message("ylim not specified explicitly... calculating...")
    yrange <- getAllGlyphRange(fig$glyphYRanges, fig$padding_factor, fig$yAxisType, yLog)
  } else {
    yrange <- fig$ylim
  }

  fig <- fig %>%
    x_range(xrange) %>%
    y_range(yrange)

  if(!fig$hasXaxis) {
    if(is.null(fig$xlab)) {
      fig <- fig %>% x_axis("x", grid = fig$xgrid, position = fig$xaxes)
    } else {
      fig <- fig %>% x_axis(fig$xlab, grid = fig$xgrid, position = fig$xaxes)
    }
  }

  if(!fig$hasYaxis) {
    if(is.null(fig$ylab)) {
      fig <- fig %>% y_axis("y", grid = fig$ygrid, position = fig$yaxes)
    } else {
      fig <- fig %>% y_axis(fig$ylab, grid = fig$ygrid, position = fig$yaxes)
    }
  }

  ## see if we need to execute any deferred functions
  if(length(fig$glyphDefer) > 0) {
    for(dfr in fig$glyphDefer) {
      tmpSpec <- dfr$fn(dfr$spec, xrange, yrange)
      tmpData <- dfr$fn(dfr$data, xrange, yrange)
      fig <- fig %>% addLayer(tmpSpec, tmpData, dfr$lname, dfr$lgroup)
    }
  }

  fig$width <- fig$model$plot$attributes$plot_width
  fig$height <- fig$model$plot$attributes$plot_height
  fig$id <- fig$model$plot$id

  names(fig$model$plot$attributes$tools) <- NULL
  names(fig$model$plot$attributes$renderers) <- NULL
  names(fig$model) <- NULL

  fig
}
