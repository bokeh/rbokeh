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

  fig <- x

  if(length(fig$layers) == 0) {
    message("This figure is empty...")
  } else {
    ## put options together
    options <- list()
    for(opt in optionNames) {
      fld <- fig[[opt]]
      if(length(fld) > 0)
        options[[opt]] <- fld
    }

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
            for(attr in entry$mapArgs) {
              ## handle glyph type
              if(attr == "glyph") {
                gl <- fig$model[[entry$id]]$attributes$glyph
                newType <- underscore2camel(getThemeValue(underscore2camel(mapItem$domain), gl$type, attr))
                fig$model[[entry$id]]$attributes$glyph$type <- newType
                fig$model[[gl$id]]$type <- newType
              } else {
                curDat <- fig$model[[did]]$attributes$data[[attr]]
                fig$model[[did]]$attributes$data[[attr]] <- getThemeValue(mapItem$domain, curDat, attr)
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
              fig <- fig %>% addLayer(spec = spec, dat = data.frame(x = c(NA, NA), y = c(NA, NA)), lname = lname, lgroup = lgroup)

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
          fig <- fig %>% addLayer(spec = spec, dat = data.frame(x = c(NA, NA), y = c(NA, NA)), lname = lname, lgroup = lgroup)

          # add reference to glyph to legend object
          nn <- length(legend[[lgroup]][[1]][[2]]) + 1
          legend[[lgroup]][[1]][[2]][[nn]] <- list(type = "GlyphRenderer", id = glrId)
        }
      }
    }

    if(length(legend) > 0)
      fig <- fig %>% addLegend(unname(unlist(legend, recursive = FALSE)))

    ## set xlim and ylim if not set
    if(length(fig$xlim) == 0) {
      message("xlim not specified explicitly... calculating...")
      options$xrange <- getAllGlyphRange(fig$glyphXRanges, fig$padding_factor, fig$xAxisType)
    } else {
      options$xrange <- fig$xlim
    }
    if(length(fig$ylim) == 0) {
      message("ylim not specified explicitly... calculating...")
      options$yrange <- getAllGlyphRange(fig$glyphYRanges, fig$padding_factor, fig$yAxisType)
    } else {
      options$yrange <- fig$ylim
    }

    # RJSONIO fix
    if(length(options$yrange) == 1)
      options$yrange <- list(options$yrange)
    if(length(options$xrange) == 1)
      options$xrange <- list(options$xrange)

    ######
    fig <- fig %>%
      x_range(options$xrange) %>%
      y_range(options$yrange)

    if(is.null(fig$xlab)) {
      fig <- fig %>% x_axis("x", grid = fig$xgrid, position = fig$xaxes)
    } else {
      fig <- fig %>% x_axis(fig$xlab, grid = fig$xgrid, position = fig$xaxes)
    }
    if(is.null(fig$ylab)) {
      fig <- fig %>% y_axis("y", grid = fig$ygrid, position = fig$yaxes)
    } else {
      fig <- fig %>% y_axis(fig$ylab, grid = fig$ygrid, position = fig$yaxes)
    }

    id <- fig$model[[which(sapply(fig$model, function(x) x$type) == "Plot")]]$id
    ######

    ## see if we need to execute any deferred functions
    if(length(fig$glyphDefer) > 0) {
      for(dfr in fig$glyphDefer) {
        tmpSpec <- dfr$fn(dfr$spec, options$xrange, options$yrange)
        tmpData <- dfr$fn(dfr$data, options$xrange, options$yrange)
        fig <- fig %>% addLayer(tmpSpec, tmpData, dfr$lname, dfr$lgroup)
      }
    }
    options$dims <- c(options$width, options$height)
    options$width <- NULL
    options$height <- NULL

    ## create widget
    htmlwidgets::createWidget(
       name = 'rbokeh',
       list(
          r_debug = debug,
          all_models = get_json(fig$model),
          elementid = digest(Sys.time()),
          modelid = id
       ),
       width = options$dims[1] + 50,
       height = options$dims[2] + 50,
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

