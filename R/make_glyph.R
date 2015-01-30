# glyphs are stored in layers
# layers have names (lname) and group names (lgroup)
# you can add new layers to an existing layer group
# which will ensure that aesthetics are mapped to glyphs within the layer

make_glyph <- function(fig, type, lname, lgroup, data, args, axisTypeRange, hover = NULL, legend = NULL, xname = NULL, yname = NULL) {

  ## give it a unique layer group name if not provided
  if(is.null(lgroup))
    lgroup <- genLayerName(names(fig$layers))
  lgroup <- as.character(lgroup)

  fig$layers[[lgroup]]$lgroup <- lgroup

  ## give it a unique layer name if not provided
  if(is.null(lname))
    lname <- genLayerName(names(fig$layers[[lgroup]]$glyphIds), prefix = "layer")
  lname <- as.character(lname)

  ## some figure elements need a single index to a layer name/group combination
  ## such as glyphDefer and glyphXRanges / glyphYRanges
  lgn <- paste(lgroup, lname, sep = "_")

  ## every layer has an associated glyphRenderer
  ## whose id is generated from the layer name and group
  glrId <- genId(fig, c("glyphRenderer", lgroup, lname))

  ## used to keep track of how many layers are in the group
  fig$layers[[lgroup]]$glyphIds[lname] <- list(glrId)

  ## make sure axis types match anything
  ## that has already been plotted
  validateAxisType(figType = fig$xAxisType,
    curType = axisTypeRange$xAxisType, which = "x")
  validateAxisType(figType = fig$yAxisType,
    curType = axisTypeRange$yAxisType, which = "y")

  fig$xAxisType <- axisTypeRange$xAxisType
  fig$yAxisType <- axisTypeRange$yAxisType

  ## make sure specified colors are bokeh-valid hex codes (if they are hex codes)
  ## only to ones that don't need to be mapped
  isMapped <- sapply(args, function(x) !is.null(attr(x, "nseName")))
  mappedArgs <- names(args)[isMapped]
  ind <- setdiff(names(args), mappedArgs)
  args[ind] <- validateColors(args[ind])

  ## deal with aesthetic inputs that are to be mapped
  ## e.g. fill_color might be mapped to a variable in the data
  ## in which case we need to track its domain
  ## and its range will be filled in from a theme
  ## when the figure is printed
  aesMaps <- getAesMaps(args, glrId)

  ## deal with manual legend
  if(!is.null(legend)) {
    legend <- as.character(legend)
    if(!is.null(aesMaps)) {
      message("Ignoring custom legend because an aesthetic is being mapped and therefore the legend is being taken care of automatically.")
    } else {
      if(!is.null(fig$commonLegend[[legend]])) {
        fig$commonLegend[[legend]]$args <- c(fig$commonLegend[[legend]]$args, list(args))
      } else {
        fig$commonLegend[[legend]] <- list(name = legend, args = list(args))
      }
    }
  }

  ## merge in aesthetic mappings (if any)
  fig$layers[[lgroup]]$maps <- mergeAesMaps(fig$layers[[lgroup]]$maps, aesMaps)

  ## save defer function (if any) and remove from data
  if(!is.null(data$defer)) {
    fig$glyphDefer[[lgn]] <- list(fn = data$defer)
    data$defer <- NULL
  }

  ## validate the spec args
  # validateOpts(opts, type)

  ## move all data scalars over to the spec
  dataLengths <- sapply(data, length)
  dataIsList <- sapply(data, is.list)
  dataNames <- names(data)
  scalarInd <- which(dataLengths == 1 & !dataIsList)
  for(ii in scalarInd)
    args[[dataNames[ii]]] <- data[[ii]]
  data[scalarInd] <- NULL

  ## move all non-scalar args over to data
  ## except for "line_dash"
  argLengths <- sapply(args, length)
  argNames <- names(args)
  longInd <- which(argLengths > 1 & argNames != "line_dash")
  for(ii in longInd) {
    data[[argNames[ii]]] <- args[[ii]]
  }
  args[longInd] <- NULL

  ## remove NAs in data at this point?

  ## spec needs to point to corresponding data
  dataNames <- names(data)
  for(nm in dataNames)
    args[[nm]] <- nm

  ## data must have something in it or it won't work
  if(length(data) == 0)
    data <- list(dummy = list(1))

  ## fix spec for "text" glyph
  if("text" %in% names(args)) {
     args$text <- list(field = "text")
  }

  if(!is.null(fig$glyphDefer[[lgn]])) {
    fig$glyphDefer[[lgn]]$spec <- args
    fig$glyphDefer[[lgn]]$data <- data
    fig$glyphDefer[[lgn]]$lgroup <- lgroup
    fig$glyphDefer[[lgn]]$lname <- lname
  }

  ## add hover info
  if(!is.null(hover)) {
    rendererRef <- list(
      type = "GlyphRenderer",
      id = glrId
    )

    fig <- fig %>% addHover(hover$dict, rendererRef)
    data <- cbind(data, data.frame(lapply(hover$data, as.character), stringsAsFactors = FALSE))
  }

  args$glyph <- type

  if(axisTypeRange$xAxisType == "datetime") {
    axisTypeRange$xRange <- toEpoch(axisTypeRange$xRange)
    if(!is.null(data$x))
      data$x <- toEpoch(data$x)
    if(!is.null(data$x0))
      data$x0 <- toEpoch(data$x0)
    if(!is.null(data$x1))
      data$x1 <- toEpoch(data$x1)
  }

  if(axisTypeRange$yAxisType == "datetime") {
    axisTypeRange$yRange <- toEpoch(axisTypeRange$yRange)
    if(!is.null(data$y))
      data$y <- toEpoch(data$y)
    if(!is.null(data$y0))
      data$y0 <- toEpoch(data$y0)
    if(!is.null(data$y1))
      data$y1 <- toEpoch(data$y1)
  }

  fig <- fig %>% addLayer(args, data, lname, lgroup)

  ## add x and y range for this glyph
  fig$glyphXRanges[[lgn]] <- axisTypeRange$xRange
  fig$glyphYRanges[[lgn]] <- axisTypeRange$yRange

  ## add x and y labels if missing
  if(is.null(fig$xlab) && length(xname) > 0)
    fig$xlab <- xname

  if(is.null(fig$ylab) && length(yname) > 0)
    fig$ylab <- yname

  fig
}

