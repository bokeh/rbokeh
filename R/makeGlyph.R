makeGlyph <- function(fig, type, lname, lgroup, data, args, axisTypeRange, hover = NULL, xname = NULL, yname = NULL) {

  ## give it a unique layer group if not supplied
  if(is.null(lgroup))
    lgroup <- genLayerGroup(names(fig$layers))
  lgroup <- as.character(lgroup)

  fig$layers[[lgroup]]$lgroup <- lgroup

  if(is.null(lname))
    lname <- as.character(length(fig$layers[[lgroup]]$glyphIds) + 1)
  lname <- as.character(lname)

  lgn <- paste(lgroup, lname, sep = "_")

  glrId <- genId(fig, c("glyphRenderer", lgroup, lname))

  aesMaps <- getAesMaps(args, glrId)

  ## see if any options won't be used and give a message
  ## but glyph can be a mapping (not a real type)
  ## so only check if it's not to be mapped
  if(is.null(aesMaps$glyph)) {
    checkOpts(args, type)
  }

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
  mappedArgs <- unique(do.call(c, lapply(aesMaps, function(x) names(x$range))))
  ind <- setdiff(names(args), mappedArgs)
  args[ind] <- validateColors(args[ind])

  fig$layers[[lgroup]]$glyphIds[lname] <- list(glrId)

  ## merge in aesthetic mappings (if any)
  if(is.null(fig$layers[[lgroup]]$maps)) {
    fig$layers[[lgroup]]$maps <- aesMaps
  } else {
    for(nn in names(aesMaps)) {
      fig$layers[[lgroup]]$maps[[nn]] <- mergeAesMaps(fig$layers[[lgroup]]$maps[[nn]], aesMaps[[nn]])
    }
  }

  # if(!is.null(fig$layers[[lgroup]][[lname]]))
  #   message("A glyph layer already exists with name '", lname, "' and group '", lgroup, "' - this is being replaced")

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
    data <- cbind(data, hover$data)
  }

  args$glyph <- type

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

