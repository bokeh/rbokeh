makeGlyph <- function(fig, type, lname, lgroup, data, args, axisTypeRange, xname = NULL, yname = NULL) {

  ## see if any options won't be used and give a message
  checkOpts(args, type)

  ## make sure axis types match anything 
  ## that has already been plotted
  validateAxisType(figType = fig$xAxisType, 
    curType = axisTypeRange$xAxisType, which = "x")
  validateAxisType(figType = fig$yAxisType, 
    curType = axisTypeRange$yAxisType, which = "y")

  fig$xAxisType <- axisTypeRange$xAxisType
  fig$yAxisType <- axisTypeRange$yAxisType

  ## make sure specified colors are bokeh-valid hex codes (if they are hex codes)
  args <- validateColors(args)

  ## give it a unique layer name if not supplied
  if(is.null(lname))
    lname <- genGlyphName(names(fig$glyphLayers))

  ## save defer function (if any) and remove from data
  fig$glyphDefer[[lname]] <- data$defer
  data$defer <- NULL

  if(length(fig$glyphLayers) > 0)
    if(lname %in% names(fig$glyphLayers))
      message("A glyph layer already exists with name '", lname, "' - this is being replaced")

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

  ## add glyph type
  args$type <- type

  ## fix spec for "text" glyph
  if("text" %in% names(args)) {
     args$text <- list(field = "text")
  }

  if(!is.null(fig$glyphDefer[[lname]])) {
    fig$glyphDeferSpecs[[lname]] <- args
    fig$glyphDeferData[[lname]] <- data    
  }

  fig$glyphLayers[[lname]] <- list(id = genId(fig, c("glyphRenderer", lname)))

  fig <- fig %>% addLayer(args, data, lname)

  ## add x and y range for this glyph
  fig$glyphXRanges[[lname]] <- axisTypeRange$xRange
  fig$glyphYRanges[[lname]] <- axisTypeRange$yRange

  ## add x and y labels if missing
  if(is.null(fig$xlab) && length(xname) > 0)
    fig$xlab <- xname

  if(is.null(fig$ylab) && length(yname) > 0)
    fig$ylab <- yname

  fig
}

