

updateLineOpts <- function(fig, opts) {
    if(is.numeric(opts$line_dash)) {
    if(length(opts$line_dash) == 1) {
      opts$line_dash <- as.character(opts$line_dash)
    }
  }
  if(is.character(opts$line_dash)) {
    if(!opts$line_dash %in% names(ltyDict))
      stop("'line_join' should be one of: ", paste(names(ltyDict), collapse = ", "), call. = FALSE)
    opts$line_dash <- ltyDict[[opts$line_dash]]$line_dash
  }

  if(is.numeric(opts$line_cap))
    opts$line_cap <- ljoinDict[[as.character(opts$line_cap)]]

  if(is.null(opts$line_color))
    opts$line_color <- getNextColor(fig)

  opts
}

validateFig <- function(fig, fct) {
  if(!inherits(fig, "BokehFigure"))
    stop("Error in ", fct, ": first argument must be of type 'BokehFigure'", call. = FALSE)
}

## some things like rainbow(), etc., give hex with alpha
## Bokeh doesn't like hex alpha, so get rid of it
validateColors <- function(opts) {
  colFields <- c("line_color", "fill_color", "text_color")

  for(fld in colFields) {
    if(!is.null(opts[[fld]])) {
      ind <- which(grepl("^#", opts[[fld]]) & nchar(opts[[fld]]) == 9)
      if(length(ind) > 0) {
        message("note - ", fld, " has hex colors with with alpha information - removing alpha - please specify that through fill_alpha or line_alpha")
        opts[[fld]][ind] <- substr(opts[[fld]][ind], 1 , 7)
      }
    }
  }
  opts
}

getNextColor <- function(fig) {
  nLayers <- length(fig$layers)
  nextColorIdx <- (nLayers + 1) %% length(fig$theme$glyph)
  fig$theme$glyph[nextColorIdx]
}

checkArcDirection <- function(direction) {
  if(!direction %in% c("clock", "anticlock"))
    stop("'direction' must be 'clock' or 'anticlock'", call. = FALSE)
}

## take a set of layer groups
## and come up with the next increment of 'layer[int]'
genLayerGroup <- function(specNames) {
  # specNames <- c("asdf", "layer1", "layer23", "qwert", "alayer7", "layer12b")
  if(length(specNames) == 0) {
    name <- "layer1"
  } else {
    layerGroupNames <- specNames[grepl("^layer([0-9]+)$", specNames)]
    if(length(layerGroupNames) == 0) {
      name <- "layer1"
    } else {
      nn <- as.integer(gsub("layer", "", layerGroupNames))
      name <- paste("layer", max(nn) + 1, sep = "")
    }
  }
  name
}

## get the axis type and range for x and y axes
getGlyphAxisTypeRange <- function(x, y, assertX = NULL, assertY = NULL, glyph = "") {
  xAxisType <- getGlyphAxisType(x)
  yAxisType <- getGlyphAxisType(y)

  if(glyph != "")
    glyphText <- paste("'", glyph, "' ")

  if(!is.null(assertX)) {
    if(xAxisType != assertX)
      stop("Glyph ", glyph, " expects a ", assertX, " x axis", call. = FALSE)
  }
  if(!is.null(assertY)) {
    if(yAxisType != assertY)
      stop("Glyph ", glyph, "expects a ", assertY, " y axis", call. = FALSE)
  }

  list(
    xAxisType = xAxisType,
    yAxisType = yAxisType,
    xRange = getGlyphRange(x, xAxisType),
    yRange = getGlyphRange(y, yAxisType)
  )
}

## determine whether axis is "numeric" or "categorical"
getGlyphAxisType <- function(a) {
  # this will surely get more complex...
  ifelse(is.character(a), "categorical", "numeric")
}

## determine the range of an axis for a glyph
getGlyphRange <- function(a, axisType = NULL, ...) {
  if(is.null(axisType))
    axisType <- getGlyphAxisType(a)
  ## ... can be size, etc. attributes
  if(axisType == "numeric") {
    range(a, na.rm = TRUE)
  } else {
    # gsub removes suffixes like ":0.6"
    unique(gsub("(.*):(-*[0-9]*\\.*)*([0-9]+)*$", "\\1", a))
  }
}

validateAxisType <- function(figType, curType, which) {
  if(length(figType) > 0) {
    # make this more informative...
    if(figType != curType)
      stop(which, " axis type (numerical / categorical) does not match that of other elements in this figure", call. = FALSE)
  }
}

## take a collection of glyph ranges (x or y axis)
## and find the global range across all glyphs
getAllGlyphRange <- function(ranges, padding_factor, axisType = "numeric") {
  if(axisType == "numeric") {
    rangeMat <- do.call(rbind, ranges)
    hardRange <- c(min(rangeMat[,1], na.rm = TRUE),
      max(rangeMat[,2], na.rm = TRUE))
    hardRange <- hardRange + c(-1, 1) * padding_factor * diff(hardRange)
    if(hardRange[1] == hardRange[2])
      hardRange <- hardRange + c(-0.5, 0.5)
    hardRange
  } else {
    sort(unique(do.call(c, ranges)))
  }
}

## give a little warning if any options are specified that won't be used
checkOpts <- function(opts, type) {
  curGlyphProps <- glyphProps[[type]]

  validOpts <- c("xlab", "ylab")
  if(curGlyphProps$lp)
    validOpts <- c(validOpts, linePropNames)
  if(curGlyphProps$fp)
    validOpts <- c(validOpts, fillPropNames)
  if(curGlyphProps$tp)
    validOpts <- c(validOpts, textPropNames)

  if(length(opts) > 0) {
    # only get names of opts that are not NULL
    idx <- which(sapply(opts, function(x) !is.null(x)))
    if(length(idx) > 0) {
      notUsed <- setdiff(names(opts)[idx], validOpts)
      if(length(notUsed) > 0)
        message("note - arguments not used: ", paste(notUsed, collapse = ", "))
    }
  }
}

## take a hex color and reduce its saturation by a factor
## (used to get fill for pch=21:25)
reduceSaturation <- function(col, factor = 0.5) {
  col2 <- do.call(rgb2hsv, structure(as.list(col2rgb(col)[,1]), names = c("r", "g", "b")))
  col2['s', ] <- col2['s', ] * factor
  do.call(hsv, as.list(col2[,1]))
}

## handle different x, y input types
## this should be more "class"-y
## but this will suffice
getXYData <- function(x, y) {
  if(is.null(y)) {
    if(is.ts(x)) {
      return(list(x = as.vector(time(x)), y = as.vector(x)))
    } else if(is.list(x)) {
      return(list(x = x[[1]], y = x[[2]]))
    } else if(is.vector(x)) {
      return(list(x = seq_along(x), y = x))
    }
  }
  list(x = x, y = y)
}

getXYNames <- function(x, y, xname, yname, dots) {

  if(length(xname) > 1)
    xname <- NULL
  if(length(yname) > 1)
    yname <- NULL

  if(is.null(y)) {
    if(is.ts(x)) {
      res <- list(x = "time", y = xname)
    } else if(is.list(x)) {
      nms <- names(x)
      res <- list(x = nms[1], y = nms[2])
    } else if(is.vector(x)) {
      res <- list(x = "index", y = xname)
    }
  } else {
    res <- list(x = xname, y = yname)
  }

  # manual specification trumps
  if("xlab" %in% names(dots))
    res$x <- dots$xlab
  if("ylab" %in% names(dots))
    res$y <- dots$ylab

  res
}

# get the "hover" argument and turn it into data and dict
# if a data frame was provided, the arg sould be a
# list of column names
# otherwise it should be a named list or data frame
getHover <- function(hn, data) {
  if(deparse(hn) == "NULL")
    return(NULL)
  if(is.null(data)) {
    hover <- eval(hn)
    hn <- names(hover)
    data <- hover
  } else {
    hn <- deparse(hn)[1]
    hn <- gsub("c\\(|list\\(|\\)| +", "", hn)
    hn <- strsplit(hn, ",")[[1]]
    if(all(! hn %in% names(data))) {
      message("There were no columns: ", paste(hn, collapse = ", "), " in the data for the hover tool - hover not added")
      return(NULL)
    }
    data <- data[hn]
  }
  # hn <- setdiff(hn, c("x", "y", "size", "glyph", "color", "line_color", "fill_color"))
  hn2 <- gsub("\\.", "_", hn)
  names(data) <- hn2

  hdict <- lapply(seq_along(hn), function(ii) list(hn[ii], paste("@", hn2[ii], sep = "")))

  return(structure(list(
    data = data,
    dict = hdict
  ), class = "hoverSpec"))
}

v_eval <- function(x, data) {
  res <- eval(x, data)
  if(is.null(res))
    return(res)
  attr(res, "nseName") <- deparse(x)
  res
}

fixArgs <- function(args, n) {
  lns <- sapply(args, length)
  nms <- names(args)
  idx <- which(!lns %in% c(0, 1, n))

  if(length(idx) > 0)
    stop("Arguments do not have correct length: ", paste(nms[idx], " (", lns[idx],")", sep = "", collapse = ", "))

  # sclIdx <- which(lns == 1)
  # splitIdx <- which(lns == n)
  nullIdx <- which(lns == 0)
  if(length(nullIdx) > 0)
    args[nullIdx] <- NULL

  args
}

getAesMaps <- function(args, glrId) {

  nms <- names(args)
  ## get an index of arguments that need a map and have an nseName
  ## nseName is used in the legend so we know what variable created the map
  mappable <- sapply(seq_along(args), function(ii) {
    if(!is.null(attr(args[[ii]], "nseName"))) {
      if(!is.null(needsMapFns[[nms[ii]]])) {
        if(needsMapFns[[nms[ii]]](args[[ii]]))
          return(TRUE)
      }
    }
    return(FALSE)
  })

  if(length(which(mappable)) == 0)
    return(NULL)

  # build an aesMap object with an entry for each unique nseName
  # entry has the name, domain
  nseNames <- as.character(sapply(args[mappable], function(x) attr(x, "nseName")))
  uNseNames <- unique(nseNames)

  lapply(uNseNames, function(x) {
    ## args we need to compute domain for
    margs <- args[mappable[nseNames == x]]
    ## args we need to maintain for legend glyphs
    gargs <- lapply(args[!mappable], function(x) x[1])
    list(name = x,
      glrId = glrId,
      domain = getDomain(do.call(c, lapply(margs, getDomain))),
      glyphAttrs = gargs,
      range = structure(replicate(length(margs), list()), names = names(margs))
    )
  })
}

mergeAesMaps <- function(d1, d2) {
  if(is.null(d1))
    return(d2)
  if(is.null(d2))
    return(d1)

  if(is.character(d1)) {
    return(unique(c(d1, d2)))
  } else {
    return(range(c(d1, d2), na.rm = TRUE))
  }
}

getDomain <- function(x) {
  if(is.factor(x)) {
    return(levels(x))
  } else if(is.character(x)) {
    return(sort(unique(x)))
  } else {
    return(range(x, na.rm = TRUE))
  }
}

## take output of map() and convert it to a data frame
map2df <- function(a) {
  dd <- data.frame(lon = a$x, lat = a$y,
    group = cumsum(is.na(a$x) & is.na(a$y)) + 1)
  dd[complete.cases(dd$lon, dd$lat), ]
}

