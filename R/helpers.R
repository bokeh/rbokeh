

updateLineOpts <- function(fig, opts) {

  ## map to what bokeh expects
  opts$line_dash <- opts$type
  opts$type <- NULL

  opts$line_color <- opts$color
  opts$color <- NULL

  opts$line_width <- opts$width
  opts$width <- NULL

  opts$line_alpha <- opts$alpha
  opts$alpha <- NULL

  if(is.numeric(opts$line_dash)) {
    if(length(opts$line_dash) == 1) {
      opts$line_dash <- as.character(opts$line_dash)
    }
  }
  if(is.character(opts$line_dash)) {
    if(!opts$line_dash %in% names(ltyDict))
      stop("'line_dash' should be one of: ", paste(names(ltyDict), collapse = ", "), call. = FALSE)
    opts$line_dash <- ltyDict[[opts$line_dash]]
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

## should make this return something that will be evaluated at render time
getNextColor <- function(lgroupobj, which = "fill_color", type = "discrete") {
  curTheme <- bk_theme[[which]][[type]](10)
  nLayers <- length(lgroupobj$glyphIds) + 1
  nextColorIdx <- (nLayers - 1) %% length(curTheme) + 1
  curTheme[nextColorIdx]
}

checkArcDirection <- function(direction) {
  if(!direction %in% c("clock", "anticlock"))
    stop("'direction' must be 'clock' or 'anticlock'", call. = FALSE)
}

## take a set of layer groups
## and come up with the next increment of 'layer[int]'
genLayerName <- function(curNames, prefix = "group") {
  # curNames <- c("asdf", "layer1", "layer23", "qwert", "alayer7", "layer12b")
  if(length(curNames) == 0) {
    name <- paste0(prefix, "1")
  } else {
    namesWithPrefix <- curNames[grepl(paste0("^", prefix, "([0-9]+)$"), curNames)]
    if(length(namesWithPrefix) == 0) {
      name <- paste0(prefix, "1")
    } else {
      nn <- as.integer(gsub(prefix, "", namesWithPrefix))
      name <- paste(prefix, max(nn) + 1, sep = "")
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
  if(is.character(a) || is.factor(a)) {
    return("categorical")
  } else if(inherits(a, c("Date", "POSIXct"))) {
    return("datetime")
  } else {
    return("numeric")
  }
}

## determine the range of an axis for a glyph
getGlyphRange <- function(a, axisType = NULL, ...) {
  if(is.null(axisType))
    axisType <- getGlyphAxisType(a)
  ## ... can be size, etc. attributes
  if(axisType %in% c("numeric", "datetime")) {
    range(a, na.rm = TRUE)
  } else {
    # gsub removes suffixes like ":0.6"
    if(is.factor(a))
      a <- levels(a)
    unique(gsub("(.*):(-*[0-9]*\\.*)*([0-9]+)*$", "\\1", a))
  }
}

validateAxisType <- function(figType, curType, which) {
  if(length(figType) > 0 && length(curType) > 0) {
    # make this more informative...
    if(figType != curType)
      stop(which, " axis type (numerical / categorical) does not match that of other elements in this figure", call. = FALSE)
  }
}

## take a collection of glyph ranges (x or y axis)
## and find the global range across all glyphs
getAllGlyphRange <- function(ranges, padding_factor, axisType = "numeric", log = FALSE) {
  if(axisType == "numeric") {
    rangeMat <- do.call(rbind, ranges)
    hardRange <- c(min(rangeMat[,1], na.rm = TRUE),
      max(rangeMat[,2], na.rm = TRUE))
    ## if log, we need to make padding multiplicative
    if(log) {
      hardRange <- hardRange * c(padding_factor * 10, 2 - (padding_factor * 10))
    } else {
      hardRange <- hardRange + c(-1, 1) * padding_factor * diff(hardRange)
    }
    if(hardRange[1] == hardRange[2])
      hardRange <- hardRange + c(-0.5, 0.5)
    hardRange
  } else if(axisType == "datetime") {
    rangeMat <- do.call(rbind, ranges)
    hardRange <- c(min(rangeMat[,1], na.rm = TRUE),
      max(rangeMat[,2], na.rm = TRUE))
    hardRange <- hardRange + c(-1, 1) * padding_factor / 2 * diff(hardRange)
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
    } else {
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
    } else {
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

## take args color and alpha and translate them to f
resolveColorAlpha <- function(args, hasLine = TRUE, hasFill = TRUE, ly) {

  ## if no color at all is specified, choose from the theme
  if(is.null(args$color) && is.null(args$fill_color) && is.null(args$line_color))
    args$color <- getNextColor(ly)

  if(!is.null(args$color)) {
    if(!is.null(args$line_color)) {
      message("both color and line_color specified - honoring line_color")
    } else {
      args$line_color <- args$color
    }
    if(!is.null(args$fill_color)) {
      message("both color and fill_color specified - honoring fill_color")
    } else {
      args$fill_color <- args$color
    }
  }

  if(!is.null(args$alpha)) {
    if(!is.null(args$line_alpha)) {
      message("both alpha and line_alpha specified - honoring line_alpha")
    } else {
      args$line_alpha <- args$alpha
    }
    if(!is.null(args$fill_alpha)) {
      message("both alpha and fill_alpha specified - honoring fill_alpha")
    } else {
      args$fill_alpha <- args$alpha
    }
  }

  args$color <- NULL
  args$alpha <- NULL

  args
}

## make sure marker fill and line properties are correct for marker glyphs
## (for example, some, such as glyph = 1, must not have fill)
resolveGlyphProps <- function(glyph, args, lgroup) {
  if(glyph %in% names(markerDict)) {
    curGlyphProps <- markerDict[[as.character(glyph)]]
    args$glyph <- curGlyphProps$glyph
    if(curGlyphProps$fill) {
      if(is.null(args$fill_color)) {
        if(!is.null(args$line_color)) {
          args$fill_color <- args$line_color
        } else {
          args$fill_color <- lgroup
        }
      }
      if(curGlyphProps$line) {
        if(is.null(args$fill_alpha)) {
          args$fill_alpha <- 0.5
        } else {
          args$fill_alpha <- args$fill_alpha * 0.5
        }
      }
    } else {
      args$fill_color <- NA
      args$fill_alpha <- NA
    }

    if(curGlyphProps$line) {
      if(is.null(args$line_color))
        if(!is.null(args$fill_color)) {
          args$line_color <- args$fill_color
        } else {
          args$line_color <- lgroup
        }
    } else {
      args$line_color <- NULL
      args$line_width <- NULL
      args$line_alpha <- NULL
    }
  }
  args
}

getLgroup <- function(lgroup, fig) {
  if(is.null(lgroup))
    lgroup <- genLayerName(names(fig$layers))
  lgroup <- as.character(lgroup)
}

# get the "hover" argument and turn it into data and dict
# if a data frame was provided, the arg sould be a
# list of column names
# otherwise it should be a named list or data frame
getHover <- function(hn, data) {
  tmp <- try(eval(hn), silent = TRUE)
  if(is.data.frame(tmp)) {
    data <- tmp
    hn <- names(data)
  } else {
    if(deparse(hn)[1] == "NULL")
      return(NULL)
    if(is.null(data)) {
      if(!is.data.frame(hn)) {
        message("hover tool not added - 'hover' must be a data frame or list of variables present in the data frame supplied as the 'data' argument")
        return(NULL)
      } else {
        data <- hn
        hn <- names(data)
      }
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
  dp <- deparse(x)[1]
  if(dp %in% names(data))
    attr(res, "nseName") <- dp
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

## take output of map() and convert it to a data frame
map2df <- function(a) {
  dd <- data.frame(lon = a$x, lat = a$y,
    group = cumsum(is.na(a$x) & is.na(a$y)) + 1)
  dd[complete.cases(dd$lon, dd$lat), ]
}


#' @importFrom bitops bitShiftL bitOr
#' @export
to_uint32 <- function(x) {
  if(is.vector(x))
    x <- matrix(x, nrow = 1)
  bitOr(bitOr(bitOr(bitShiftL(x[,4], 24), bitShiftL(x[,3], 16)),
    bitShiftL(x[,2], 8)), bitShiftL(x[,1], 0))
}

toEpoch <- function(x) {
  if(inherits(x, "Date")) {
    return(as.numeric(x) * 86400000)
  } else if(inherits(x, "POSIXct")) {
    return(as.numeric(x) * 1000)
  }
  x
}

subset_with_attributes <- function(x, ...) {
  res <- x[...]
  attr.names <- names(attributes(x))
  attr.names <- attr.names[attr.names != 'names']
  attributes(res)[attr.names] <- attributes(x)[attr.names]
  res
}

getHexbinData <- function(x, y, xbins = 30, shape = 1, xbnds = range(x), ybnds = range(y),
  style = "lattice", minarea = 0.04, maxarea = 0.8, mincnt = 1, maxcnt = NULL, trans = NULL, inv = NULL) {

  if(is.null(xbnds))
    xbnds <- range(x, na.rm = TRUE)

  if(is.null(ybnds))
    ybnds <- range(y, na.rm = TRUE)

  dat <- hexbin(x, y, shape = shape, xbins = xbins, xbnds = xbnds, ybnds = ybnds)

  cnt <- dat@count
  xbins <- dat@xbins
  shape <- dat@shape
  tmp <- hcell2xy(dat)
  if(is.null(maxcnt))
    maxcnt <- max(dat@count)

  good <- mincnt <= cnt & cnt <= maxcnt

  xnew <- tmp$x[good]
  ynew <- tmp$y[good]
  cnt <- cnt[good]

  sx <- xbins / diff(dat@xbnds)
  sy <- (xbins * shape) / diff(dat@ybnds)

  if (is.null(trans)) {
     if (min(cnt, na.rm = TRUE) < 0) {
        pcnt <- cnt + min(cnt)
        rcnt <- {
           if (maxcnt == mincnt) rep.int(1, length(cnt)) else (pcnt - mincnt)/(maxcnt - mincnt)
        }
     } else rcnt <- {
        if (maxcnt == mincnt) rep.int(1, length(cnt)) else (cnt - mincnt)/(maxcnt - mincnt)
     }
  } else {
     rcnt <- (trans(cnt) - trans(mincnt))/(trans(maxcnt) - trans(mincnt))
     if (any(is.na(rcnt))) stop("bad count transformation")
  }

  if(style == "lattice") {
    area <- minarea + rcnt * (maxarea - minarea)
    area <- pmin(area, maxarea)
    radius <- sqrt(area)
  } else {
    radius <- rep(1, length(xnew))
  }

  inner <- 0.5
  outer <- (2 * inner) / sqrt(3)
  dx <- inner / sx
  dy <- outer / (2 * sy)
  # rad <- sqrt(dx^2 + dy^2)
  hexC <- hexcoords(dx, dy, sep = NULL)

  xs <- lapply(seq_along(xnew), function(i)
    hexC$x * radius[i] + xnew[i])
  ys <- lapply(seq_along(xnew), function(i)
    hexC$y * radius[i] + ynew[i])

  list(xs = xs, ys = ys, data = data.frame(x = xnew, y = ynew, rcnt = rcnt, cnt = cnt))
}



