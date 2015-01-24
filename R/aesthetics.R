# __layerAesMap__
# -> var1
#   -> domain (vector of characters or numeric range)
#   -> legendGlyphs (list of aesLegendGlyph objects)
#   -> mapEntries (list of aesMapEntry objects)
# -> var2

# __aesLegendGlyph__
# -> name (name of the glyph this map applies to)
# -> mapArgs (vector of glyph attribute names that need to be mapped)
# -> args (arguments that do not need to be mapped - used to create legend glyphs)

# __aesMapEntry__
# -> id (id of glyphRenderer that needs its variables updated)
# -> mapArgs (vector of glyph attribute names that need to be mapped)

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

  if(length(mappable) > 0) {
    if(length(which(mappable)) == 0)
      return(NULL)

    # build an aesMap object with an entry for each unique nseName
    # entry has the name, domain
    nseNames <- as.character(sapply(args[mappable], function(x) attr(x, "nseName")))
    uNseNames <- unique(nseNames)

    layerAesMap <- structure(vector("list",
      length = length(uNseNames)), names = uNseNames)

    for(nm in uNseNames) {
      ## args we need to map
      margs <- args[mappable[nseNames == nm]]
      ## args we need to maintain for legend glyphs
      ## -- for now the assumption is these will be scalar
      ## -- (assume all args are either mapped or scalar)
      ## -- but should relax this by using idx when building "entries" below
      gargs <- lapply(args[!mappable], function(x) x[1])

      dmn <- getDomain(do.call(c, lapply(margs, getDomain)))
      layerAesMap[[nm]]$domain <- dmn

      glyphName <- as.character(args$glyph[1])
      if(!glyphName %in% names(glyphProps))
        glyphName <- "mappedGlyph"

      layerAesMap[[nm]]$legendGlyphs[[glyphName]] <- list(name = glyphName, args = gargs, mapArgs = names(margs))

      layerAesMap[[nm]]$mapEntries[[glrId]] <- list(id = glrId, mapArgs = names(margs), args = gargs)
    }
    return(layerAesMap)
  }
  NULL
}

# merge map2 into map1
mergeAesMaps <- function(map1, map2) {
  m1n <- names(map1)
  m2n <- names(map2)
  sameVar <- intersect(m1n, m2n)
  newVar <- setdiff(m2n, sameVar)
  if(length(newVar) > 0) {
    map1[newVar] <- map2[newVar]
  }
  if(length(sameVar) > 0) {
    for(nm in sameVar) {
      ## merge the domains
      map1[[nm]]$domain <- mergeAesDomains(map1[[nm]]$domain, map2[[nm]]$domain)
      ## merge legend entries
      # map1[[nm]]$legendEntries <- mergeAesLegendEntries(map1[[nm]]$legendEntries, map2[[nm]]$legendEntries)
      ## merge map entries
      map1[[nm]]$legendGlyphs <- mergeAesLegendGlyphs(map1[[nm]]$legendGlyphs, map2[[nm]]$legendGlyphs)
      id <- map2[[nm]]$mapEntries[[1]]$id
      map1[[nm]]$mapEntries[[id]] <- map2[[nm]]$mapEntries[[1]]
    }
  }
  map1
}

mergeAesLegendGlyphs <- function(gly1, gly2) {
  g1n <- names(gly1)
  g2n <- names(gly2)
  sameVar <- intersect(g1n, g2n)
  newVar <- setdiff(g2n, sameVar)
  if(length(newVar) > 0) {
    gly1[newVar] <- gly2[newVar]
  }
  if(length(sameVar) > 0) {
    if(any(sameVar == "mappedGlyph")) {
      if(!identical(gly1[[sameVar]], gly2[[sameVar]]))
        message("A layer added to an existing layer group has the same glyph mapped to a same attribute... it will be ignored.")
    } else {
      message("A layer added to an existing layer group has the same glyph mapped to a same attribute... it will be ignored.")
    }
  }
  gly1
}

mergeAesDomains <- function(d1, d2) {
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

getThemeValue <- function(domain, values, type) {
  isDiscrete <- ifelse(is.numeric(domain), FALSE, TRUE)
  subtype <- ifelse(isDiscrete, "discrete", "continuous")
  if(isDiscrete) {
    idx <- match(values, domain)
    vals <- bk_theme[[type]][[subtype]](length(domain))
    return(vals[idx])
  } else {
    ct <- cut(values, domain, include.lowest = TRUE)
    vals <- bk_theme[[type]][[subtype]](length(levels(ct)))
    return(vals[as.integer(ct)])
  }
}

validColor <- function(dd) {
  all(dd %in% cssColors || (nchar(as.character(dd)) == 7 && grepl("^#", dd)))
}

validLine <- function(dd) {
  all(as.character(dd) %in% ltyNames)
}

# any variable with nseName will be a candidate to be mapped
# but if it is a valid value, it won't be mapped
needsMapFns <- list(
  glyph = function(dd)
    !all(dd %in% markerPchTypes || dd %in% markerNames),
  color = function(dd)
    !validColor(dd),
  line_color = function(dd)
    !validColor(dd),
  fill_color = function(dd)
    !validColor(dd),
  lty = function(dd)
    !validLine(dd),
  size = function(dd)
    TRUE,
  line_dash = function(dd)
    !validLine(dd)
)

