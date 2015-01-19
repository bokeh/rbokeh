validColor <- function(dd) {
  all(dd %in% cssColors || (nchar(as.character(dd)) == 7 && grepl("^#", dd)))
}

validLine <- function(dd) {
  all(as.character(dd) %in% ltyNames)
}

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
  line_dash = function(dd)
    !validLine(dd)
)

needsMap <- function(dd, type, n) {
  if(length(dd) == n) {
    if(needsMapFns[[type]](dd))
      attr(dd, "needsMap") <- TRUE
  }
  dd
}
