to_epoch <- function(x) {
  if (inherits(x, "Date")) {
    return(as.numeric(x) * 86400000)
  } else if (inherits(x, "POSIXt")) {
    return(as.numeric(x) * 1000)
  }
  x
}
