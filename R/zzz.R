.onLoad <- function(libname, pkgname){
  load(system.file("bk_prop_types.rda", package = "rbokeh"), envir = parent.env(environment()))
}
