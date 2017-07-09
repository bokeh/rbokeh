.onLoad <- function(libname, pkgname){
  options(bokeh_theme = bk_default_theme())
  load(system.file("bk_prop_types.rda", package = "rbokeh"), envir = parent.env(environment()))
}
