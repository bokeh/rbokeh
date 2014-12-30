.onLoad <- function(libname, pkgname){
  registerMethods(list(
    c("knitr", "knit_print", "BokehFigure")
  ))

  options(bokeh_theme = tableau_theme)
}
