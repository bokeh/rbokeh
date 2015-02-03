.onLoad <- function(libname, pkgname){
  registerMethods(list(
    c("knitr", "knit_print", "BokehFigure"),
    c("knitr", "knit_print", "BokehGridPlot")
  ))

  options(bokeh_theme = tableau_theme)
}
