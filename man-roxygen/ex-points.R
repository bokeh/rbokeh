\donttest{
figure() %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris,
    color = Species, glyph = Species,
    hover = list(Sepal.Length, Sepal.Width))

# custom hover
mtcars$model <- row.names(mtcars)
figure() %>%
  ly_points(disp, mpg, data = mtcars, color = cyl,
    hover = "This <strong>@model</strong><br>has @hp horsepower!")
}
