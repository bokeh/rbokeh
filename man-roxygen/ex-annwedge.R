\donttest{
rescale <- function(x)
  (x - min(x)) / diff(range(x))

figure() %>%
  ly_annular_wedge(Sepal.Length, Sepal.Width, data = iris,
    end_angle = rescale(Petal.Length)*2*pi, color = Species,
    inner_radius = 0.1, outer_radius = 0.15, alpha = 0.5,
    hover = Species)

figure() %>%
  ly_wedge(Sepal.Length, Sepal.Width, data = iris,
    end_angle = rescale(Petal.Length)*2*pi, color = Species,
    radius = 0.15, alpha = 0.5,
    hover = Species)

figure() %>%
  ly_arc(Sepal.Length, Sepal.Width, data = iris,
    end_angle = rescale(Petal.Length)*2*pi, color = Species,
    alpha = 0.5)

figure() %>%
  ly_annulus(Sepal.Length, Sepal.Width, data = iris,
    color = Species, hover = Species,
    outer_radius = rescale(Petal.Length) * 0.3,
    inner_radius = rescale(Petal.Length) * 0.1)
}
