
ir <- iris
ir$glyphVal <- as.numeric(ir$Species)
ir$glyphCol <- c("red", "green", "blue")[ ir$glyphVal ]


# ly_points
load_all(); figure() %>% ly_points(Sepal.Length, data = ir) -> a; a
load_all(); figure() %>% ly_points(Sepal.Length, data = ir, color = glyphCol) -> a; a
load_all(); figure() %>% ly_points(Sepal.Length, data = ir, glyph = glyphVal, hover = Species) -> a; a
load_all(); figure() %>% ly_points(Sepal.Length, data = ir, color = Species) -> a; a

# ly_annular_wedge
load_all(); figure() %>% ly_annular_wedge(Sepal.Length, Sepal.Width, data = iris, end_angle = rescale(Petal.Length)*2*pi, inner_radius = 0.1, outer_radius = 0.15, alpha = 0.5, hover = Species, color = Species) -> a; a

