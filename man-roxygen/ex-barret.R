
ir <- iris
ir$glyphVal <- as.numeric(ir$Species)
ir$glyphCol <- c("red", "green", "blue")[ ir$glyphVal ]

bFig <- figure(width = 480*1.5,height = 520*1.5)

# ly_points
load_all(); bFig %>% ly_points(Sepal.Length, data = ir) -> a; a
load_all(); bFig %>% ly_points(Sepal.Length, data = ir, color = glyphCol) -> a; a
load_all(); bFig %>% ly_points(Sepal.Length, data = ir, glyph = glyphVal, hover = Species) -> a; a
load_all(); bFig %>% ly_points(Sepal.Length, data = ir, color = Species) -> a; a

# ly_annular_wedge
load_all(); bFig %>% ly_annular_wedge(Sepal.Length, Sepal.Width, data = iris, end_angle = rescale(Petal.Length)*2*pi, inner_radius = 0.1, outer_radius = 0.15, alpha = 0.5, hover = Species, color = Species) -> a; a

# ly_annulus
# Hover does not work.  It did not work beforehand
load_all(); bFig %>% ly_annulus(Sepal.Length, Sepal.Width, data = ir, inner_radius = 0.1, outer_radius = 0.15, alpha = 0.5, hover = Species) -> a; a
load_all(); bFig %>% ly_annulus(Sepal.Length, Sepal.Width, data = ir, inner_radius = 0.1, outer_radius = 0.15, alpha = 0.5, hover = Species, color = Species) -> a; a
