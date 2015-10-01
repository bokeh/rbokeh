
ir <- iris
ir$glyphVal <- as.numeric(ir$Species)
ir$glyphCol <- c("red", "green", "blue")[ ir$glyphVal ]


# ly_points
load_all(); figure() %>% ly_points(Sepal.Length, data = ir) -> a; a
load_all(); figure() %>% ly_points(Sepal.Length, data = ir, color = glyphCol) -> a; a
load_all(); figure() %>% ly_points(Sepal.Length, data = ir, glyph = glyphVal, hover = Species) -> a; a
load_all(); figure() %>% ly_points(Sepal.Length, data = ir, color = Species) -> a; a

