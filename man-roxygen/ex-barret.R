
ir <- iris
ir$glyphVal <- as.numeric(ir$Species)
ir$glyphCol <- c("red", "green", "blue")[ ir$glyphVal ]

rescale <- function(x) {
  (x - min(x)) / diff(range(x))
}

bFig <- figure(width = 480*1.5,height = 520*1.5)

# test evaluation function
(test_b_eval = function(){
  matchVal = ir$Species
  attr(matchVal, "stringName") <- "Species"
  load_all(); require(testthat); require(lazyeval)
  b <- b_eval(ir); a <- function(x){ b(lazy(x)) };
  col = 5; expect_equivalent(a(col), 5)
  expect_equivalent(a("Species"), matchVal)
  z = "Species"; expect_equivalent(a(z), matchVal)
  z = I("Species"); expect_equivalent(a(z), I("Species"))
  expect_equivalent(a(Species), matchVal)
  expect_error(a(DOES_NOT_EXIST))

  TRUE
})()


# ly_points
load_all(); bFig %>% ly_points(ir$Sepal.Length) -> a; a
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


# ly_arc
load_all(); bFig %>% ly_arc(Sepal.Length, Sepal.Width, data = ir, end_angle = rescale(Petal.Length)*2*pi, color = Species, alpha = 0.5) -> a

# ly_wedge
load_all(); bFig %>% ly_wedge(Sepal.Length, Sepal.Width, data = ir) -> a; a
load_all(); bFig %>% bly_wedge(Sepal.Length, Sepal.Width, data = ir, end_angle = rescale(Petal.Length)*2*pi, color = Species, radius = 0.15, alpha = 0.5, hover = Species) -> a; a
load_all(); bFig %>% ly_wedge(Sepal.Length, Sepal.Width, data = ir, end_angle = rescale(Petal.Length)*2*pi, color = Species, radius = 0.15, alpha = 0.5, hover = Species) -> a; a




xx <- rnorm(10000)
yy <- rnorm(10000)

# ly_polygon
load_all(); bFig %>% ly_hexbin(xx, yy) -> a; a
