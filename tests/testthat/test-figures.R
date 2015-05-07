
## difficult to test what the plots actually look like
## currently just run a wide variety of things and make
## sure there are no errors in creating or preparing

test_that("examples", {

  p <- figure() %>%
    ly_annular_wedge(Sepal.Length, Sepal.Width, data = iris,
      color = Species, inner_radius = 0.1, outer_radius = 0.15,
      alpha = 0.5, hover = Species)
  print_model_json(p, file = "/dev/null")

  p <- figure() %>%
    ly_wedge(Sepal.Length, Sepal.Width, data = iris,
      color = Species, radius = 0.15, alpha = 0.5, hover = Species)
  print_model_json(p, file = "/dev/null")

  p <- figure() %>%
    ly_arc(Sepal.Length, Sepal.Width, data = iris,
      color = Species, alpha = 0.5)
  print_model_json(p, file = "/dev/null")

  p <- figure() %>%
    ly_annulus(Sepal.Length, Sepal.Width, data = iris,
      color = Species, hover = Species)
  print_model_json(p, file = "/dev/null")

  p <- figure() %>%
    ly_points(rexp(1000), rexp(1000)) %>%
    x_axis(label = "x", log = TRUE) %>%
    y_axis(label = "y", log = TRUE)
  print_model_json(p, file = "/dev/null")

  p <- figure(ylab = "Height (inches)", width = 600) %>%
    ly_boxplot(voice.part, height, data = lattice::singer)
  print_model_json(p, file = "/dev/null")

  # prepare data
  data(elements, package = "rbokeh")
  elements <- subset(elements, !is.na(group))
  elements$group <- as.character(elements$group)
  elements$period <- as.character(elements$period)

  # add colors for groups
  metals <- c("alkali metal", "alkaline earth metal", "halogen",
    "metal", "metalloid", "noble gas", "nonmetal", "transition metal")
  colors <- c("#a6cee3", "#1f78b4", "#fdbf6f", "#b2df8a", "#33a02c",
    "#bbbb88", "#baa2a6", "#e08e79")
  elements$color <- colors[match(elements$metal, metals)]
  elements$type <- elements$metal

  # make coordinates for labels
  elements$symx <- paste(elements$group, ":0.1", sep = "")
  elements$numbery <- paste(elements$period, ":0.8", sep = "")
  elements$massy <- paste(elements$period, ":0.15", sep = "")
  elements$namey <- paste(elements$period, ":0.3", sep = "")

  # create figure
  p <- figure(title = "Periodic Table", tools = c("resize", "hover"),
    ylim = as.character(c(7:1)), xlim = as.character(1:18),
    xgrid = FALSE, ygrid = FALSE, xlab = "", ylab = "",
    height = 600, width = 1200) %>%

  # plot rectangles
  ly_crect(group, period, data = elements, 0.9, 0.9,
    fill_color = color, line_color = color, fill_alpha = 0.6,
    hover = list(name, atomic.number, type, atomic.mass,
      electronic.configuration)) %>%

  # add symbol text
  ly_text(symx, period, text = symbol, data = elements,
    font_style = "bold", font_size = "15pt",
    align = "left", baseline = "middle") %>%

  # add atomic number text
  ly_text(symx, numbery, text = atomic.number, data = elements,
    font_size = "9pt", align = "left", baseline = "middle") %>%

  # add name text
  ly_text(symx, namey, text = name, data = elements,
    font_size = "6pt", align = "left", baseline = "middle") %>%

  # add atomic mass text
  ly_text(symx, massy, text = atomic.mass, data = elements,
    font_size = "6pt", align = "left", baseline = "middle")

  print_model_json(p, file = "/dev/null")

  data(flightFreq, package = "rbokeh")
  p <- figure(width = 1000) %>%
    ly_points(date, Freq, data = flightfreq,
      hover = list(date, Freq, dow), size = 5) %>%
    ly_abline(v = as.Date("2001-09-11"))
  print_model_json(p, file = "/dev/null")

  tools <- c("pan", "wheel_zoom", "box_zoom", "box_select", "resize", "reset")
  p1 <- figure(tools = tools) %>%
    ly_points(Sepal.Length, Sepal.Width, data = iris,
      color = Species, hover = list(Sepal.Length, Sepal.Width))

  p2 <- figure(tools = tools) %>%
    ly_points(Petal.Length, Petal.Width, data = iris,
      color = Species, hover = list(Sepal.Length, Sepal.Width))

  # 1 row, 2 columns
  p <- grid_plot(list(p1, p2))
  print_model_json(p, file = "/dev/null")
  # x and y axis with same (and linked) limits
  p <- grid_plot(list(p1, p2), same_axes = TRUE)
  print_model_json(p, file = "/dev/null")
  # x axis has same (and linked) limits
  p <- grid_plot(list(p1, p2), same_axes = c(TRUE, FALSE))
  print_model_json(p, file = "/dev/null")
  # same axes and data is linked (try box_select tool)
  p <- grid_plot(list(p1, p2), same_axes = TRUE, link_data = TRUE)
  print_model_json(p, file = "/dev/null")
  # 1 column, 2 rows
  p <- grid_plot(list(p1, p2), ncol = 1)
  print_model_json(p, file = "/dev/null")
  # send lists instead of specifying nrow and ncol
  p <- grid_plot(list(c(list(p1), list(p2))))
  print_model_json(p, file = "/dev/null")
  p <- grid_plot(list(list(p1), list(p2)))
  print_model_json(p, file = "/dev/null")

  p <- figure(xlim = c(0, 1), ylim = c(0, 1), title = "Volcano") %>%
    ly_image(volcano) %>%
    ly_contour(volcano)
  print_model_json(p, file = "/dev/null")

  # check palette with ly_image
  #  should reject a single color
   expect_error(
     ly_image(
       figure( width = 700, height = 400 )
       ,volcano
       ,palette = "#FF00FF"
     )
   )
  #  should accept no palette and use default
  p <- ly_image(
    figure( width = 700, height = 400 )
    ,volcano
  )
  print_model_json(p, file = "/dev/null")
  #  should accept a Bokeh palette name
  p <- ly_image(
    figure( width = 700, height = 400 )
    ,volcano
    ,palette = "Greys9"
  )
  print_model_json(p, file = "/dev/null")
  #  should accept a vector of colors
  p <- ly_image(
    figure( width = 700, height = 400 )
    ,volcano
    ,palette = blues9
  )
  print_model_json(p, file = "/dev/null")



  url <- c("http://bokeh.pydata.org/en/latest/_static/bokeh-transparent.png",
    "http://developer.r-project.org/Logo/Rlogo-4.png")

  ss <- seq(0, 2*pi, length = 13)[-1]
  ws <- runif(12, 2.5, 5) * rep(c(1, 0.8), 6)

  imgdat <- data.frame(
    x = sin(ss) * 10, y = cos(ss) * 10,
    w = ws, h = ws * rep(c(1, 0.76), 6),
    url = rep(url, 6)
  )

  p <- figure(xlab = "x", ylab = "y") %>%
    ly_image_url(x, y, w = w, h = h, url = url, data = imgdat,
      anchor = "center") %>%
    ly_lines(sin(c(ss, ss[1])) * 10, cos(c(ss, ss[1])) * 10,
      width = 15, alpha = 0.1)
  print_model_json(p, file = "/dev/null")


  z <- lm(dist ~ speed, data = cars)
  p <- figure() %>%
    ly_points(cars, hover = cars) %>%
    ly_lines(lowess(cars), legend = "lowess") %>%
    ly_abline(z, type = 2, legend = "lm", width = 2)
  print_model_json(p, file = "/dev/null")

  p <- figure() %>%
    ly_points(Sepal.Length, Sepal.Width, data = iris,
      color = Species, glyph = Species,
      hover = list(Sepal.Length, Sepal.Width))
  print_model_json(p, file = "/dev/null")

  # get data from Duluth site in 'barley' data
  du <- subset(lattice::barley, site == "Duluth")

  # plot with default ranges
  p <- figure(width = 600) %>%
    ly_points(yield, variety, color = year, data = du)
  print_model_json(p, file = "/dev/null")
  # y axis is alphabetical

  # manually set x and y axis (y in order of 1932 yield)
  p <- p %>%
    x_range(c(20, 40)) %>%
    y_range(du$variety[order(subset(du, year == 1932)$yield)])
  print_model_json(p, file = "/dev/null")

  # google map
  print_model_json(gmap(), file = "/dev/null")
  
  # axis tick formatters
  p <- figure() %>%
    ly_points(rnorm(10), rnorm(10) / 1000) %>%
    y_axis(use_scientific = FALSE)
  print_model_json(p, file = "/dev/null")
  
  p <- figure() %>%
    ly_lines(seq(as.Date("2012-01-01"),as.Date("2012-12-31"), by="days"), 
             rnorm(366)) %>%
    x_axis(label = "Date", formats = list(months="%b-%Y", days="%d"))
  print_model_json(p, file = "/dev/null")
  p <- figure() %>%
    ly_lines(seq(as.Date("2012-01-01"),as.Date("2012-02-01"), by="days"), 
             rnorm(32)) %>%
    x_axis(label = "Date", formats = list(months="%b-%Y", days="%d"))
  print_model_json(p, file = "/dev/null")
  
  p <- figure() %>%
    ly_points(rnorm(10), rnorm(10) * 10000) %>%
    y_axis(number_formatter = "numeral", format = "0,000")
  print_model_json(p, file = "/dev/null")
  
  p <- figure() %>%
    ly_points(rnorm(10), rnorm(10) * 100) %>%
    y_axis(number_formatter = "printf", format = "%0.1f%%")
  print_model_json(p, file = "/dev/null")
  
})
