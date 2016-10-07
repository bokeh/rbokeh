
## difficult to test what the plots actually look like
## currently just run a wide variety of things and make
## sure there are no errors in creating or preparing
## then we can use widget2png and inspect the phantomjs output
## if there is anything output, it means there has been an error

fname <- ifelse(Sys.info()["sysname"] == "Windows", "nul", "/dev/null") # nolint

p <- vector(length = 105, mode = "list")

test_that("examples", {

  p[[1]] <- figure() %>%
    ly_annular_wedge(Sepal.Length, Sepal.Width, data = iris,
      color = Species, inner_radius = 0.1, outer_radius = 0.15,
      alpha = 0.5, hover = Species)
  print_model_json(p[[1]], file = fname)

  p[[2]] <- figure() %>%
    ly_wedge(Sepal.Length, Sepal.Width, data = iris,
      color = Species, radius = 0.15, alpha = 0.5, hover = Species)
  print_model_json(p[[2]], file = fname)

  p[[3]] <- figure() %>%
    ly_arc(Sepal.Length, Sepal.Width, data = iris,
      color = Species, alpha = 0.5)
  print_model_json(p[[3]], file = fname)

  p[[4]] <- figure() %>%
    ly_annulus(Sepal.Length, Sepal.Width, data = iris,
      color = Species, hover = Species)
  print_model_json(p[[4]], file = fname)

  p[[5]] <- figure() %>%
    ly_points(rexp(1000), rexp(1000)) %>%
    x_axis(label = "x", log = TRUE) %>%
    y_axis(label = "y", log = TRUE)
  print_model_json(p[[5]], file = fname)

  # prepare data
  data(elements, package = "rbokeh")

  # create figure
  p[[7]] <- figure(title = "Periodic Table", tools = c("hover"),
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

  print_model_json(p[[7]], file = fname)

  data(flightfreq, package = "rbokeh")
  p[[8]] <- figure(width = 1000) %>%
    ly_points(date, Freq, data = flightfreq,
      hover = list(date, Freq, dow), size = 5) %>%
    ly_abline(v = as.Date("2001-09-11"))
  print_model_json(p[[8]], file = fname)

  p[[16]] <- figure(xlim = c(0, 1), ylim = c(0, 1), title = "Volcano") %>%
    ly_image(volcano) %>%
    ly_contour(volcano)
  print_model_json(p[[16]], file = fname)

  # check palette with ly_image
  # should reject a single color
  expect_error(
    pp <- figure(width = 700, height = 400) %>%
      ly_image(volcano, palette = "#FF00FF")
  )

  #  should accept no palette and use default
  p[[18]] <- figure(width = 700, height = 400) %>%
    ly_image(volcano)
  print_model_json(p[[18]], file = fname)

  #  should accept a Bokeh palette name
  p[[19]] <- figure(width = 700, height = 400) %>%
    ly_image(volcano, palette = "Greys9")
  print_model_json(p[[19]], file = fname)

  #  should accept a vector of colors
  p[[20]] <- figure(width = 700, height = 400) %>%
    ly_image(volcano, palette = blues9)
  print_model_json(p[[20]], file = fname)

  url <- c("  http://bokeh.pydata.org/en/latest/_static/images/logo.png",
    "http://developer.r-project.org/Logo/Rlogo-4.png")

  ss <- seq(0, 2 * pi, length = 13)[-1]
  ws <- runif(12, 2.5, 5) * rep(c(1, 0.8), 6)

  imgdat <- data.frame(
    x = sin(ss) * 10, y = cos(ss) * 10,
    w = ws, h = ws * rep(c(1, 0.76), 6),
    url = rep(url, 6)
  )

  p[[21]] <- figure(xlab = "x", ylab = "y") %>%
    ly_image_url(x, y, w = w, h = h, image_url = url, data = imgdat,
      anchor = "center") %>%
    ly_lines(sin(c(ss, ss[1])) * 10, cos(c(ss, ss[1])) * 10,
      width = 15, alpha = 0.1)
  print_model_json(p[[21]], file = fname)

  z <- lm(dist ~ speed, data = cars)
  p[[22]] <- figure() %>%
    ly_points(cars, hover = cars) %>%
    ly_lines(lowess(cars), legend = "lowess") %>%
    ly_abline(z, type = 2, legend = "lm", width = 2)
  print_model_json(p[[22]], file = fname)

  mtcars$model <- row.names(mtcars)
  p[[23]] <- figure() %>%
    ly_points(disp, mpg, data = mtcars, color = cyl,
      hover = "This <strong>@model</strong><br>has @hp horsepower!")
  print_model_json(p[[23]], file = fname)

  p[[24]] <- figure() %>%
    ly_points(Sepal.Length, Sepal.Width, data = iris,
      color = Species, glyph = Species,
      hover = list(Sepal.Length, Sepal.Width))
  print_model_json(p[[24]], file = fname)

  # get data from Duluth site in 'barley' data
  du <- subset(lattice::barley, site == "Duluth")

  # plot with default ranges
  p[[25]] <- figure(width = 600) %>%
    ly_points(yield, variety, color = year, data = du)
  print_model_json(p[[25]], file = fname)
  # y axis is alphabetical

  # manually set x and y axis (y in order of 1932 yield)
  p[[26]] <- p[[25]] %>%
    x_range(c(20, 40)) %>%
    y_range(du$variety[order(subset(du, year == 1932)$yield)])
  print_model_json(p[[26]], file = fname)

  # google map
  print_model_json(gmap(), file = fname)

  p[[27]] <- gmap(lat = 40.74, lng = -73.95, zoom = 11,
    width = 600, height = 600,
    map_style = gmap_style("blue_water"))
  print_model_json(p[[27]], file = fname)


  ## axis
  ##---------------------------------------------------------
  p[[28]] <- figure() %>%
    ly_points(rexp(1000), rexp(1000)) %>%
    x_axis(label = "x", log = TRUE) %>%
    y_axis(label = "y", log = TRUE)
  print_model_json(p[[28]], file = fname)

  p[[29]] <- figure() %>%
    ly_points(2 ^ (1:10)) %>%
    y_axis(log = 2)
  print_model_json(p[[29]], file = fname)

  # disable scientific tick labels
  p[[30]] <- figure() %>%
    ly_points(rnorm(10), rnorm(10) / 1000) %>%
    y_axis(use_scientific = FALSE)
  print_model_json(p[[30]], file = fname)

  # specify datetime tick labels
  # the appropriate datetime units are automatically chosen
  big_range <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "days")
  small_range <- seq(as.Date("2012-01-01"), as.Date("2012-02-01"), by = "days")

  p[[31]] <- figure() %>%
    ly_lines(big_range, rnorm(366)) %>%
    x_axis(label = "Date", format = list(months = "%b-%Y", days = "%d"))
  print_model_json(p[[31]], file = fname)

  p[[32]] <- figure() %>%
    ly_lines(small_range, rnorm(32)) %>%
    x_axis(label = "Date", format = list(months = "%b-%Y", days = "%d"))
  print_model_json(p[[32]], file = fname)

  # specify numeric tick labels
  p[[33]] <- figure() %>%
    ly_points(rnorm(10), rnorm(10) * 10000) %>%
    y_axis(number_formatter = "numeral", format = "0,000")
  print_model_json(p[[33]], file = fname)

  p[[34]] <- figure() %>%
    ly_points(rnorm(10), rnorm(10) * 100) %>%
    y_axis(number_formatter = "printf", format = "%0.1f%%")
  print_model_json(p[[34]], file = fname)

  ## bar
  ##---------------------------------------------------------

  # count of variety
  p[[35]] <- figure() %>%
    ly_bar(variety, data = lattice::barley) %>%
    theme_axis("x", major_label_orientation = 90)
  print_model_json(p[[35]], file = fname)

  # total yield per variety
  p[[36]] <- figure() %>%
    ly_bar(variety, yield, data = lattice::barley, hover = TRUE) %>%
    theme_axis("x", major_label_orientation = 90)
  print_model_json(p[[36]], file = fname)

  # swap axes and add hover
  p[[37]] <- figure() %>%
    ly_bar(yield, variety, data = lattice::barley, hover = TRUE)
  print_model_json(p[[37]], file = fname)

  # stack by year
  p[[38]] <- figure() %>%
    ly_bar(variety, yield, color = year, data = lattice::barley, hover = TRUE) %>%
    theme_axis("x", major_label_orientation = 90)
  print_model_json(p[[38]], file = fname)

  # proportional bars
  p[[39]] <- figure() %>%
    ly_bar(variety, yield, color = year,
      data = lattice::barley, position = "fill", width = 1) %>%
    theme_axis("x", major_label_orientation = 90) %>%
    set_palette(discrete_color = pal_color(c("red", "blue")))
  print_model_json(p[[39]], file = fname)

  # swap axes and use different palette
  p[[40]] <- figure() %>%
    ly_bar(yield, variety, color = year,
      data = lattice::barley, position = "fill") %>%
    set_palette(discrete_color = pal_color(c("red", "blue")))
  print_model_json(p[[40]], file = fname)

  # side by side bars
  p[[41]] <- figure() %>%
    ly_bar(variety, yield, color = year,
      data = lattice::barley, position = "dodge") %>%
    theme_axis("x", major_label_orientation = 90)
  print_model_json(p[[41]], file = fname)

  # use a different theme
  p[[42]] <- figure() %>%
    ly_bar(variety, yield, color = year,
      data = lattice::barley, position = "dodge") %>%
    theme_axis("x", major_label_orientation = 90)
  print_model_json(p[[42]], file = fname)

  ## boxplot
  ##---------------------------------------------------------

  p[[43]] <- figure(ylab = "Height (inches)", width = 600) %>%
    ly_boxplot(voice.part, height, data = lattice::singer)
  print_model_json(p[[43]], file = fname)

  # change orientation of x axis labels
  p[[44]] <- figure(ylab = "Height (inches)", width = 600) %>%
    ly_boxplot(voice.part, height, data = lattice::singer) %>%
    theme_axis("x", major_label_orientation = 90)
  print_model_json(p[[44]], file = fname)

  d <- data.frame(x = rnorm(1000), y = sample(letters[1:5], 1000, replace = TRUE))
  p[[103]] <- figure() %>%
    ly_boxplot(y, x, outlier_glyph = NA, data = d)
  print_model_json(p[[103]], file = fname)

  ## callbacks
  ##---------------------------------------------------------

  p[[45]] <- figure() %>%
    ly_points(1:10) %>%
    x_range(callback = console_callback()) %>%
    y_range(callback = console_callback())
  print_model_json(p[[45]], file = fname)

  # debug callback
  p[[46]] <- figure() %>%
    ly_points(1:10) %>%
    x_range(callback = debug_callback())
  print_model_json(p[[46]], file = fname)

  # character callback
  p[[47]] <- figure() %>%
    ly_points(1:10) %>%
    x_range(callback = "console.log('hi')")
  print_model_json(p[[47]], file = fname)

  # console callback (prints cb_data and cb_obj when hovered)
  p[[48]] <- figure() %>%
    ly_points(1:10, lname = "points") %>%
    tool_hover(console_callback(), "points") %>%
    tool_selection(console_callback(), "points")
  print_model_json(p[[48]], file = fname)

  # debug callback (launches debugger)
  p[[49]] <- figure() %>%
    ly_points(1:10, lname = "points") %>%
    tool_hover(debug_callback("points"), "points")
  print_model_json(p[[49]], file = fname)

  # just hover
  p[[50]] <- figure() %>%
    ly_points(1:10, hover = data.frame(a = 1:10))
  print_model_json(p[[50]], file = fname)

  # both hover and hover callback
  p[[51]] <- figure() %>%
    ly_points(1:10, hover = data.frame(a = 1:10), lname = "points") %>%
    tool_hover(console_callback(), "points")
  print_model_json(p[[51]], file = fname)

  # two different glyphs with different hover callbacks
  p[[52]] <- figure() %>%
    ly_points(1:10, lname = "points1") %>%
    ly_points(2:12, lname = "points2") %>%
    tool_hover("if(cb_data.index['1d'].indices.length > 0) console.log('1')", "points1") %>%
    tool_hover("if(cb_data.index['1d'].indices.length > 0) console.log('2')", "points2")
  print_model_json(p[[52]], file = fname)

  # tool_hover with references to lnames made available callback
  # is only triggered on l1 hover
  p[[53]] <- figure() %>%
    ly_points(1:10, lname = "l1") %>%
    ly_points(2:11, lname = "l2") %>%
    tool_hover(debug_callback(c("l1", "l2")), "l1")
  print_model_json(p[[53]], file = fname)

  dd <- data.frame(x = 1:10, link = paste0("http://google.com#q=", 1:10))

  # just url
  p[[54]] <- figure() %>%
    ly_points(x, url = "@link", data = dd, lname = "points")
  print_model_json(p[[54]], file = fname)

  # console callback (prints cb_obj and cb_data (empty) when point is clicked)
  p[[55]] <- figure() %>%
    ly_points(x, data = dd, lname = "points") %>%
    tool_tap(console_callback(), "points")
  print_model_json(p[[55]], file = fname)

  # debug callback
  p[[56]] <- figure() %>%
    ly_points(x, data = dd, lname = "points") %>%
    tool_tap(debug_callback("points"), "points")
  print_model_json(p[[56]], file = fname)

  # both console and url (note that you can toggle which one in toolbar)
  # but would be good to be able to do both
  p[[57]] <- figure() %>%
    ly_points(x, url = "@link", data = dd, lname = "points") %>%
    tool_tap(console_callback(), "points")
  print_model_json(p[[57]], file = fname)

  # two layers both with different tap callback
  # only first is honored (no matter what point is clicked)
  # would be good if could do both
  # https://github.com/bokeh/bokeh/issues/3804
  p[[58]] <- figure() %>%
    ly_points(1:10, lname = "l1") %>%
    ly_points(2:11, lname = "l2") %>%
    tool_tap("console.log('l1')", "l1") %>%
    tool_tap("console.log('l2')", "l2")
  print_model_json(p[[58]], file = fname)

  p[[59]] <- figure(tools = "lasso_select") %>%
    ly_points(1:10, lname = "points") %>%
    tool_selection(debug_callback(), "points")
  print_model_json(p[[59]], file = fname)

  p[[60]] <- figure(tools = "box_select") %>%
    ly_points(1:10, lname = "points") %>%
    tool_selection(debug_callback(), "points")
  print_model_json(p[[60]], file = fname)

  dat <- data.frame(x = rnorm(10), y = rnorm(10))
  p[[61]] <- figure() %>% ly_points(x = x, y = y, data = dat,
    hover = list(x, y), lname = "points") %>%
    tool_hover(shiny_callback("hover_info"), "points") %>%
    tool_tap(shiny_callback("tap_info"), "points") %>%
    tool_box_select(shiny_callback("selection_info"), "points") %>%
    x_range(callback = shiny_callback("x_range")) %>%
    y_range(callback = shiny_callback("y_range"))
  print_model_json(p[[61]], file = fname)

  ## grid
  ##---------------------------------------------------------

  idx <- split(1:150, iris$Species)
  figs <- lapply(idx, function(x) {
    figure(width = 300, height = 300) %>%
      ly_points(Sepal.Length, Sepal.Width, data = iris[x, ],
        hover = list(Sepal.Length, Sepal.Width))
  })

  # 1 row, 3 columns
  p[[63]] <- grid_plot(figs)
  print_model_json(p[[63]], file = fname)
  # specify xlim and ylim to be applied to all panels
  p[[64]] <- grid_plot(figs, xlim = c(4, 8), ylim = c(1.5, 4.5))
  print_model_json(p[[64]], file = fname)
  # unnamed list will remove labels
  p[[65]] <- grid_plot(unname(figs))
  print_model_json(p[[65]], file = fname)
  # 2 rows, 2 columns
  p[[66]] <- grid_plot(figs, nrow = 2)
  print_model_json(p[[66]], file = fname)
  # x and y axis with same (and linked) limits
  p[[67]] <- grid_plot(figs, same_axes = TRUE)
  print_model_json(p[[67]], file = fname)
  # x axis with same (and linked) limits
  p[[68]] <- grid_plot(figs, same_axes = c(TRUE, FALSE), nrow = 2)
  print_model_json(p[[68]], file = fname)
  # x axis with same (and linked) limits and custom xlim
  p[[69]] <- grid_plot(figs, same_axes = c(TRUE, FALSE), xlim = c(5, 7), nrow = 2)
  print_model_json(p[[69]], file = fname)
  # send lists instead of specifying nrow and ncol
  p[[70]] <- grid_plot(list(
    c(list(figs[[1]]), list(figs[[3]])),
    c(list(NULL), list(figs[[2]]))
  ))
  print_model_json(p[[70]], file = fname)
  # a null entry will be skipped in the grid
  figs2 <- figs
  figs2[1] <- list(NULL)
  p[[71]] <- grid_plot(figs2, nrow = 2)
  print_model_json(p[[71]], file = fname)

  # link data across plots in the grid (try box_select tool)
  # (data sources must be the same)
  tools <- c("pan", "wheel_zoom", "box_zoom", "box_select", "reset")
  p1 <- figure(tools = tools, width = 500, height = 500) %>%
    ly_points(Sepal.Length, Sepal.Width, data = iris, color = Species)
  p2 <- figure(tools = tools, width = 500, height = 500) %>%
    ly_points(Petal.Length, Petal.Width, data = iris, color = Species)
  p[[72]] <- grid_plot(list(p1, p2), same_axes = TRUE, link_data = TRUE)
  print_model_json(p[[72]], file = fname)

  ## themes
  ##---------------------------------------------------------

  p[[73]] <- figure() %>%
    ly_points(1:10) %>%
    theme_plot(background_fill_color = "#E6E6E6",
      outline_line_color = "white") %>%
    theme_grid(c("x", "y"), grid_line_color = "white",
      minor_grid_line_color = "white",
      minor_grid_line_alpha = 0.4) %>%
    theme_axis(c("x", "y"), axis_line_color = "white",
      major_label_text_color = "#7F7F7F",
      major_tick_line_color = "#7F7F7F",
      minor_tick_line_alpha = 0, num_minor_ticks = 2)
  print_model_json(p[[73]], file = fname)

  # or use the built in ggplot theme (under development)
  p[[74]] <- figure(data = iris, legend = "top_left", tools = NULL) %>%
    ly_points(Sepal.Length, Petal.Length, color = Species) %>%
    set_theme(bk_ggplot_theme())
  print_model_json(p[[74]], file = fname)

  p[[75]] <- figure(data = iris, legend = "top_left", tools = NULL) %>%
    ly_points(Sepal.Length, Petal.Length, color = Species) %>%
    set_theme(bk_default_theme())
  print_model_json(p[[75]], file = fname)

  ## other
  ##---------------------------------------------------------

  p[[76]] <- figure() %>% ly_hexbin(rnorm(10000), rnorm(10000))
  print_model_json(p[[76]], file = fname)

  library(maps)
  data(world.cities)
  caps <- subset(world.cities, capital == 1)
  caps$population <- prettyNum(caps$pop, big.mark = ",")
  p[[77]] <- figure(width = 800, height = 450, padding_factor = 0) %>%
    ly_map("world", col = "gray") %>%
    ly_points(long, lat, data = caps, size = 5,
      hover = c(name, country.etc, population))
  print_model_json(p[[77]], file = fname)

  p[[78]] <- figure(data = lattice::singer) %>%
    ly_points(catjitter(voice.part), jitter(height), color = "black") %>%
    ly_boxplot(voice.part, height, with_outliers = FALSE)
  print_model_json(p[[78]], file = fname)

  p[[81]] <- point_types()
  print_model_json(p[[81]], file = fname)

  p[[82]] <- figure(legend_location = "top_left") %>%
    ly_points(1:10, legend = "a") %>%
    theme_legend(border_line_width = 2)
  print_model_json(p[[82]], file = fname)

  p[[83]] <- figure() %>%
    ly_points(Sepal.Length, Sepal.Width, data = iris,
      color = Species, glyph = Species) %>%
    set_palette(discrete_color = pal_color(c("red", "blue", "green")))
  print_model_json(p[[83]], file = fname)

  pp <- figure() %>% ly_points(1:10)
  get_object_refs(pp)

  p[[85]] <- figure(width = 600, height = 400) %>%
    ly_hist(eruptions, data = faithful, breaks = 40, freq = FALSE) %>%
    ly_density(eruptions, data = faithful)
  print_model_json(p[[85]], file = fname)

  p[[86]] <- figure(legend_location = "top_left") %>%
    ly_quantile(Sepal.Length, group = Species, data = iris)
  print_model_json(p[[86]], file = fname)

  # wa_cancer <- droplevels(subset(latticeExtra::USCancerRates, state == "Washington"))
  # rownames(wa_cancer) <- NULL
  # wa_cancer <- wa_cancer[, c("LCL95.male", "UCL95.male", "rate.male", "county")]
  # wa_cancer <- wa_cancer[order(wa_cancer$rate.male, decreasing = TRUE),]
  # wa_cancer <- wa_cancer[1:10, ]
  wa_cancer <- data.frame(
    LCL95.male = c(237, 233.1, 266, 219.8, 227.5, 239.7, 245.4, 237.5, 208, 216.2),
    UCL95.male = c(466, 471.6, 316.8, 347.2, 303, 283.4, 263.3, 268.1, 300.3, 290.5),
    rate.male = c(332, 329.8, 290.5, 276.4, 263, 260.8, 254.2, 252.4, 250.6, 250.3),
    county = c("Columbia", "Wahkiakum", "Grays Harbor", "Pend Oreille", "Franklin",
      "Cowlitz", "Pierce", "Thurston", "Klickitat", "Pacific"),
    stringsAsFactors = FALSE)

  ## y axis sorted by male rate
  ylim <- levels(with(wa_cancer, reorder(county, rate.male)))

  p[[88]] <- figure(ylim = ylim, tools = NULL, data = wa_cancer) %>%
    ly_segments(LCL95.male, county, UCL95.male,
      county, color = NULL, width = 2) %>%
    ly_points(rate.male, county, glyph = 16)
  print_model_json(p[[88]], file = fname)

  chippy <- function(x) sin(cos(x) * exp(-x / 2))
  p[[89]] <- figure(width = 800) %>%
    ly_curve(chippy, -8, 7, n = 2001)
  print_model_json(p[[89]], file = fname)

  p[[90]] <- figure() %>%
    ly_ray(Sepal.Length, Sepal.Width,
      data = iris, length = runif(150),
      angle = runif(150, max = 2 * pi),
      color = Species)
  print_model_json(p[[90]], file = fname)

  # broken!!
  p[[91]] <- figure() %>%
    ly_bezier(
      x0 = Sepal.Length,
      x1 = Sepal.Length + runif(150),
      cx0 = Sepal.Length + runif(150),
      cx1 = Sepal.Length + runif(150),
      y0 = Sepal.Width,
      y1 = Sepal.Width + runif(150),
      cy0 = Sepal.Width + runif(150),
      cy1 = Sepal.Width + runif(150),
      color = Species,
      data = iris,
    )
  print_model_json(p[[91]], file = fname)

  p[[92]] <- figure() %>%
    ly_quadratic(
      x0 = Sepal.Length,
      x1 = Sepal.Length + runif(150),
      cx = Sepal.Length + runif(150),
      y0 = Sepal.Width,
      y1 = Sepal.Width + runif(150),
      cy = Sepal.Width + runif(150),
      color = Species,
      data = iris,
    )
  print_model_json(p[[92]], file = fname)

  xs <- list()
  ys <- list()
  for (i in 1:500) {
    count <- sample(1:10, 1)
    angles <- runif(count + 1, 0, 2 * pi)
    x_dists <- (1 / 2) ^ (0:count) * cos(angles)
    y_dists <- (1 / 2) ^ (0:count) * sin(angles)

    xs[[length(xs) + 1]] <- c(cumsum(x_dists))
    ys[[length(ys) + 1]] <- c(cumsum(y_dists))
  }

  p[[93]] <- figure() %>%
    ly_multi_line(xs = xs, ys = ys)
  print_model_json(p[[93]], file = fname)

  p[[94]] <- figure() %>%
    ly_points(1:26, letters) %>%
    ly_abline(h = "j") %>%
    ly_abline(v = 10)
  print_model_json(p[[94]], file = fname)

  p[[95]] <- figure() %>%
    ly_points(1:10) %>%
    ly_abline(v = 1:10) %>%
    ly_abline(h = 1:10)
  print_model_json(p[[95]], file = fname)

  p[[96]] <- figure() %>%
    ly_points(0:10) %>%
    ly_abline(0, seq(0, 1, by = 0.1))
  print_model_json(p[[96]], file = fname)

  p[[97]] <- figure() %>%
    ly_oval(Sepal.Length, Sepal.Width, data = iris, color = Species, alpha = 0.5)
  print_model_json(p[[97]], file = fname)

  # single patch doesn't allow line and fill color to come from data source
  # p[[98]] <- figure() %>%
  #   ly_patch(Sepal.Length, Sepal.Width, data = iris, color = Species, alpha = 0.5)
  # print_model_json(p[[98]], file = fname)
  # p[[99]] <- figure() %>%
  #   ly_patch(Sepal.Length, Sepal.Width, data = iris, color = "blue", alpha = 0.5)
  # print_model_json(p[[99]], file = fname)

  p[[100]] <- figure() %>%
    ly_points(disp, mpg, data = mtcars, color = cyl,
      hover = "cyl")
  print_model_json(p[[100]], file = fname)

  p[[101]] <- figure() %>%
    ly_boxplot(rnorm(500))
  print_model_json(p[[101]], file = fname)

  p[[102]] <- figure() %>%
    ly_boxplot(sample(1:20, 500, replace = TRUE), rnorm(500))
  print_model_json(p[[102]], file = fname)

  p[[103]] <- grid_plot(figs) %>%
    theme_title(text_color = "red") %>%
    theme_plot(background_fill_color = "#E6E6E6",
      outline_line_color = "white") %>%
    theme_grid(c("x", "y"), grid_line_color = "white",
      minor_grid_line_color = "white",
      minor_grid_line_alpha = 0.4) %>%
    theme_axis(c("x", "y"), axis_line_color = "white",
      major_label_text_color = "#7F7F7F",
      major_tick_line_color = "#7F7F7F",
      minor_tick_line_alpha = 0, num_minor_ticks = 2)
  print_model_json(p[[103]], file = fname)

  p[[104]] <- grid_plot(figs) %>%
    set_theme(bk_ggplot_theme)
  print_model_json(p[[104]], file = fname)

  pp <- figure() %>% ly_points(1:10)
  rbokeh2html(pp)

  # tf <- tempfile(fileext = ".png")
  # figure(tools = NULL) %>%
  #   ly_points(1:10) %>%
  #   widget2png(tf)
  # system2("open", tf)
})

# # sapply(p, length)
# for (i in seq_along(p)) {
#   if (!is.null(p[[i]])) {
#     res <- widget2png(p[[i]], file = tempfile(fileext = ".png"))
#     if (length(res) > 0)
#       message("JS errors for plot ", i)
#     # expect(length(res) == 0, message = paste("JS errors for plot", i))
#   }
# }
