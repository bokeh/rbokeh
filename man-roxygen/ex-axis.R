\dontrun{
# func tick formatter and number of ticks
figure() %>%
  ly_points(1:26, letters) %>%
  x_axis(
    ticker = ticker_num(desired_num_ticks = 20),
    tickformatter = tickformatter_func(code = "return tick + 'm';"))

# simple log axis
figure() %>%
  ly_points(1:10) %>%
  x_axis(log = TRUE)

# log axis with more detailed specification of parameters
figure() %>%
  ly_points(rexp(100, rate = 1 / 100), rnorm(100)) %>%
  x_axis(ticker = ticker_log(base = 2))

# interval ticker
figure() %>%
  ly_points(1:10) %>%
  x_axis(ticker = ticker_interval(0.5))

# fixed ticker
figure() %>%
  ly_points(1:10) %>%
  x_axis(ticker = ticker_fixed(c(1, 7, 9)))

# basic formatter
figure() %>%
  ly_points(c(1:10) * 1000000, 1:10) %>%
  x_axis(tickformatter = tickformatter_num(use_scientific = FALSE))

# numeral formatter
figure() %>%
  ly_points(1:10) %>%
  x_axis(tickformatter = tickformatter_numeral(format = "0o"))

# numeral formatter
figure() %>%
  ly_points((1:10) ^ 4, 1:10) %>%
  x_axis(tickformatter = tickformatter_numeral(format = "($0,0)", language = "it"))

# numeral formatter
figure() %>%
  ly_points((1:11) ^ 6, 1:11) %>%
  x_axis(tickformatter = tickformatter_numeral(format = "0.0b"))

# printf formatter
figure() %>%
  ly_points(1:10) %>%
  x_axis(tickformatter = tickformatter_printf(format = "%5.3f mu"))

# date formatter
figure(data = d, width = 1000) %>%
  ly_lines(date, co2_df) %>%
  x_axis(ticker = ticker_date(desired_num_ticks = 20))

# basic formatter works with dates too
figure(data = d, width = 1000) %>%
  ly_lines(date, co2_df, hover = d) %>%
  x_axis(ticker = ticker_num(desired_num_ticks = 20))

# func formatter (transform to how many years ago)
figure(data = d, width = 1000) %>%
  ly_lines(date, co2_df) %>%
  x_axis(tickformatter = tickformatter_func("
var cur = new Date();
var diff = (cur.getTime() - tick) / (1000 * 60 * 60 * 24 * 365);
return diff.toFixed(2) + ' years ago'"))

# date formatter
figure(data = d, width = 1000) %>%
  ly_lines(date, co2_df) %>%
  x_axis(tickformatter = tickformatter_date(year = "'%y"))

# date formatter
figure() %>%
  ly_points(Sys.time() - c(0:9) * 900, rnorm(10)) %>%
  x_axis(tickformatter = tickformatter_date(hourmin = "%r %Z"))

# axis options
figure() %>%
  ly_points(1:10) %>%
  x_axis(label = "x axis", axis = axis_spec(
    axis_label_text_color = "blue",
    axis_label_standoff = 30,
    axis_label_text_font_size = "20pt"
  )) %>%
  y_axis(label = "y axis", axis = axis_spec(
    major_label_text_color = "red"
  )) %>%
  rbokeh_prerender(keep_aux = TRUE)
  prepare_figure()

# axis options
figure() %>%
  ly_points(1:10) %>%
  x_axis(axis = axis_spec(bounds = c(3, 8)))

# axis options
figure() %>%
  ly_points(1:10) %>%
  x_axis(axis = axis_spec(
    major_tick_line_color = "firebrick",
    major_tick_line_width = 3,
    minor_tick_line_color = "orange",
    major_tick_out = 10,
    minor_tick_in = -3,
    minor_tick_out = 8
  )) %>%
  y_axis(axis = axis_spec(
    minor_tick_line_color = NA,
    major_tick_out = 10,
    minor_tick_in = -3,
    minor_tick_out = 8
  ))

# axis options
figure() %>%
  ly_points(1:10) %>%
  x_axis(axis = axis_spec(major_label_orientation = 45)) %>%
  y_axis(axis = axis_spec(major_label_orientation = "vertical"))

# grid options
figure() %>%
  ly_points(1:10) %>%
  x_axis(grid = grid_spec(grid_line_color = NA)) %>%
  y_axis(grid = grid_spec(
    band_fill_alpha = 0.1,
    band_fill_color = "navy"
  ))

# illustrating how "clear" works
# you can call x_axis() multiple times
# if you use clear = TRUE, it will clear out previous settings
# otherwise it will continue to add settings
p <- figure() %>%
  ly_points(1:10) %>%
  x_axis(
    ticker = ticker_num(desired_num_ticks = 10),
    grid = grid_spec(bounds = c(4, 7)))

p %>%
  x_axis(grid = grid_spec(grid_line_color = "purple", level = "overlay"))

p %>%
  x_axis(grid = grid_spec(grid_line_color = "purple", level = "overlay", clear = TRUE))

# multiple axes
figure() %>%
  ly_points(1:10) %>%
  y_axis(axis = axis_spec(bounds = c(3, 8)),
    grid = grid_spec(bounds = c(3, 8)), position = "right")
}
