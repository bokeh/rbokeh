\dontrun{
figure() %>%
  ly_points(rexp(1000), rexp(1000)) %>%
  x_axis(label = "x", log = TRUE) %>%
  y_axis(label = "y", log = TRUE)

figure() %>%
  ly_points(2^(1:10)) %>%
  y_axis(log = 2)

# disable scientific tick labels
figure() %>%
  ly_points(rnorm(10), rnorm(10) / 1000) %>%
  y_axis(use_scientific = FALSE)

# specify datetime tick labels
# the appropriate datetime units are automatically chosen
big_range <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by="days")
small_range <- seq(as.Date("2012-01-01"),as.Date("2012-02-01"), by="days")

figure() %>%
  ly_lines(big_range, rnorm(366)) %>%
  x_axis(label = "Date", format = list(months="%b-%Y", days="%d"))

figure() %>%
  ly_lines(small_range, rnorm(32)) %>%
  x_axis(label = "Date", format = list(months="%b-%Y", days="%d"))

# specify numeric tick labels
figure() %>%
  ly_points(rnorm(10), rnorm(10) * 10000) %>%
  y_axis(number_formatter = "numeral", format = "0,000")

figure() %>%
  ly_points(rnorm(10), rnorm(10) * 100) %>%
  y_axis(number_formatter = "printf", format = "%0.1f%%")
}
