

dat <- data.frame(x = runif(500), y = runif(500))

p <- figure(title = "select points to adjust mean line",
  tools = "lasso_select") %>%
  ly_points(x, y, data = dat, lname = "points") %>%
  ly_lines(x = c(0, 1), y = rep(mean(dat$y), 2), line_width = 6,
    color = "orange", alpha = 0.75, lname = "mean")

code <- "
  var inds = cb_obj.get('selected')['1d'].indices;
  var d = cb_obj.get('data');
  var ym = 0;

  if (inds.length == 0) { return; }

  for (i = 0; i < inds.length; i++) {
    ym += d['y'][inds[i]];
  }
  ym /= inds.length;

  mean_data.get('data').y = [ym, ym];

  cb_obj.trigger('change');
  mean_data.trigger('change');
"

p %>% tool_lasso_select(custom_callback(code, "mean"), "points")
