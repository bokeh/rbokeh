

dat <- data.frame(x = runif(500), y = runif(500))

p <- figure(tools = "lasso_select") %>%
  ly_points(x, y, data = dat, lname = "points") %>%
  ly_abline(h = mean(dat$y), line_width = 6,
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

  mean_data.get('data')['y0'] = [ym];
  mean_data.get('data')['y1'] = [ym];

  cb_obj.trigger('change');
  mean_data.trigger('change');
"

p %>% selection_callback(custom_callback(code, "mean"), "points")
