figure() %>%
  ly_points(sample(1:26), letters) %>%
  x_range(c(-5, 31), range_padding = 0)

figure() %>%
  ly_points(sample(1:26), letters) %>%
  x_range(flipped = TRUE)

figure() %>%
  ly_points(sample(1:26), letters) %>%
  y_range(rev(letters))

figure() %>%
  ly_points(sample(1:26), letters) %>%
  x_range(bounds = c(-5, 31))

figure() %>%
  ly_points(sample(1:26), letters) %>%
  x_range(min_interval = 10, max_interval = 40)

figure() %>%
  ly_points(sample(1:26), letters) %>%
  y_range(min_interval = 10, max_interval = 40)

figure() %>%
  ly_points(sample(1:26), letters) %>%
  x_range(c(-5, 31), follow = "start", follow_interval = 10)

figure() %>%
  ly_points(sample(1:26), letters) %>%
  x_range(callback = "console.log(this.attributes.start)")
