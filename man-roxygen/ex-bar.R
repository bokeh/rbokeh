\donttest{
# count of variety
figure() %>%
  ly_bar(variety, data = lattice::barley) %>%
  theme_axis("x", major_label_orientation = 90)

# total yield per variety
figure() %>%
  ly_bar(variety, yield, data = lattice::barley) %>%
  theme_axis("x", major_label_orientation = 90)

# swap axes
figure() %>%
  ly_bar(yield, variety, data = lattice::barley)

# stack by year
figure() %>%
  ly_bar(variety, yield, color = year, data = lattice::barley) %>%
  theme_axis("x", major_label_orientation = 90)

# proportional bars
figure() %>%
  ly_bar(variety, yield, color = year,
    data = lattice::barley, position = "fill", width = 1) %>%
  theme_axis("x", major_label_orientation = 90)

# swap axes
figure() %>%
  ly_bar(yield, variety, color = year,
    data = lattice::barley, position = "fill", width = 1)

# side by side bars
figure() %>%
  ly_bar(variety, yield, color = year,
    data = lattice::barley, position = "dodge") %>%
  theme_axis("x", major_label_orientation = 90)
}
