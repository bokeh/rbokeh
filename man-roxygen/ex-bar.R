\donttest{
figure() %>%
  ly_bar(variety, yield, data = lattice::barley) %>%
  theme_axis("x", major_label_orientation = 90)

figure() %>%
  ly_bar(variety, yield, color = year, data = lattice::barley) %>%
  theme_axis("x", major_label_orientation = 90)

figure() %>%
  ly_bar(variety, yield, color = year,
    data = lattice::barley, position = "fill", width = 1) %>%
  theme_axis("x", major_label_orientation = 90)

figure() %>%
  ly_bar(variety, yield, color = year,
    data = lattice::barley, position = "dodge") %>%
  theme_axis("x", major_label_orientation = 90)
}
