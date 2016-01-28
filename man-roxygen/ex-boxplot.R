\donttest{
figure(ylab = "Height (inches)", width = 600) %>%
  ly_boxplot(voice.part, height, data = lattice::singer)

# change orientation of x axis labels
figure(ylab = "Height (inches)", width = 600) %>%
  ly_boxplot(voice.part, height, data = lattice::singer) %>%
  theme_axis("x", major_label_orientation = 90)
}
