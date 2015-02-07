\donttest{
# bwplot(voice.part ~ height, data=singer, xlab="Height (inches)")
figure(ylab = "Height (inches)", width = 600) %>%
  ly_boxplot(voice.part, height, data = lattice::singer) %>%
  # ly_points(voice.part, height, data = lattice::singer)
}
