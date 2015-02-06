# bwplot(voice.part ~ height, data=singer, xlab="Height (inches)")
figure(ylab = "Height (inches)", width = 600) %>%
  ly_boxplot(voice.part, height, data = lattice::singer) %>%
  # ly_point(voice.part, height, data = lattice::singer)
