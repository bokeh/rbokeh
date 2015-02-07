\donttest{
p <- figure(xlim = c(0, 1), ylim = c(0, 1), title = "Volcano") %>%
  ly_image(volcano) %>%
  ly_contour(volcano)
p
}
