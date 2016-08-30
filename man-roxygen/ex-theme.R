\donttest{
# manually specify a ggplot-like grid and background
figure() %>%
  ly_points(1:10) %>%
  theme_plot(background_fill_color = "#E6E6E6",
    outline_line_color = "white") %>%
  theme_grid(c("x", "y"), grid_line_color = "white",
    minor_grid_line_color = "white",
    minor_grid_line_alpha = 0.4) %>%
  theme_axis(c("x", "y"), axis_line_color = "white",
    major_label_text_color = "#7F7F7F",
    major_tick_line_color = "#7F7F7F",
    minor_tick_line_alpha = 0, num_minor_ticks = 2)

# or use the built in ggplot theme (under development)
figure(data = iris, legend = "top_left", tools = NULL) %>%
  ly_points(Sepal.Length, Petal.Length, color = Species) %>%
  set_theme(bk_ggplot_theme)
}
\dontrun{
# or to set the theme for all future plots
options(bokeh_theme = bk_ggplot_theme)

figure() %>%
  ly_points(1:10)

figure() %>%
  ly_boxplot(1:10)
}
