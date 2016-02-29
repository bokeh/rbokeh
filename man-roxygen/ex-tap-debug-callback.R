\donttest{
figure() %>%
  ly_points(1:10, lname = "points") %>%
  tool_tap(debug_callback("points"), "points")
}
