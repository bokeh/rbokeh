\donttest{
# hover over the blue points and make the orange points move
figure(title = "hover a blue point") %>%
  ly_points(1:10, lname = "blue", lgroup = "g1") %>%
  ly_points(2:12, lname = "orange", lgroup = "g1") %>%
  tool_hover(custom_callback(
    code = "debugger;if(cb_data.index['1d'].indices.length > 0)
    orange_data.get('data').x[cb_data.index['1d'].indices] += 0.1
    orange_data.trigger('change')", "orange"), "blue")
}
