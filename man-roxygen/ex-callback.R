# hover
figure() %>% ly_points(1:10)
figure() %>% ly_points(1:10, hover_callback = console_callback())
figure() %>% ly_points(1:10, hover = data.frame(a = 1:10))
figure() %>% ly_points(1:10, hover_callback = console_callback(), hover = data.frame(a = 1:10))


# tap
dd <- data.frame(x = 1:10, link = "http://google.com")
figure() %>% ly_points(x, url = "@link", data = dd)
figure() %>% ly_points(x, data = dd, tap_callback = console_callback())
figure() %>% ly_points(x, url = "@link", data = dd, tap_callback = console_callback())

# tool_hover with references to lnames made available in custom_callback
p <- figure() %>%
  ly_points(1:10, lname = "l1") %>%
  ly_points(10:1, lname = "l2") %>%
  tool_hover(custom_callback(code = "console.log(l1_data); console.log(l2_data)", args = c("l1", "l2")))

# won't work when splitting up glyphs...
# figure() %>% ly_points(1:10, glyph = rep(c("a", "b"), 5))


## shiny example
##---------------------------------------------------------

library("shiny")
library("rbokeh")
p <- print_model_json
dat <- data.frame(x = 1:3, y = 1:3, z = "http://people.mozilla.com")

custom_tap <- rbokeh:::custom_callback(code = '
console.log("tap data");
console.log(cb_obj.get("data"));
console.log("tap selected");
console.log(cb_obj.get("selected"))
', args = NULL)

ui <- fluidPage(
  rbokehOutput("rbokeh", width = 500, height = 540),
  textOutput("x_range_text"),
  textOutput("hover_text"),
  "selected data:",
  htmlOutput("tap_table")
)

server <- function(input, output, session) {
  output$rbokeh <- renderRbokeh({
    figure() %>% ly_points(x = x, y = y, data = dat,
      # url = "@z",
      hover = list(x,y),
      hover_callback = shiny_callback("hover_info"),
      tap_callback = shiny_callback("tap_info")) %>%
      # tap_callback = custom_tap) %>%
      x_range(callback = shiny_callback("x_range"))
  })

  output$x_range_text <- reactive({
    xrng <- input$x_range
    if(!is.null(xrng)) {
      paste0("factors: ", xrng$factors, ", start: ", xrng$start,
        ", end: ", xrng$end)
    } else {
      "waiting for axis event..."
    }
  })

  output$hover_text <- reactive({
    hi <- input$hover_info
    if(!is.null(hi)) {
      paste0(" ", hi$index[["1d"]]$indices, " d ",
        hi$geom$sx, " -> ", hi$geom$sy)
    } else {
      "waiting for hover event..."
    }
  })

  output$tap_table <- renderTable({
    ti <- input$tap_info
    if(!is.null(ti)) {
      data.frame(x = unlist(ti$x), y = unlist(ti$y))
    }
  })
}

shinyApp(ui, server)

