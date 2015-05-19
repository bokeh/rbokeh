library(shiny)
library(rbokeh)

df = data.frame(group = rep(c("blue", "green"), 5), x = rnorm(10), y = rnorm(10))

ui = shinyUI(fixedPage(
  fixedRow(
    rbokehOutput("rbka")
  ),
  fixedRow(
    textOutput("rbka_action")
  ),
  fixedRow(
    rbokehOutput("rbka_sub")
  )
))

server = function(input, output) {
  output$rbka <- renderRbokeh({
    figure(xlab = "x", ylab = "y", xlim = c(-2, 2), ylim = c(-2, 2)) %>%
      ly_points(x, y, data = df, color = group) #, url = "http://www.google.com?q=@group")
  })
  
  output$rbka_action <- renderPrint({
    if (!is.null(input$rbka_action))
      input$rbka_action
  })
  
  output$rbka_sub <- renderRbokeh({
    if (!is.null(input$rbka_action)) {
      figure(xlab = "x", ylab = "y", xlim = c(-2, 2), ylim = c(-2, 2)) %>%
        ly_points(x, y, data = df[df$group == input$rbka_action$fill_color, ],
                  color = group)
    }
  })
}

shinyApp(ui = ui, server = server)