library(rbokeh)

D <- data.frame(
  x = c(2, 3, 5, 6, 8, 7),
  y = c(6, 4, 3, 8, 7, 5))

links <- list(
  "0" = c(1, 2),
  "1" = c(0, 3, 4),
  "2" = c(0, 5),
  "3" = c(1, 4),
  "4" = c(1, 3),
  "5" = c(2, 3, 4)
)

src <- data.frame(
  x0 = numeric(0), y0 = numeric(0),
  x1 = numeric(0), y1 = numeric(0))

p <- figure(width = 400, height = 500, toolbar_location = NULL,
  title = "Hover Over Points", tools = NULL) %>%
  ly_segments(x0 = x0, y0 = y0, x1 = x1, y1 = y1, color = "olive",
    alpha = 0.6, line_width = 3, data = src, lname = "segment") %>%
  ly_points(x = x, y = y, data = D, color = "olive", size = 30,
    alpha = 0.4, lname = "circle")

callback <- custom_callback(code = sprintf("
var links=%s;
var data = {'x0': [], 'y0': [], 'x1': [], 'y1': []};
var cdata = circle_data.get('data');
var indices = cb_data.index['1d'].indices;
for (i=0; i < indices.length; i++) {
  ind0 = indices[i]
  for (j=0; j < links[ind0].length; j++) {
    ind1 = links[ind0][j];
    data['x0'].push(cdata.x[ind0]);
    data['y0'].push(cdata.y[ind0]);
    data['x1'].push(cdata.x[ind1]);
    data['y1'].push(cdata.y[ind1]);
  }
}
segment_data.set('data', data);
", jsonlite::toJSON(links)), lnames = c("circle", "segment"))

p <- tool_hover(p, callback = callback, c("circle"))
p
