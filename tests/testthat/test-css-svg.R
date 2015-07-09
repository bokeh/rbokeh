
# see here: https://github.com/bokeh/rbokeh/issues/40
test_that("svgs in css are base64 encoded", {
  ff <- file.path(system.file(package = "rbokeh"), "htmlwidgets/lib/bokehjs/bokeh.min.css")
  css <- suppressWarnings(readLines(ff))
  expect_false(any(grepl("<svg", css)))
})
