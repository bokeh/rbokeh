
# see here: https://github.com/bokeh/rbokeh/issues/40
test_that("svgs in css are base64 encoded", {
  ff <- file.path(system.file(package = "rbokeh"), "htmlwidgets/lib/bokehjs/bokeh.min.css")
  css <- suppressWarnings(readLines(ff))
  expect_false(any(grepl("<svg", css)))
})

# to fix svg problem: find something like this in bokeh.min.css:
# .bk-logo.grey{filter:url("data:image/svg+xml;utf8,<svgxmlns=\'http://www.w3.org/2000/svg\'><filterid=\'grayscale\'><feColorMatrixtype=\'matrix\'values=\'0.33330.33330.3333000.33330.33330.3333000.33330.33330.33330000010\'/></filter></svg>#grayscale");filter:gray;-webkit-filter:grayscale(100%)}

# .bk-logo.grey{filter:url("data:image/svg+xml;utf8,<svg xmlns=\'http://www.w3.org/2000/svg\'><filter id=\'grayscale\'><feColorMatrix type=\'matrix\' values=\'0.3333 0.3333 0.3333 0 0 0.3333 0.3333 0.3333 0 0 0.3333 0.3333 0.3333 0 0 0 0 0 1 0\'/></filter></svg>#grayscale");filter:gray;-webkit-filter:grayscale(100%)}

# then
# base64enc::base64encode(charToRaw("<svg xmlns=\'http://www.w3.org/2000/svg\'><filter id=\'grayscale\'><feColorMatrix type=\'matrix\' values=\'0.3333 0.3333 0.3333 0 0 0.3333 0.3333 0.3333 0 0 0.3333 0.3333 0.3333 0 0 0 0 0 1 0\'/></filter></svg>"))
# then replace it with:
# .bk-logo.grey{filter:url("data:image/svg+xml;base64,PHN2ZyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnPjxmaWx0ZXIgaWQ9J2dyYXlzY2FsZSc+PGZlQ29sb3JNYXRyaXggdHlwZT0nbWF0cml4JyB2YWx1ZXM9JzAuMzMzMyAwLjMzMzMgMC4zMzMzIDAgMCAwLjMzMzMgMC4zMzMzIDAuMzMzMyAwIDAgMC4zMzMzIDAuMzMzMyAwLjMzMzMgMCAwIDAgMCAwIDEgMCcvPjwvZmlsdGVyPjwvc3ZnPg==");filter:gray;-webkit-filter:grayscale(100%)}

# also update css tooltip colors
# #1e4b6c to #aaa for background and to #888 for border
# #9ab9b1 to #fff
# #e2ddbd to #fff
