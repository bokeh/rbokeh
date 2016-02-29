
# see here: https://github.com/bokeh/rbokeh/issues/40
test_that("svgs in css are base64 encoded", {
  ff <- file.path(system.file(package = "rbokeh"), "htmlwidgets/lib/bokehjs/bokeh.min.css")
  css <- suppressWarnings(readLines(ff))
  expect_false(any(grepl("<svg", css)))
})

# to fix svg problem: find something like this in bokeh.min.css:
# .bk-logo.grey{filter:url("data:image/svg+xml;utf8,<svgxmlns=\'http://www.w3.org/2000/svg\'><filterid=\'grayscale\'><feColorMatrixtype=\'matrix\'values=\'0.33330.33330.3333000.33330.33330.3333000.33330.33330.33330000010\'/></filter></svg>#grayscale");filter:gray;-webkit-filter:grayscale(100%)}
# then
# base64enc::base64encode(charToRaw("<svgxmlns=\'http://www.w3.org/2000/svg\'><filterid=\'grayscale\'><feColorMatrixtype=\'matrix\'values=\'0.33330.33330.3333000.33330.33330.3333000.33330.33330.33330000010\'/></filter></svg>"))
# then replace it with:
# .bk-logo.grey{filter:url("data:image/svg+xml;base64,PHN2Z3htbG5zPSdodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2Zyc+PGZpbHRlcmlkPSdncmF5c2NhbGUnPjxmZUNvbG9yTWF0cml4dHlwZT0nbWF0cml4J3ZhbHVlcz0nMC4zMzMzMC4zMzMzMC4zMzMzMDAwLjMzMzMwLjMzMzMwLjMzMzMwMDAuMzMzMzAuMzMzMzAuMzMzMzAwMDAwMTAnLz48L2ZpbHRlcj48L3N2Zz4=");filter:gray;-webkit-filter:grayscale(100%)}

# also update css tooltip colors
# #1e4b6c to #aaa for background and to #888 for border
# #9ab9b1 to #fff
# #e2ddbd to #fff

