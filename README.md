# rbokeh

[![Build Status](https://travis-ci.org/hafen/rBokeh.svg?branch=master)](https://travis-ci.org/hafen/rBokeh)

R interface to Bokeh.

This is a work in progress, but covers quite a bit of ground.  Major changes in the design could happen.

### Install

Using the `devtools` package:

```
devtools::install_github("ramnathv/htmlwidgets")
devtools::install_github("hafen/rbokeh")
```

### Using

Please see [here](http://hafen.github.io/bokeh/rbokeh_examples.html) for several examples - documentation will come as things solidify.

### Status / Plans

Currently all glyphs are working except `image_rgba` and `image_url`.  Also mechanisms are in place to deal ensure consistency with numerical / categorical axes within a figure as well as keeping track of axis ranges.

In rough order of importance / imminence, things to cover next include:

- Affirming current design
- Default choices for line, fill, and text elements
- Type checking for these elements
- R docs
- All options for fine control of plot
- Higher-level abstractions?
- Wrappers for common statistial plots
- Tooltips / hover / click event handling specification from R
- Legends
- Headless javascript unit tests
- Investigate server side / streaming options

