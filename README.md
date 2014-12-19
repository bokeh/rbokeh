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

- Default choices for line, fill, and text attributes
- Best mechanism for mapping these attributes based on variables in the data
- R docs
- More layers for common statistial plots
- Tooltips / hover / click event handling specification from R
- Legends
- Investigate server side / streaming options

