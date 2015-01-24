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

Currently all glyphs are working except `image_rgba` and `image_url`.  Also mechanisms are in place to ensure consistency with numerical / categorical axes within a figure as well as keeping track of axis ranges.

In rough order of importance / imminence, things to cover next include:

- Add datetime axis
- Mapping aesthetics for continuous variables
- Fig image and friends
- Mapping aesthetics for other layer types
- Linking / brushing example
- R docs
- More layers for common statistial plots
- Investigate server side / streaming options

