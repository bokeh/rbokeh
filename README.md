# rbokeh

"rbokeh" is a native R plotting library that provides a flexible declarative interface for creating interactive web-based graphics, backed by the Bokeh visualization library.

This branch contains code for an ongoing refactoring effort to bring rbokeh models to parity with Bokeh and considerably clean up the internal code.

### Install

```r
install.packages("devtools")
devtools::install_github("bokeh/rbokeh@refactor")
```

### Use

```r
# minimal plot example
p <- figure() %>%
  ly_line(1:100, rnorm(100))
p

# print json of model
print_model_json(p)
```
