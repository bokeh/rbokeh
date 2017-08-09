# rbokeh

"rbokeh" is a native R plotting library that provides a flexible declarative interface for creating interactive web-based graphics, backed by the Bokeh visualization library.

This branch contains code for an ongoing refactoring effort to bring rbokeh models to parity with Bokeh and considerably clean up the internal code.

### Install

```r
install.packages("devtools")
devtools::install_github("hafen/rbokeh@refactor")
```

### Use

```r
library(rbokeh)

# minimal plot example
p <- figure(iris) %>%
  ly_points(x = Sepal.Width, y = Sepal.Length,
    color = Species, size = Petal.Length)

# view the plot
p

# print json of model
print_model_json(p)

# example of internal stuff
p <- rbokeh:::BoxAnnotation$new(line_alpha = 1, line_cap = "round")
p
p$to_json()
p$to_json(include_defaults = FALSE)
p$get_instance()
```

### Some Refactoring Details

R6 classes are generated for all models using [this code](https://github.com/hafen/rbokeh/blob/refactor/code-gen/process.R) and reading from the json output provided [here](https://gist.githubusercontent.com/bryevdv/de62a68029661a6e44169c17a34966f5/raw/997a5f1e7f92fea86b273a5c7c8bfaf246760d1e/gistfile1.txt). The generated R code is [here](https://github.com/hafen/rbokeh/blob/refactor/R/bk_model_autogen.R).

These classes are then used as building blocks underneath the hood for the high-level layer functions. A bare bones set of functions illustrating how this is done is [here](https://github.com/hafen/rbokeh/blob/refactor/R/barebones.R) and will be expanded / generalized across all models.
