Version 0.2

- add basic theme support
- add non-standard evaluation specification of parameters
- add aesthetic mapping
- add automatic generation of legends for aesthetic mappings
- add generic legend support
- hover tool support
- add datetime axis
- add log axis
- migrate to standard json model
- several "stats" layers - hist, density, quantile, boxplot, etc.
- axis labels
- tools control
- move to s3 methods + pipes for adding layers

Version 0.1
----------------------------------------------------------------------

- add `data` argument to several glyph functions
- add `elements` data set for plotting periodic table
- add checking for spefication of options that are not used
- fix handling of pch and associated options
- remove individual marker methods - all accessible through `points()`
- `hist` method to show example of higher-level stat plots using glyphs
- automatic calculation for continuous and categorical scales
- basic color theme for adding components to figures
- preliminary support for base R graphics:
  - `points()` including glyph mappings to R's `pch`
  - `lines()` including `line_dash` mappings to R's `lty`
- nearly all glyphs implemented (image\_rgba and image\_url remain)
- reference class implementation of a bokeh "figure"
