Version 0.2

- migrate to standard json model
- axis labels
- tools control

Version 0.1
----------------------------------------------------------------------

- add `data` argument to several glyph functions
- add `elements` data set for plotting periodic table
- add checking for spefication of options that are not used
- fix handling of pch and associated options
- remove individual marker methods - all accessible through `points()`
- `hist` method to show example of higher-level stat plots using glyphs
- Automatic calculation for continuous and categorical scales
- Basic color theme for adding components to figures
- Preliminary support for base R graphics:
  - `points()` including glyph mappings to R's `pch`
  - `lines()` including `line_dash` mappings to R's `lty`
- Nearly all glyphs implemented (image\_rgba and image\_url remain)
- Reference class implementation of a bokeh "figure"
