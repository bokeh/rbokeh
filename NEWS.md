Version 0.2.3
----------------------------------------------------------------------

- fix an issue with `grid_plot` and `tool_events` not having unique id
- added support for axis tick formatting to `x_axis` and `y_axis`
- switched to `jsonlite` from `RJSONIO`

Version 0.2.2
----------------------------------------------------------------------

- update sizing policy to deal with `renderBokeh()`
- rbokeh plots are now htmlwidgets from the start
- fix URL tool

Version 0.2.1
----------------------------------------------------------------------

- fix dimension calculation so that resulting div is exactly the dimensions specified
- update default padding and text sizes
- fix sizing so that plots and grid plots on are exact dimensions as specified (resize canvas elements to accommodate for things drawn in the margins)
- fix bug in generating unique id for each plot
- allow toolbar to be completely hidden
- update documentation for `figure()`
- add sizing policy in rbokeh.js
- add default nonselection glyph properties for box/lasso select
- add basic theme support
- add non-standard evaluation specification of parameters
- add aesthetic mapping
- add automatic generation of legends for aesthetic mappings
- add generic legend support
- hover tool support
- tap to open URL support
- add datetime axis
- add log axis
- add ly_hexbin
- add grid_plot with linked pan/zoom and brushing
- several "stats" layers - hist, density, quantile, boxplot, etc.
- add image_url
- axis labels
- tools control
- migrate to standard json model
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
