# Plot a `tidyvpcobj`

Use ggplot2 graphics to plot and customize the appearance of VPC.

## Usage

``` r
# S3 method for class 'tidyvpcobj'
plot(
  x,
  facet = FALSE,
  show.points = TRUE,
  show.boundaries = TRUE,
  show.stats = !is.null(x$stats),
  show.binning = isFALSE(show.stats),
  xlab = NULL,
  ylab = NULL,
  color = c("red", "blue", "red"),
  linetype = c("dotted", "solid", "dashed"),
  point.alpha = 0.4,
  point.size = 1,
  point.shape = "circle-fill",
  point.stroke = 1,
  ribbon.alpha = 0.1,
  legend.position = "top",
  facet.scales = "free",
  custom.theme = NULL,
  censoring.type = c("none", "both", "blq", "alq"),
  censoring.output = c("grid", "list"),
  censoring.color = c(observed = "black", simulated = "red"),
  censoring.fill = "red",
  ...
)
```

## Arguments

- x:

  A `tidyvpcobj`.

- facet:

  Set to `TRUE` to facet plot by quantile (continuous VPC) or category
  (categorical VPC).

- show.points:

  Should the observed data points be plotted?

- show.boundaries:

  Should the bin boundary be displayed?

- show.stats:

  Should the VPC stats be displayed?

- show.binning:

  Should the binning be displayed by coloring the observed data points
  by bin?

- xlab:

  A character label for the x-axis.

- ylab:

  A character label for the y-axis.

- color:

  A character vector of colors for the percentiles, from low to high.

- linetype:

  A character vector of line type for the percentiles, from low to high.

- point.alpha:

  Numeric value specifying transparency of points.

- point.size:

  Numeric value specifying size of point.

- point.shape:

  Character one of
  `"circle", "circle-fill", "diamond", "diamond-fill", "square", "square-fill", "triangle-fill" , "triangle")`.
  Defaults to `"circle-fill"`.

- point.stroke:

  Numeric value specifying size of point stroke.

- ribbon.alpha:

  Numeric value specifying transparency of ribbon.

- legend.position:

  A character string specifying the position of the legend. Options are
  `"top", "bottom", "left", "right"`.

- facet.scales:

  A character string specifying the `scales` argument to use for
  faceting. Options are `"free", "fixed"`.

- custom.theme:

  A custom ggplot2 theme supplied either as a character string,
  function, or object of class `"theme"`.

- censoring.type:

  A character string specifying additional blq/alq plots to include.
  Only applicable if
  [`censoring`](https://github.com/certara/tidyvpc/reference/censoring.md)
  was performed.

- censoring.output:

  A character string specifying whether to return percentage of blq/alq
  plots as an arranged `"grid"` or as elements in a `"list"`. Only
  applicable if `censoring.type != "none"`.

- censoring.color:

  A named character vector of colors for the censoring plot lines. Names
  must include `"observed"` and `"simulated"`. Default is
  `c(observed = "black", simulated = "red")`. Only applicable if
  `censoring.type != "none"`.

- censoring.fill:

  A character string specifying the fill color for the censoring plot
  ribbon. Default is `"red"`. Only applicable if
  `censoring.type != "none"`.

- ...:

  Additional arguments for
  [`ggarrange`](https://rdrr.io/pkg/egg/man/ggarrange.html) e.g., `ncol`
  and `nrow`. Only used if `censoring.type != "none"` and
  `censoring.output == "grid"`.

## Value

A `ggplot` object.

## See also

`ggplot`
