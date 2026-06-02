# Compute VPC statistics

Compute prediction interval statistics for VPC.

## Usage

``` r
vpcstats(o, ...)

# S3 method for class 'tidyvpcobj'
vpcstats(
  o,
  vpc.type = c("continuous", "categorical"),
  qpred = c(0.05, 0.5, 0.95),
  ...,
  conf.level = 0.95,
  quantile.type = 7
)
```

## Arguments

- o:

  A `tidyvpcobj`.

- ...:

  Other arguments to include.

- vpc.type:

  Character specifying type of VPC (e.g., `"continuous"` (Default) or
  `"categorical"`).

- qpred:

  Numeric vector of length 3 specifying quantile prediction interval.
  Only applicable for `vpc.type = "continuous"`.

- conf.level:

  Numeric specifying confidence level.

- quantile.type:

  Numeric indicating quantile type. See
  [`quantile`](https://rdrr.io/r/stats/quantile.html).

## Value

Updates `tidyvpcobj` with `stats` `data.table` object, which contains
the following columns:

- `bin`: Resulting bin value as specified in
  [`binning()`](https://github.com/certara/tidyvpc/reference/binning.md)
  function

- `xbin`: Midpoint x-value of the observed data points in the bin as
  specified in `xbin` argument of
  [`binning()`](https://github.com/certara/tidyvpc/reference/binning.md)
  function

- `qname`: Quantiles specified in `qpred`. Only returned if
  `vpc.type = "continuous"`

- `pname`: Categorical probability names. Only returned if
  `vpc.type = "categorical"`

- `y`: Observed y value for the specified quantile

- `lo`: Lower bound of specified confidence interval for y value in
  simulated data

- `md`: Median y value in simulated data

- `hi`: Upper bound of specified confidence interval for y value in
  simulated data

## See also

[`observed`](https://github.com/certara/tidyvpc/reference/observed.md)
[`simulated`](https://github.com/certara/tidyvpc/reference/simulated.md)
[`censoring`](https://github.com/certara/tidyvpc/reference/censoring.md)
[`stratify`](https://github.com/certara/tidyvpc/reference/stratify.md)
[`binning`](https://github.com/certara/tidyvpc/reference/binning.md)
[`binless`](https://github.com/certara/tidyvpc/reference/binless.md)
[`predcorrect`](https://github.com/certara/tidyvpc/reference/predcorrect.md)
