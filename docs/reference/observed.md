# Specify observed dataset and variables for VPC

The observed function is the first function in the vpc piping chain and
is used for specifying observed data and variables for VPC. Note:
Observed data must not contain missing DV and may require filtering
`MDV == 0` before generating VPC. Also observed data must be ordered by:
Subject (ID), IVAR (Time)

## Usage

``` r
observed(o, ...)

# S3 method for class 'data.frame'
observed(
  o,
  x,
  yobs,
  pred = NULL,
  blq = NULL,
  lloq = -Inf,
  alq = NULL,
  uloq = Inf,
  ...
)
```

## Arguments

- o:

  A `data.frame` of observation data.

- ...:

  Other arguments.

- x:

  Numeric x-variable, typically named TIME.

- yobs:

  Numeric y-variable, typically named DV.

- pred:

  Population prediction variable, typically named PRED.

- blq:

  Logical variable indicating below limit of quantification.

- lloq:

  Number or numeric variable in data indicating the lower limit of
  quantification.

- alq:

  Logical variable indicating above limit of quantification .

- uloq:

  Number or numeric variable in data indicating the upper limit of
  quantification.

## Value

A `tidyvpcobj` containing both original data and observed data formatted
with `x` and `y` variables as specified in function. Resulting data is
of class `data.frame` and `data.table`.

## See also

[`simulated`](https://github.com/certara/tidyvpc/reference/simulated.md)
[`censoring`](https://github.com/certara/tidyvpc/reference/censoring.md)
[`stratify`](https://github.com/certara/tidyvpc/reference/stratify.md)
[`predcorrect`](https://github.com/certara/tidyvpc/reference/predcorrect.md)
[`binning`](https://github.com/certara/tidyvpc/reference/binning.md)
[`binless`](https://github.com/certara/tidyvpc/reference/binless.md)
[`vpcstats`](https://github.com/certara/tidyvpc/reference/vpcstats.md)

## Examples

``` r

obs_data <- obs_data[MDV == 0]
sim_data <- sim_data[MDV == 0]

vpc <- observed(obs_data, x=TIME, y=DV)
```
