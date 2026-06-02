# Specify simulated dataset and variables for VPC

The simulated function is used for specifying simulated input data and
variables for VPC. Note: Simulated data must not contain missing DV and
may require filtering `MDV == 0` before generating VPC. Simulated data
must be ordered by: Replicate, Subject (ID), IVAR (Time).

## Usage

``` r
simulated(o, ...)

# S3 method for class 'tidyvpcobj'
simulated(o, data, ysim, xsim, repl, ...)
```

## Arguments

- o:

  A `tidyvpcobj`.

- ...:

  Other arguments.

- data:

  A `data.frame` of simulated data.

- ysim:

  Numeric y-variable, typically named DV.

- xsim:

  Numeric x-variable, typically named TIME. This argument is not
  required, see details below.

- repl:

  Numeric replicate variable, typically named REPL. This argument is not
  required, see details below.

## Value

A `tidyvpcobj` containing simulated dataset `sim` formatted with columns
`x`, `y`, and `repl`, which indicates the replicate number. The column
`x` is used from the
[`observed()`](https://github.com/certara/tidyvpc/reference/observed.md)
function. Resulting dataset is of class `data.frame` and `data.table`.

## See also

[`observed`](https://github.com/certara/tidyvpc/reference/observed.md)
[`censoring`](https://github.com/certara/tidyvpc/reference/censoring.md)
[`stratify`](https://github.com/certara/tidyvpc/reference/stratify.md)
[`predcorrect`](https://github.com/certara/tidyvpc/reference/predcorrect.md)
[`binning`](https://github.com/certara/tidyvpc/reference/binning.md)
[`binless`](https://github.com/certara/tidyvpc/reference/binless.md)
[`vpcstats`](https://github.com/certara/tidyvpc/reference/vpcstats.md)

## Examples

``` r
require(magrittr)

vpc <- observed(obs_data, x=TIME, y=DV) %>%
    simulated(sim_data, y=DV)
```
