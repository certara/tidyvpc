# Stratification for Visual Predictive Check (VPC)

Use to specify stratification variables for VPC.

## Usage

``` r
stratify(o, ...)

# S3 method for class 'tidyvpcobj'
stratify(o, formula, data = o$data, data.sim = NULL, ...)
```

## Arguments

- o:

  A `tidyvpcobj`.

- ...:

  Other arguments to include.

- formula:

  Formula for stratification.

- data:

  Observed data supplied in
  [`observed()`](https://github.com/certara/tidyvpc/reference/observed.md)
  function.

- data.sim:

  Simulated data supplied in
  [`simulated()`](https://github.com/certara/tidyvpc/reference/simulated.md)
  function.

## Value

Returns updated `tidyvpcobj` with stratification formula, stratification
column(s), and strat.split datasets, which is `obs` split by unique
levels of stratification variable(s). Resulting datasets are of class
object `data.frame` and `data.table`.

## See also

[`observed`](https://github.com/certara/tidyvpc/reference/observed.md)
[`simulated`](https://github.com/certara/tidyvpc/reference/simulated.md)
[`censoring`](https://github.com/certara/tidyvpc/reference/censoring.md)
[`predcorrect`](https://github.com/certara/tidyvpc/reference/predcorrect.md)
[`binning`](https://github.com/certara/tidyvpc/reference/binning.md)
[`binless`](https://github.com/certara/tidyvpc/reference/binless.md)
[`vpcstats`](https://github.com/certara/tidyvpc/reference/vpcstats.md)

## Examples

``` r
# \donttest{
require(magrittr)

vpc <- observed(obs_data, x=TIME, y=DV) %>%
    simulated(sim_data, y=DV) %>%
    stratify(~ GENDER) %>%
    binning(NTIME) %>%
    vpcstats()

# Example with 2-way stratification by GENDER and STUDY.

vpc <- vpc %>%
    stratify(~ GENDER + STUDY) %>%
    binning(bin = "centers", centers = c(1,3,5,7,10)) %>%
    vpcstats()
# }
```
