# Censoring observed data for Visual Predictive Check (VPC)

Specify censoring variable or censoring value for VPC.

## Usage

``` r
censoring(o, ...)

# S3 method for class 'tidyvpcobj'
censoring(o, blq, lloq, alq, uloq, data = o$data, ...)
```

## Arguments

- o:

  A `tidyvpcobj`.

- ...:

  Other arguments to include.

- blq:

  blq variable if present in observed data.

- lloq:

  Numeric value or numeric variable in data indicating the upper limit
  of quantification.

- alq:

  Logical variable indicating above limit of quantification.

- uloq:

  Numeric value or numeric variable in data indicating the upper limit
  of quantification.

- data:

  Observed data supplied in
  [`observed()`](https://github.com/certara/tidyvpc/reference/observed.md)
  function.

## Value

Updates `obs` `data.frame` in `tidypcobj` with censored values for
observed data which includes `lloq` and `uloq` specified values for
lower/upper limit of quantification. Logicals for `blq` and `alq` are
returned that indicate whether the DV value lies below/above limit of
quantification.

## See also

[`observed`](https://github.com/certara/tidyvpc/reference/observed.md)
[`simulated`](https://github.com/certara/tidyvpc/reference/simulated.md)
[`stratify`](https://github.com/certara/tidyvpc/reference/stratify.md)
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
    censoring(blq=(DV < 50), lloq=50) %>%
    binning(bin = "pam", nbins = 5) %>%
    vpcstats()

#Using LLOQ variable in data with different values of LLOQ by Study:

obs_data$LLOQ <- obs_data[, ifelse(STUDY == "Study A", 50, 25)]

vpc <- observed(obs_data, x=TIME, y=DV) %>%
    simulated(sim_data, y=DV) %>%
    censoring(blq=(DV < LLOQ), lloq=LLOQ) %>%
    stratify(~ STUDY) %>%
    binning(bin = "kmeans", nbins = 4) %>%
    vpcstats()
# }
```
