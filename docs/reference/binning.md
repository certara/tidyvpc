# Binning methods for Visual Predictive Check (VPC)

This function executes binning methods available in classInt e.g.,
"jenks", "kmeans", "sd", "pretty", "pam", "kmeans", "hclust", "bclust",
"fisher", "dpih", "box", "headtails", and "maximum". You may also bin
directly on x-variable or alternatively specify "centers" or "breaks".
For explanation of binning methods see
[`classIntervals`](https://r-spatial.github.io/classInt/reference/classIntervals.html).

## Usage

``` r
binning(o, ...)

# S3 method for class 'tidyvpcobj'
binning(
  o,
  bin,
  data = o$data,
  xbin = "xmedian",
  centers,
  breaks,
  nbins,
  altx,
  stratum = NULL,
  by.strata = TRUE,
  ...
)
```

## Arguments

- o:

  A `tidyvpcobj`.

- ...:

  Other arguments to include for `classIntervals`. See `...` usage for
  `style` in `?classIntervals`.

- bin:

  Character string indicating binning method or unquoted variable name
  if binning on x-variable.

- data:

  Observed data supplied in
  [`observed()`](https://github.com/certara/tidyvpc/reference/observed.md)
  function.

- xbin:

  Character string indicating midpoint type for binning.

- centers:

  Numeric vector of centers for binning. Use `bin = "centers"`, if
  supplying centers.

- breaks:

  Numeric vector of breaks for binning. Use `bin = "breaks"`, if
  supplying breaks.

- nbins:

  Numeric number indicating the number of bins to use.

- altx:

  Unquoted variable name in observed data for alternative x-variable
  binning.

- stratum:

  List indicating the name of stratification variable and level, if
  using different binning methods by strata.

- by.strata:

  Logical indicating whether binning should be performed by strata.

## Value

Updates `tidyvpcobj` with `data.frame` containing bin information
including left/right boundaries and midpoint, as specified in `xbin`
argument.

## See also

[`observed`](https://github.com/certara/tidyvpc/reference/observed.md)
[`simulated`](https://github.com/certara/tidyvpc/reference/simulated.md)
[`censoring`](https://github.com/certara/tidyvpc/reference/censoring.md)
[`predcorrect`](https://github.com/certara/tidyvpc/reference/predcorrect.md)
[`stratify`](https://github.com/certara/tidyvpc/reference/stratify.md)
[`binless`](https://github.com/certara/tidyvpc/reference/binless.md)
[`vpcstats`](https://github.com/certara/tidyvpc/reference/vpcstats.md)

## Examples

``` r
# \donttest{
require(magrittr)

 # Binning on x-variable NTIME
vpc <- observed(obs_data, x=TIME, y=DV) %>%
    simulated(sim_data, y=DV) %>%
    binning(bin = NTIME) %>%
    vpcstats()

 # Binning using ntile and xmean for midpoint
vpc <- observed(obs_data, x=TIME, y=DV) %>%
    simulated(sim_data, y=DV) %>%
    binning(bin = "ntile", nbins = 8, xbin = "xmean") %>%
    vpcstats()

 # Binning using centers
vpc <- observed(obs_data, x=TIME, y=DV) %>%
    simulated(sim_data, y=DV) %>%
    binning(bin = "centers", centers = c(1,3,5,7)) %>%
    vpcstats()

 # Different Binning for each level of Strata
vpc <- observed(obs_data, x=TIME, y=DV) %>%
    simulated(sim_data, y=DV) %>%
    stratify(~ GENDER) %>%
    binning(stratum = list(GENDER = "M"), bin = "jenks", nbins = 5, by.strata = TRUE) %>%
    binning(stratum = list(GENDER = "F"), bin = "kmeans", nbins = 4, by.strata = TRUE) %>%
    vpcstats()

 # Binning Categorical DV using rounded time variable

  vpc <- observed(obs_cat_data, x = agemonths, y = zlencat ) %>%
      simulated(sim_cat_data, y = DV) %>%
      binning(bin = round(agemonths, 0)) %>%
      vpcstats(vpc.type = "categorical")
# }
```
