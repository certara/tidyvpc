# Perform binless Visual Predictive Check (VPC)

Use this function in place of traditional binning methods to derive VPC.
For continuous VPC, this is obtained using additive quantile regression
([`quantreg::rqss()`](https://rdrr.io/pkg/quantreg/man/rqss.html)) and
LOESS for pcVPC. While for categorical VPC, this is obtained using a
generalized additive model (`gam(family = "binomial")`).

## Usage

``` r
binless(o, ...)

# S3 method for class 'tidyvpcobj'
binless(
  o,
  optimize = TRUE,
  optimization.interval = c(0, 7),
  loess.ypc = NULL,
  lambda = NULL,
  span = NULL,
  sp = NULL,
  ...
)
```

## Arguments

- o:

  A `tidyvpcobj`.

- ...:

  Other arguments to include will be ignored.

- optimize:

  Logical indicating whether smoothing parameters should be optimized
  using AIC.

- optimization.interval:

  Numeric vector of length 2 specifying the min/max range of smoothing
  parameter for optimization. Only applicable if `optimize = TRUE`.

- loess.ypc:

  (Deprecated) Argument is ignored. For a LOESS pcVPC using the
  \`binless\` method, usage of
  [`predcorrect`](https://github.com/certara/tidyvpc/reference/predcorrect.md)
  will automatically perform LOESS prediction correction.

- lambda:

  Numeric vector of length 3 specifying lambda values for each quantile.
  If stratified, specify a `data.frame` with given strata represented
  the column name, and value specified as a numeric vector of length 3.
  See below examples. Only applicable to continuous VPC with
  `optimize = FALSE`.

- span:

  Numeric between 0,1 specifying smoothing parameter for LOESS
  prediction correction. Only applicable for continuous VPC with
  `optimize = FALSE` and usage of
  [`predcorrect`](https://github.com/certara/tidyvpc/reference/predcorrect.md).

- sp:

  List of smoothing parameters applied to
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html). Elements of
  list must be in the same order as unique values of DV. If one or more
  stratification variables present, the order of sp should be specified
  as unique combination of strata + DV, in ascending order. See below
  examples. Only applicable for categorical VPC, if `optimize = FALSE`.

## Value

For continuous VPC, updates `tidyvpcobj` with additive quantile
regression fits for observed and simulated data for quantiles specified
in the `qpred` argument of
[`vpcstats()`](https://github.com/certara/tidyvpc/reference/vpcstats.md).
If the `optimize = TRUE` argument is specified, the resulting
`tidyvpcobj` will contain optimized lambda values according to AIC. For
prediction corrected VPC (pcVPC), specifying `loess.ypc = TRUE` will
return optimized span value for LOESS smoothing. For categorical VPC,
updates `tidyvpcobj` with fits obtained by `gam(family="binomial")` for
observed and simulated data for each category of DV (in each stratum if
`stratify` defined). If `optimize = TRUE` argument is specified, the
resulting `tidyvpcobj` wil contain optimized `sp` values according to
AIC.

## See also

[`observed`](https://github.com/certara/tidyvpc/reference/observed.md)
[`simulated`](https://github.com/certara/tidyvpc/reference/simulated.md)
[`censoring`](https://github.com/certara/tidyvpc/reference/censoring.md)
[`predcorrect`](https://github.com/certara/tidyvpc/reference/predcorrect.md)
[`stratify`](https://github.com/certara/tidyvpc/reference/stratify.md)
[`binning`](https://github.com/certara/tidyvpc/reference/binning.md)
[`vpcstats`](https://github.com/certara/tidyvpc/reference/vpcstats.md)

## Examples

``` r
# \donttest{

require(magrittr)
#> Loading required package: magrittr
require(data.table)
#> Loading required package: data.table
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:base':
#> 
#>     %notin%

obs_data <- obs_data[MDV == 0]
sim_data <- sim_data[MDV == 0]


 vpc <- observed(obs_data, y = DV, x = TIME) %>%
      simulated(sim_data, y = DV) %>%
      binless() %>%
      vpcstats()
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: 
#> Warning: 
#> Warning: 

 # Binless example with LOESS prediction correction

 obs_data$PRED <- sim_data[REP == 1, PRED]

 vpc <- observed(obs_data, y = DV, x = TIME) %>%
      simulated(sim_data, y = DV) %>%
      binless(optimize = TRUE) %>%
      predcorrect(pred = PRED) %>% 
      vpcstats()
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: 
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: 
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: tiny diagonals replaced with Inf when calling blkfct

# Binless example with user specified lambda values stratified on
# "GENDER" with 2 levels ("M", "F"), 10%, 50%, 90% quantiles.

 lambda_strat <- data.table(
 GENDER_M = c(3,5,2),
 GENDER_F = c(1,3,4)
 )

 vpc <- observed(obs_data, y = DV, x = TIME) %>%
      simulated(sim_data, y = DV) %>%
      stratify(~ GENDER) %>%
      binless(optimize = FALSE, lambda = lambda_strat) %>%
      vpcstats(qpred = c(0.1, 0.5, 0.9))
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: tiny diagonals replaced with Inf when calling blkfct
#> Warning: tiny diagonals replaced with Inf when calling blkfct

 # Binless example for categorical DV with optimized smoothing
 vpc <- observed(obs_cat_data, x = agemonths, yobs = zlencat) %>%
       simulated(sim_cat_data, ysim = DV) %>%
       stratify(~ Country_ID_code) %>%
       binless() %>%
       vpcstats(vpc.type = "cat", quantile.type = 6)

 # Binless example for categorical DV with user specified sp values
 user_sp <- list(
 Country1_prob0 = 100,
 Country1_prob1 = 3,
 Country1_prob2 = 4,
 Country2_prob0 = 90,
 Country2_prob1 = 3,
 Country2_prob2 = 4,
 Country3_prob0 = 55,
 Country3_prob1 = 3,
 Country3_prob2 = 200)

 vpc <- observed(obs_cat_data, x = agemonths, yobs = zlencat) %>%
        simulated(sim_cat_data, ysim = DV) %>%
        stratify(~ Country_ID_code) %>%
        binless(optimize = FALSE, sp = user_sp) %>%
        vpcstats(vpc.type = "categorical", conf.level = 0.9, quantile.type = 6)
# }
```
