# Prediction corrected Visual Predictive Check (pcVPC)

Specify prediction variable for pcVPC.

## Usage

``` r
predcorrect(o, ...)

# S3 method for class 'tidyvpcobj'
predcorrect(o, pred, data = o$data, ..., log = FALSE, varcorr = FALSE)
```

## Arguments

- o:

  A \`tidyvpcobj\`.

- ...:

  Other arguments to include.

- pred:

  Prediction variable in observed data.

- data:

  Observed data supplied in \`observed()\` function.

- log:

  Logical indicating whether DV was modeled in logarithmic scale.

- varcorr:

  Logical indicating whether variability correction should be applied
  for prediction corrected dependent variable

## Value

Updates \`tidyvpcobj\` with required information to perform prediction
correction, which includes the \`predcor\` logical indicating whether
prediction corrected VPC is to be performed, the \`predcor.log\` logical
indicating whether the DV is on a log-scale, the \`varcorr\` logical
indicating whether variability correction for prediction corrected
dependent variable is applied and the \`pred\` prediction column from
the original data. Both \`obs\` and \`sim\` data tables in the returned
\`tidyvpcobj\` object have additional \`ypc\` column with the results of
prediction correction and \`ypcvc\` column if variability correction is
requested.

## See also

[`observed`](https://github.com/certara/tidyvpc/reference/observed.md)
[`simulated`](https://github.com/certara/tidyvpc/reference/simulated.md)
[`censoring`](https://github.com/certara/tidyvpc/reference/censoring.md)
[`stratify`](https://github.com/certara/tidyvpc/reference/stratify.md)
[`binning`](https://github.com/certara/tidyvpc/reference/binning.md)
[`binless`](https://github.com/certara/tidyvpc/reference/binless.md)
[`vpcstats`](https://github.com/certara/tidyvpc/reference/vpcstats.md)

## Examples

``` r
# \donttest{
require(magrittr)

obs_data <- obs_data[MDV == 0]
sim_data <- sim_data[MDV == 0]

 # Add PRED variable to observed data from first replicate of
 # simulated data

obs_data$PRED <- sim_data[REP == 1, PRED]

  vpc <- observed(obs_data, x=TIME, yobs=DV) %>%
       simulated(sim_data, ysim=DV) %>%
       binning(bin = NTIME) %>%
       predcorrect(pred=PRED, varcorr = TRUE) %>%
       vpcstats()

 # For binless loess prediction corrected, use predcorrect() before
 # binless() and set loess.ypc = TRUE

  vpc <- observed(obs_data, x=TIME, yobs=DV) %>%
       simulated(sim_data, ysim=DV) %>%
       predcorrect(pred=PRED) %>%
       binless() %>%
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
       # }
```
