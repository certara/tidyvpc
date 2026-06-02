# Compute Quantitative Predictive Check (QPC) statistics

Compute quantitative predictive quality metrics that summarize agreement
between observed and simulated data in a VPC. QPC statistics numerically
encode features typically assessed visually (coverage, deviation, trend,
and sharpness), and include a composite `qpc_score` (lower is better)
suitable for automated model comparison and optimization.

## Usage

``` r
qpcstats(
  o,
  alpha = 0.05,
  w = c(med_cov = 0.35, tail_cov = 0.2, mae = 0.15, drift = 0.1, sharp = 0.1, interval =
    0.1),
  sharp_ref = NULL,
  interval_ref = NULL,
  ...
)
```

## Arguments

- o:

  A `tidyvpcobj` produced by
  [`vpcstats`](https://github.com/certara/tidyvpc/reference/vpcstats.md)
  (typically using
  [`binless`](https://github.com/certara/tidyvpc/reference/binless.md)
  VPC summaries).

- alpha:

  Numeric. Miscoverage level for interval scoring (default `0.05`
  corresponds to a 95% prediction interval).

- w:

  Named numeric vector of weights used to combine component penalties
  into `qpc_score`. Names must include: `med_cov`, `tail_cov`, `mae`,
  `drift`, `sharp`, `interval`. Lower `qpc_score` is better.

- sharp_ref:

  Numeric or `NULL`. Reference value used to scale the sharpness
  (interval width) penalty. If `NULL` (default), a self-normalizing
  bounded transform is used so that a single VPC can be scored without
  external calibration. Set `sharp_ref` when you need scores to be
  comparable across a population of models (e.g., Darwin searches,
  benchmarking across multiple datasets/runs).

- interval_ref:

  Numeric or `NULL`. Reference value used to scale the interval-score
  penalty. If `NULL` (default), a self-normalizing bounded transform is
  used. Set `interval_ref` for cross-model or cross-run comparability
  (e.g., Darwin optimization or evaluation studies).

- ...:

  Additional arguments (reserved for future extensions).

## Value

Returns `tidyvpcobj` with an additional `qpc.stats` `data.table`
containing QPC summary metrics and `qpc_score`.

## Details

**When should I set `sharp_ref` / `interval_ref`?**

- *Single-model / single-VPC scoring (default):* leave both as `NULL`.
  This produces stable, bounded penalties without requiring any prior
  run for calibration.

- *Population scoring / optimization:* provide references when comparing
  many models (e.g., Darwin search) where you want the sharpness and
  interval-score penalties to be on a consistent scale across
  models/runs. A common choice is to compute `sharp_ref` and
  `interval_ref` from a representative run (e.g., median or 75th
  percentile values across all evaluated models).

## See also

[`vpcstats`](https://github.com/certara/tidyvpc/reference/vpcstats.md),
[`binless`](https://github.com/certara/tidyvpc/reference/binless.md),
[`predcorrect`](https://github.com/certara/tidyvpc/reference/predcorrect.md)

## Examples

``` r
if (FALSE) { # \dontrun{
vpc <- observed(obs_data, y = DV, x = TIME) %>%
  simulated(sim_data, y = DV) %>%
  binless(optimize = TRUE) %>%
  predcorrect(pred = PRED) %>%
  vpcstats()

# Default: single-model scoring (no calibration required)
vpc <- qpcstats(vpc)

# Population scoring (e.g., Darwin run): anchor penalties for comparability
vpc <- qpcstats(
  vpc,
  sharp_ref = 0.15,
  interval_ref = 2.5
)

vpc$qpc.stats
} # }
```
