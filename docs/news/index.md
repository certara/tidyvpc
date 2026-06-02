# Changelog

## tidyvpc 1.6.0

CRAN release: 2026-06-01

- New
  [`qpcstats()`](https://github.com/certara/tidyvpc/reference/qpcstats.md)
  function computes a Quantitative Predictive Check (QPC) score for
  continuous VPCs, including a composite `qpc_score` (lower is better)
  plus component penalties for coverage, MAE, drift, sharpness, and the
  Winkler interval score. Works with
  [`binless()`](https://github.com/certara/tidyvpc/reference/binless.md)
  and
  [`binning()`](https://github.com/certara/tidyvpc/reference/binning.md),
  with stratification, prediction correction, and censoring. New
  vignette `tidyvpc_qpc` documents usage
  [\#66](https://github.com/certara/tidyvpc/pull/66).
- Added support for non-replicate simulated data.
  [`simulated()`](https://github.com/certara/tidyvpc/reference/simulated.md)
  gains `xsim` and `repl` arguments,
  [`stratify()`](https://github.com/certara/tidyvpc/reference/stratify.md)
  gains a `data.sim` argument, and
  [`binning()`](https://github.com/certara/tidyvpc/reference/binning.md)/[`binless()`](https://github.com/certara/tidyvpc/reference/binless.md)
  propagate observed-data bins and strata to the simulated data when sim
  is not a replicate of obs
  [\#63](https://github.com/certara/tidyvpc/pull/63).
- [`plot.tidyvpcobj()`](https://github.com/certara/tidyvpc/reference/plot.tidyvpcobj.md)
  gains `censoring.color` and `censoring.fill` arguments to customize
  the colors used in BLQ/ALQ percentage plots
  [\#69](https://github.com/certara/tidyvpc/pull/69).
- R CMD check housekeeping: added Linux reference data for binless
  categorical tests and resolved remaining global-variable NOTEs
  [\#71](https://github.com/certara/tidyvpc/pull/71).
- The `tidyvpc_RsNLME` vignette is now shipped as a precompiled PDF (it
  was previously only available as source).

## tidyvpc 1.5.2

CRAN release: 2024-11-21

- Changes to the computational workflow for npde
  [\#59](https://github.com/certara/tidyvpc/pull/59)
- Added varcorr argument to predcorrect method
  [\#60](https://github.com/certara/tidyvpc/pull/60)
- CRAN fix [\#62](https://github.com/certara/tidyvpc/pull/62)

## tidyvpc 1.5.1

CRAN release: 2024-01-18

- Patch release that ensures unit tests do not fail when env var
  `_R_CHECK_DEPENDS_ONLY_=true` in R CMD check. As a result, the cluster
  dependency has been moved from Suggests to Imports.

## tidyvpc 1.5.0

CRAN release: 2023-10-30

- Support for generating percentage blq/alq plots using
  `plot.tidyvpcobj`. For VPC with
  [`censoring()`](https://github.com/certara/tidyvpc/reference/censoring.md),
  users can supply arguments `censoring.type` (options are `'none'`,
  `'blq'`, `'alq'`, or `'both'`, defaults to ‘none’) and
  `censoring.output` (options are `'grid'` or `'list'`, defaults to
  `'grid'`).[\#21](https://github.com/certara/tidyvpc/issues/21)
- Plotting updates were made for ggplot2 version 3.4.0 to use
  `linewidth` instead of `size` for
  lines[\#39](https://github.com/certara/tidyvpc/issues/39).
- [`simulated.tidyvpcobj()`](https://github.com/certara/tidyvpc/reference/simulated.md)
  detects if the number of simulated rows is not an integer multiple of
  the number of observed rows and adds the new `xsim` argument to test
  that x values match between replicated simulations. It will suggest
  that MDV filtering may not have occurred if either of these fails
  [\#35](https://github.com/certara/tidyvpc/issues/35).
- Prevent division by zero in
  [`predcorrect()`](https://github.com/certara/tidyvpc/reference/predcorrect.md)
  transformation [\#31](https://github.com/certara/tidyvpc/issues/31).
- Usability enhancements for prediction corrected VPC (pcVPC), which
  include support for
  [`binning.tidyvpcobj()`](https://github.com/certara/tidyvpc/reference/binning.md)
  either before or after usage of
  [`predcorrect.tidyvpcobj()`](https://github.com/certara/tidyvpc/reference/predcorrect.md),
  and automatically performing LOESS pcVPC when
  [`binless.tidyvpcobj()`](https://github.com/certara/tidyvpc/reference/binless.md)
  is used. As a result, the `loess.ypc` argument is no longer
  required[\#43](https://github.com/certara/tidyvpc/issues/43).
- The `binless.tidyvpcobj` function is now compatible with usage of
  `censoring.tidyvpcobj` with ALQ data, in addition to BLQ
  data.[\#49](https://github.com/certara/tidyvpc/issues/49)
- VPC can work with a single value in a group
  [\#51](https://github.com/certara/tidyvpc/issues/51)
- A vignette for use with `nlmixr2` was added
  [\#27](https://github.com/certara/tidyvpc/issues/27)

## tidyvpc 1.4.0

CRAN release: 2022-10-27

- Fix for npde calculation fix npde calc
  [\#16](https://github.com/certara/tidyvpc/pull/16)
- Added R CMD check and test coverage workflows
  [\#18](https://github.com/certara/tidyvpc/pull/18)
- Support `headtails`, `maximum`, and `box` binning methods
  [\#23](https://github.com/certara/tidyvpc/pull/23)
- Usage of
  [`predcorrect()`](https://github.com/certara/tidyvpc/reference/predcorrect.md)
  may now occur either before or after call to `binless(loess.ypc=TRUE)`
- Additional unit tests

## tidyvpc 1.3.0

CRAN release: 2022-03-10

- Additional arguments in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) function now
  include:
  - `point.size`
  - `point.stroke`
  - `point.shape`
  - `point.alpha`
  - `ribbon.alpha`
- Fixed plotting [issue](https://github.com/certara/tidyvpc/issues/11)
  to restore
  [`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  usage for one-side stratification formula

## tidyvpc 1.2.0

CRAN release: 2021-10-01

- Add support for categorical VPC using binless and binning methods
- Update [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  function to include plotting methods for categorical VPC

## tidyvpc 1.1.0

CRAN release: 2020-09-29

- Add NPDE functionality
- Add unit tests
- Bug fix for plotting of censored observations
- Maintain forward compatibility with `quantreg`

## tidyvpc 1.0.0

CRAN release: 2020-03-26

- Initial CRAN release
- Add
  [`binless()`](https://github.com/certara/tidyvpc/reference/binless.md)
  vpc functionality to existing code in
  [`vpcstats`](https://github.com/benjaminrich/vpcstats)
