## Release summary

This is a minor release `1.6.0` with the following user-visible changes:

* New `qpcstats()` function computes a Quantitative Predictive Check (QPC)
  score for continuous VPCs, including a composite `qpc_score` (lower is
  better) plus component penalties for coverage, MAE, drift, sharpness, and
  the Winkler interval score. Works with `binless()` and `binning()`, with
  stratification, prediction correction, and censoring. A new vignette
  (`tidyvpc_qpc`) documents the methodology and typical workflows.
* Added support for non-replicate simulated data. `simulated()` gains `xsim`
  and `repl` arguments; `stratify()` gains a `data.sim` argument; and
  `binning()` / `binless()` propagate observed-data bins and strata to the
  simulated data when sim is not a replicate of obs.
* `plot.tidyvpcobj()` gains `censoring.color` and `censoring.fill` arguments
  to customize the colors used in BLQ/ALQ percentage plots.
* R CMD check housekeeping: added Linux reference data for binless
  categorical tests and resolved remaining global-variable NOTEs.
* The `tidyvpc_RsNLME` vignette is now shipped as a precompiled PDF
  (previously available only as source).

## Test environments

* Local: Windows 11, R 4.5.1 and R 4.6.0 (R CMD check --as-cran clean on both;
  full test suite with NOT_CRAN=true also clean on both)
* GitHub Actions (check-standard): Windows-latest, macOS-latest,
  Ubuntu-latest, R release and devel
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

R CMD check succeeded.
