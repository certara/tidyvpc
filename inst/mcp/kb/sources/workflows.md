# tidyvpc workflows

## tidyvpc.workflow.standard_vpc
type: workflow_recipe
title: Standard VPC from RsNLME output
summary: Load predcheck0 and predout from an RsNLME VPC run, build a binned VPC, and plot.
keywords: VPC, standard, binning, observed, simulated, vpcstats, plot
related: Certara.RsNLME.workflow.vpc_tidyvpc, tidyvpc.fn.observed, tidyvpc.fn.simulated, tidyvpc.fn.binning, tidyvpc.fn.vpcstats, tidyvpc.fn.plot
provenance.source_file: vignettes/tidyvpc_RsNLME.Rmd
source.kind: vignette

```r
library(tidyvpc)
library(magrittr)

vpc <- observed(obs, x = TIME, yobs = DV) %>%
  simulated(sim, ysim = DV) %>%
  binning(bin = TIME, nbins = 12) %>%
  vpcstats()
plot(vpc)
```

MCP: `tidyvpc_load_from_dir` -> `tidyvpc_build_vpc` -> `tidyvpc_plot_vpc`.

## tidyvpc.workflow.predcorrect_vpc
type: workflow_recipe
title: Prediction-corrected VPC
summary: Apply predcorrect() after observed/simulated and before binning for pcVPC.
keywords: pcVPC, predcorrect, prediction correction
related: tidyvpc.fn.predcorrect, tidyvpc.workflow.standard_vpc
provenance.source_file: vignettes/tidyvpc_RsNLME.Rmd
source.kind: vignette

Apply `predcorrect()` between `simulated()` and `binning()` when individual
predictions are available in the observed table.

## tidyvpc.workflow.stratified_vpc
type: workflow_recipe
title: Stratified VPC by covariate
summary: stratify() on a formula, then bin and plot per stratum.
keywords: stratify, covariate, subgroup VPC
related: tidyvpc.fn.stratify, Certara.RsNLME.guidance.topic.vpc_stratification
provenance.source_file: vignettes/tidyvpc_RsNLME.Rmd
source.kind: vignette

```r
vpc <- observed(obs, x = TIME, yobs = DV) %>%
  simulated(sim, ysim = DV) %>%
  stratify(~ SEX) %>%
  binning(bin = TIME, nbins = 8) %>%
  vpcstats()
```

MCP: `tidyvpc_stratify` after `tidyvpc_build_vpc` (or pass `stratify_formula` to build).

## tidyvpc.workflow.non_replicate_vpc
type: workflow_recipe
title: Non-replicate (rich) simulated data VPC
summary: When simulated data is not a 1:1 replicate of observed data, supply xsim and repl so bins/strata propagate correctly.
keywords: non-replicate, rich simulation, xsim, repl, data.sim
related: tidyvpc.fn.simulated, tidyvpc.fn.stratify, tidyvpc.workflow.standard_vpc
provenance.source_file: vignettes/tidyvpc_whats_new.Rmd
source.kind: vignette

`simulated()` warns when the number of simulated rows is not a multiple of the
observed rows. If non-replicate data is intentional, pass `xsim` and `repl`; if
not, verify observed data was filtered (e.g. `MDV == 0`). Stratify non-replicate
data with `data.sim`.

```r
vpc <- observed(obs, x = TIME, yobs = DV) %>%
  simulated(sim_rich, xsim = TIME, ysim = DV, repl = REP) %>%
  stratify(~ GENDER, data.sim = sim_rich) %>%
  binning(bin = "jenks", nbins = 5) %>%
  vpcstats()
```

MCP: pass `xsim_col`/`repl_col` to `tidyvpc_build_vpc`, and `data_sim = TRUE` to `tidyvpc_stratify`.

## tidyvpc.workflow.qpc_scoring
type: workflow_recipe
title: Opt-in QPC scoring after vpcstats()
summary: qpcstats() turns a built VPC into a single qpc_score for optional, secondary model comparison - never an automatic acceptance gate.
keywords: QPC, qpcstats, qpc_score, opt-in, secondary metric, Darwin, sequential LRT
related: tidyvpc.fn.qpcstats, tidyvpc.fn.binless, tidyvpc.workflow.standard_vpc, Certara.RsNLME.fn.sequential_lrt_tools
provenance.source_file: R/qpc.R
source.kind: Rd

`qpc_score` (MCP) / `qpcstats()` (R) numerically encodes what a VPC review
visually assesses - coverage, deviation, drift, and sharpness - into a single
composite score (lower is better). Use it as an **opt-in, secondary**
criterion alongside a visual VPC and diagnostics review. Never treat it as an
automatic acceptance gate, and never call it without an explicit user
request: no Certara MCP workflow (Darwin candidate qualification, structural-
anchor selection, sequential LRT) invokes it on its own.

```r
vpc <- observed(obs, x = TIME, yobs = DV) %>%
  simulated(sim, ysim = DV) %>%
  binless(x = TIME) %>%
  vpcstats()

# Single-model scoring: leave sharp_ref/interval_ref NULL.
vpc <- qpcstats(vpc)
vpc$qpc.stats

# Population scoring (e.g. comparing several qualified candidates): anchor
# sharp_ref/interval_ref from a representative run for cross-model comparability.
vpc <- qpcstats(vpc, sharp_ref = 0.15, interval_ref = 2.5)
```

MCP: `tidyvpc_build_vpc` (prefer `binless = TRUE`) -> `qpc_score`. When
comparing scores across several candidates or sessions, set `sharp_ref` /
`interval_ref` to the same value on every call so the sharpness and interval
penalties stay on a consistent scale.
