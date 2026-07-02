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
