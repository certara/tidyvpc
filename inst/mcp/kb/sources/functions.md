# tidyvpc function docs

## tidyvpc.fn.observed
type: function_doc
title: observed()
summary: First step in the VPC pipe; specify observed data and x/yobs columns.
keywords: observed, yobs, TIME, DV
symbols: observed
related: tidyvpc.fn.simulated
provenance.source_file: R/vpcstats.R
provenance.symbol: observed
source.kind: Rd

Use `yobs = DV` (not `y =`). Filter `MDV == 0` when required.

## tidyvpc.fn.simulated
type: function_doc
title: simulated()
summary: Attach simulated replicate data with ysim column.
keywords: simulated, ysim, replicates
symbols: simulated
related: tidyvpc.fn.observed
provenance.source_file: R/vpcstats.R
provenance.symbol: simulated
source.kind: Rd

Use `ysim = DV` for the simulated DV column.

## tidyvpc.fn.binning
type: function_doc
title: binning()
summary: Bin the x-axis for traditional VPC percentiles.
keywords: binning, nbins, centers, breaks
symbols: binning
related: tidyvpc.fn.binless
provenance.source_file: R/vpcstats.R
provenance.symbol: binning
source.kind: Rd

## tidyvpc.fn.binless
type: function_doc
title: binless()
summary: Binless VPC using AQR/LOESS (Jamsen et al.).
keywords: binless, AQR, LOESS
symbols: binless
related: tidyvpc.fn.binning
provenance.source_file: R/binless.R
provenance.symbol: binless
source.kind: Rd

## tidyvpc.fn.vpcstats
type: function_doc
title: vpcstats()
summary: Compute VPC percentile intervals and observed counts per bin.
keywords: vpcstats, percentiles, PI
symbols: vpcstats
related: tidyvpc.fn.plot
provenance.source_file: R/vpcstats.R
provenance.symbol: vpcstats
source.kind: Rd

## tidyvpc.fn.plot
type: function_doc
title: plot.tidyvpcobj
summary: ggplot VPC from a vpcstats() result.
keywords: plot, VPC figure
symbols: plot
related: tidyvpc.anti.vpc_plot_fn
provenance.source_file: R/plot.R
provenance.symbol: plot.tidyvpcobj
source.kind: Rd

## tidyvpc.fn.predcorrect
type: function_doc
title: predcorrect()
summary: Prediction correction for pcVPC.
keywords: predcorrect, pcVPC
symbols: predcorrect
related: tidyvpc.workflow.predcorrect_vpc
provenance.source_file: R/vpcstats.R
provenance.symbol: predcorrect
source.kind: Rd

## tidyvpc.fn.stratify
type: function_doc
title: stratify()
summary: Stratify VPC by covariate formula before binning.
keywords: stratify, formula, covariate
symbols: stratify
related: tidyvpc.workflow.stratified_vpc
provenance.source_file: R/stratify.R
provenance.symbol: stratify
source.kind: Rd

## tidyvpc.fn.npde
type: function_doc
title: npde()
summary: Normalized prediction distribution errors when appropriate.
keywords: NPDE, npde
symbols: npde
related: tidyvpc.guidance.toc
provenance.source_file: R/npde.R
provenance.symbol: npde
source.kind: Rd

## tidyvpc.fn.censoring
type: function_doc
title: censoring()
summary: Specify BLQ/ALQ censoring for observed data before binning and vpcstats().
keywords: censoring, BLQ, ALQ, LLOQ, ULOQ, BQL
symbols: censoring
related: tidyvpc.fn.observed, tidyvpc.fn.vpcstats, tidyvpc.fn.plot
provenance.source_file: R/vpcstats.R
provenance.symbol: censoring
source.kind: Rd

Apply after `observed()` + `simulated()` and before `binning()` / `binless()`.
Use logical `blq`/`alq` with numeric `lloq`/`uloq` (scalar or column).

## tidyvpc.fn.qpcstats
type: function_doc
title: qpcstats()
summary: Quantitative predictive check metrics and composite qpc_score after vpcstats().
keywords: QPC, qpcstats, qpc_score, predictive check
symbols: qpcstats
related: tidyvpc.fn.vpcstats, tidyvpc.fn.binless
provenance.source_file: R/qpc.R
provenance.symbol: qpcstats
source.kind: Rd

Run on a continuous VPC after `vpcstats()` (often with `binless()`). Not for categorical VPCs.
