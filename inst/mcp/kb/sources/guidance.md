# tidyvpc guidance

## tidyvpc.guidance.toc
type: guidance_chapter
title: Visual predictive checks with tidyvpc
summary: Entry point for building, stratifying, and plotting VPCs from RsNLME simulation output using the tidyvpc pipe (observed, simulated, binning/binless, vpcstats, plot).
keywords: VPC, visual predictive check, tidyvpc, predcorrect, stratify, NPDE, RsNLME handoff
chapter_order: 1
guidance_refs: FDA Population PK Guidance (2022) section VI (model validation); EMA Guideline on reporting population PK analyses
related: Certara.RsNLME.guidance.validation, Certara.RsNLME.workflow.vpc_tidyvpc, tidyvpc.workflow.standard_vpc

tidyvpc turns RsNLME VPC simulation tables into publication-ready VPC plots.
RsNLME runs the simulation (`start_nlme_vpcmodel`); tidyvpc owns the VPC object
semantics and plotting.

Standard sequence:

1. Load observed (`predcheck0`) and simulated (`predout`) data from the VPC run.
2. Pipe through `observed()` + `simulated()` + `binning()` or `binless()` +
   `vpcstats()`.
3. Optionally `predcorrect()` or `stratify()` before binning.
4. `plot()` and save the figure; register in the modeling report via MCP plot tools.
