# tidyvpc anti-patterns

## tidyvpc.anti.vpc_plot_fn
type: anti_pattern
title: Do not use vpc_plot() — use plot() on the tidyvpc object
summary: vpc_plot is not the tidyvpc API; call plot() on the object returned by vpcstats().
keywords: vpc_plot, plot, anti-pattern
symbols: plot
error_signature: could not find function "vpc_plot"
related: tidyvpc.fn.plot, Certara.RsNLME.antipattern.vpc_plot_fn
provenance.source_file: R/plot.R
source.kind: Rd

The correct call is `plot(vpc)` after `vpcstats()`. Over MCP use
`tidyvpc_plot_vpc` to save PNG and register the figure in the report Rmd.
