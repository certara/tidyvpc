#!/usr/bin/env Rscript
# Rebuild precompiled PDF vignettes for tidyvpc.
#
# Each user-facing vignette uses the R.rsp::asis pattern:
#   vignettes/<name>.Rmd      <- source (knitr::rmarkdown)
#   vignettes/<name>.pdf      <- precompiled output (built by this script,
#                                shipped to inst/doc/ at R CMD build time)
#   vignettes/<name>.pdf.asis <- static stub picked up by R CMD build,
#                                must already exist; its
#                                \VignetteIndexEntry{} must match the Rmd title.
#
# Usage:
#   Rscript tools/build_vignettes.R               # rebuild all user-facing PDFs
#   Rscript tools/build_vignettes.R cont qpc      # rebuild a subset
#
# Notes
# - pandoc 2.5 (e.g. PsN's bundled copy) breaks the lua filter chain.
#   This script verifies pandoc >= 3 before rendering.
# - MIKTEX_AUTO_INSTALL is forced off so the render does not hang on a
#   network fetch of optional LaTeX packages.

VIGNETTES <- c(
  cont    = "tidyvpc_cont.Rmd",
  cat     = "tidyvpc_cat.Rmd",
  nlmixr2 = "tidyvpc_nlmixr2.Rmd",
  qpc     = "tidyvpc_qpc.Rmd",
  RsNLME  = "tidyvpc_RsNLME.Rmd"
)

ensure_modern_pandoc <- function() {
  pv <- tryCatch(rmarkdown::pandoc_version(), error = function(e) NULL)
  if (is.null(pv) || pv < "3.0.0") {
    stop(
      "Need pandoc >= 3.0; found ", format(pv), ".\n",
      "On Windows, RStudio bundles a modern pandoc at\n",
      "  C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools\n",
      "Put that directory on PATH (or set RSTUDIO_PANDOC) before running."
    )
  }
  message("Using pandoc ", pv)
}

# Render one vignette to PDF.
# Forces the output format to pdf_document even when the Rmd YAML declares
# html_vignette, so the PDF used at R CMD build time is regenerated from
# the same source file pkgdown would render to HTML.
render_one <- function(rmd_path) {
  stopifnot(file.exists(rmd_path))
  out_dir <- dirname(rmd_path)
  out_pdf <- sub("\\.Rmd$", ".pdf", basename(rmd_path))

  fmt <- rmarkdown::pdf_document()
  # pandoc invoked with 8.3-shortname lua paths fails on some Windows
  # configurations; resolve to long paths defensively.
  if (length(fmt$pandoc$lua_filters)) {
    fmt$pandoc$lua_filters <- normalizePath(
      fmt$pandoc$lua_filters, winslash = "/", mustWork = TRUE
    )
  }

  message("Rendering ", rmd_path, " -> ", file.path(out_dir, out_pdf))
  rmarkdown::render(
    rmd_path,
    output_format = fmt,
    output_file   = out_pdf,
    output_dir    = out_dir,
    quiet         = TRUE
  )

  pdf_path <- file.path(out_dir, out_pdf)
  invisible(list(
    pdf    = pdf_path,
    bytes  = file.info(pdf_path)$size
  ))
}

# Resolve subset of vignettes to build, allowing short keys or full file names.
resolve_targets <- function(args) {
  if (length(args) == 0L) return(VIGNETTES)
  keys <- names(VIGNETTES)
  hits <- vapply(args, function(a) {
    if (a %in% keys) return(VIGNETTES[[a]])
    if (a %in% VIGNETTES) return(a)
    sub("^(vignettes/)?", "", a)
  }, character(1))
  setNames(hits, ifelse(args %in% keys, args, args))
}

main <- function(args) {
  Sys.setenv(MIKTEX_AUTO_INSTALL = "0")
  ensure_modern_pandoc()

  targets <- resolve_targets(args)
  results <- list()
  for (nm in names(targets)) {
    rmd <- file.path("vignettes", targets[[nm]])
    results[[nm]] <- render_one(rmd)
  }

  cat("\n--- Rebuilt vignette PDFs ---\n")
  for (nm in names(results)) {
    r <- results[[nm]]
    cat(sprintf("  %-8s  %7.1f KB  %s\n",
                nm, r$bytes / 1024, r$pdf))
  }
  invisible(results)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  main(args)
}
