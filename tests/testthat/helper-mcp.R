# Optional Certara.R MCP host: only active when Certara.R has already loaded
# itself in this session (mirrors R/mcp_session.R's .tv_certara_fn gate) and
# exports the MCP repro API. Load Certara.R yourself first (e.g. library())
# to exercise these tests locally; they skip otherwise.
.certara_mcp_host <- function() {
  if (!isNamespaceLoaded("Certara.R")) return(FALSE)
  "mcp_repro_record" %in% getNamespaceExports("Certara.R")
}

.certara_mcp_fn <- function(name) {
  if (!.certara_mcp_host()) return(NULL)
  if (!name %in% getNamespaceExports("Certara.R")) return(NULL)
  get(name, envir = asNamespace("Certara.R"), mode = "function")
}

.certara_mcp_reset <- function() {
  for (nm in c("mcp_repro_reset", "mcp_report_reset", "mcp_session_paths_reset")) {
    fn <- .certara_mcp_fn(nm)
    if (!is.null(fn)) fn()
  }
  invisible(NULL)
}
