# tidyvpcobj handle cache for the MCP tool layer.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Optional Certara.R MCP host hooks. Only hook in when Certara.R has already
# loaded itself (i.e. it is the one orchestrating this session) rather than
# merely being installed; this also keeps tidyvpc free of any declared
# dependency on Certara.R, since we never take responsibility for loading it.
.tv_certara_fn <- function(name) {
  if (!isNamespaceLoaded("Certara.R")) return(NULL)
  if (!name %in% getNamespaceExports("Certara.R")) return(NULL)
  get(name, envir = asNamespace("Certara.R"), mode = "function")
}

.tv_host <- function() !is.null(.tv_certara_fn("mcp_repro_record"))

.tidyvpc_state <- new.env(parent = emptyenv())

.tidyvpc_state_reset <- function() {
  .tidyvpc_state$sessions <- list()
  .tidyvpc_state$counter <- 0L
  invisible(NULL)
}

.tidyvpc_state_ensure <- function() {
  if (is.null(.tidyvpc_state$sessions)) .tidyvpc_state_reset()
  invisible(NULL)
}

.tidyvpc_new_handle <- function() {
  .tidyvpc_state_ensure()
  .tidyvpc_state$counter <- .tidyvpc_state$counter + 1L
  paste0("vpc", .tidyvpc_state$counter)
}

.tidyvpc_store <- function(session, meta = list()) {
  handle <- .tidyvpc_new_handle()
  .tidyvpc_state$sessions[[handle]] <- list(session = session, meta = meta)
  handle
}

.tidyvpc_update <- function(handle, session) {
  .tidyvpc_state_ensure()
  if (is.null(.tidyvpc_state$sessions[[handle]])) {
    stop(sprintf("Unknown tidyvpc handle '%s'.", handle), call. = FALSE)
  }
  meta <- .tidyvpc_state$sessions[[handle]]$meta
  .tidyvpc_state$sessions[[handle]] <- list(session = session, meta = meta)
  invisible(handle)
}

.tidyvpc_get <- function(handle) {
  .tidyvpc_state_ensure()
  if (!is.character(handle) || length(handle) != 1L || !nzchar(handle)) {
    stop("`handle` must be a single non-empty string (e.g. \"vpc1\").",
         call. = FALSE)
  }
  entry <- .tidyvpc_state$sessions[[handle]]
  if (is.null(entry)) {
    active <- names(.tidyvpc_state$sessions)
    stop(sprintf(
      "Unknown tidyvpc handle '%s'. %s Load data with tidyvpc_load_from_dir first.",
      handle,
      if (length(active)) {
        paste0("Active handles: ", paste(active, collapse = ", "), ".")
      } else {
        "No VPC session has been created yet."
      }
    ), call. = FALSE)
  }
  entry$session
}

.tidyvpc_sessions_overview <- function() {
  .tidyvpc_state_ensure()
  handles <- names(.tidyvpc_state$sessions)
  if (!length(handles)) {
    return(data.frame(handle = character(0), run_dir = character(0),
                      has_vpc = logical(0), stringsAsFactors = FALSE))
  }
  data.frame(
    handle = handles,
    run_dir = vapply(handles, function(h) {
      .tidyvpc_state$sessions[[h]]$meta$run_dir %||% NA_character_
    }, character(1)),
    has_vpc = vapply(handles, function(h) {
      !is.null(.tidyvpc_state$sessions[[h]]$session$vpc_obj)
    }, logical(1)),
    stringsAsFactors = FALSE
  )
}

.tidyvpc_resolve_artifacts_dir <- function(run_dir) {
  art <- file.path(run_dir, "artifacts")
  if (dir.exists(art)) art else run_dir
}

.tidyvpc_read_sim <- function(art_dir, sim_file = NULL) {
  candidates <- c(sim_file, "predout.csv", "simout.csv")
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  for (f in candidates) {
    p <- file.path(art_dir, f)
    if (file.exists(p)) {
      return(list(path = p, data = utils::read.csv(p, stringsAsFactors = FALSE)))
    }
  }
  stop("No simulation file found (tried predout.csv, simout.csv).",
       call. = FALSE)
}

.tidyvpc_figures_dir <- function(out_dir = NULL) {
  if (!is.null(out_dir) && nzchar(out_dir)) return(out_dir)
  fn <- .tv_certara_fn("mcp_session_figures_dir")
  if (!is.null(fn)) {
    fig <- fn()
    if (!is.null(fig)) return(fig)
  }
  tempdir()
}

.tidyvpc_plot_path <- function(out_dir, stem, handle) {
  safe <- gsub("[^A-Za-z0-9_.-]+", "_", stem)
  file.path(out_dir, sprintf("%s_%s.png", safe, handle))
}
