# MCP tool layer for tidyvpc.

.tvts <- function(desc, required = FALSE) {
  if (!requireNamespace("ellmer", quietly = TRUE)) return(NULL)
  ellmer::type_string(desc, required = required)
}
.tvti <- function(desc, required = FALSE) {
  if (!requireNamespace("ellmer", quietly = TRUE)) return(NULL)
  ellmer::type_integer(desc, required = required)
}
.tvtb <- function(desc, required = FALSE) {
  if (!requireNamespace("ellmer", quietly = TRUE)) return(NULL)
  ellmer::type_boolean(desc, required = required)
}

.tvptool <- function(fun, name, description, arguments = list()) {
  ellmer::tool(fun, name = name, description = description, arguments = arguments)
}

.tv_repro_libs <- function() c("tidyvpc", "magrittr", "ggplot2")

.tv_sym <- function(x) {
  fn <- .tv_certara_fn("mcp_repro_sym")
  if (!is.null(fn)) fn(x) else structure(as.character(x), class = "mcp_repro_sym")
}

.tv_call <- function(fn, args = list(), var = NULL) {
  call_fn <- .tv_certara_fn("mcp_repro_call")
  if (!is.null(call_fn)) call_fn(fn, args, var) else NULL
}

.tv_record <- function(code) {
  record_fn <- .tv_certara_fn("mcp_repro_record")
  if (!is.null(code) && !is.null(record_fn)) {
    record_fn(code, libraries = .tv_repro_libs())
  }
  invisible(NULL)
}

.tv_repro_path <- function() {
  fn <- .tv_certara_fn("mcp_repro_path")
  if (!is.null(fn)) fn() else NA_character_
}

.tv_report_path <- function() {
  fn <- .tv_certara_fn("mcp_report_path")
  if (!is.null(fn)) fn() else NA_character_
}

.tv_record_ggsave <- function(path, width = 7, height = 5, dpi = 150) {
  .tv_record(.tv_call(
    "ggplot2::ggsave",
    list(filename = path, plot = .tv_sym("p"), width = width,
         height = height, dpi = dpi)
  ))
}

.tv_plot_payload <- function(path, caption, key = NULL) {
  report_fn <- .tv_certara_fn("mcp_report_figure")
  if (!is.null(report_fn)) {
    report_fn(path, caption, section = "diagnostics.vpc", key = key)
  }
  list(
    plot_path = path,
    repro_script = .tv_repro_path(),
    report_rmd = .tv_report_path()
  )
}

.tv_save_plot <- function(p, path, width = 7, height = 5, dpi = 150) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(filename = path, plot = p, width = width, height = height,
                  dpi = dpi)
  path
}

.tv_col_summary <- function(df) {
  list(
    n_rows = nrow(df),
    columns = names(df)
  )
}

# ---- data loaders -----------------------------------------------------------

.tv_load_from_dir <- function(run_dir, obs_file = "predcheck0.csv",
                             sim_file = NULL) {
  if (!dir.exists(run_dir)) {
    stop("`run_dir` does not exist: ", run_dir, call. = FALSE)
  }
  art <- .tidyvpc_resolve_artifacts_dir(run_dir)
  obs_path <- file.path(art, obs_file)
  if (!file.exists(obs_path)) {
    stop("Observed VPC file not found: ", obs_path, call. = FALSE)
  }
  obs <- utils::read.csv(obs_path, stringsAsFactors = FALSE)
  sim_info <- .tidyvpc_read_sim(art, sim_file)
  meta <- list(run_dir = run_dir, obs_path = obs_path, sim_path = sim_info$path)
  handle <- .tidyvpc_store(
    list(obs = obs, sim = sim_info$data, vpc_obj = NULL),
    meta = meta
  )
  .tv_record(.tv_call("read.csv", list(obs_path), var = "obs"))
  .tv_record(.tv_call("read.csv", list(sim_info$path), var = "sim"))
  list(
    handle = handle,
    observed = .tv_col_summary(obs),
    simulated = .tv_col_summary(sim_info$data),
    repro_script = .tv_repro_path()
  )
}

.tv_load_from_rds <- function(vpc_rds) {
  if (!file.exists(vpc_rds)) {
    stop("`vpc_rds` not found: ", vpc_rds, call. = FALSE)
  }
  vpc_job <- readRDS(vpc_rds)
  obs <- vpc_job$predcheck0
  sim <- vpc_job$predout %||% vpc_job$simout
  if (is.null(obs) || is.null(sim)) {
    stop("vpc.rds must contain predcheck0 and predout/simout.", call. = FALSE)
  }
  meta <- list(run_dir = dirname(vpc_rds), vpc_rds = vpc_rds)
  handle <- .tidyvpc_store(list(obs = obs, sim = sim, vpc_obj = NULL), meta = meta)
  .tv_record(.tv_call("readRDS", list(vpc_rds), var = "vpc_job"))
  .tv_record("obs <- vpc_job$predcheck0")
  .tv_record("sim <- if (!is.null(vpc_job$predout)) vpc_job$predout else vpc_job$simout")
  list(
    handle = handle,
    observed = .tv_col_summary(obs),
    simulated = .tv_col_summary(sim),
    repro_script = .tv_repro_path()
  )
}

# ---- build ------------------------------------------------------------------

.tv_nzchar <- function(x) !is.null(x) && nzchar(x)

# Collect warnings (e.g. non-replicate signals) so the agent can reason about
# them instead of losing them on stderr.
.tv_with_warnings <- function(expr) {
  warns <- character(0)
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warns[[length(warns) + 1L]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warns)
}

.tv_simulated <- function(vpc, sim, ysim_col, xsim_col = NULL, repl_col = NULL) {
  has_x <- .tv_nzchar(xsim_col)
  has_r <- .tv_nzchar(repl_col)
  if (has_x && has_r) {
    tidyvpc::simulated(vpc, sim, ysim = !!rlang::sym(ysim_col),
                       xsim = !!rlang::sym(xsim_col), repl = !!rlang::sym(repl_col))
  } else if (has_x) {
    tidyvpc::simulated(vpc, sim, ysim = !!rlang::sym(ysim_col),
                       xsim = !!rlang::sym(xsim_col))
  } else if (has_r) {
    tidyvpc::simulated(vpc, sim, ysim = !!rlang::sym(ysim_col),
                       repl = !!rlang::sym(repl_col))
  } else {
    tidyvpc::simulated(vpc, sim, ysim = !!rlang::sym(ysim_col))
  }
}

.tv_build_vpc <- function(handle, x_col = "TIME", yobs_col = "DV",
                          ysim_col = "DV", bin_col = NULL, nbins = 12L,
                          binless = FALSE, predcorrect = FALSE,
                          pred_col = NULL, stratify_formula = NULL,
                          xsim_col = NULL, repl_col = NULL) {
  sess <- .tidyvpc_get(handle)
  obs <- sess$obs
  sim <- sess$sim
  bin_col <- bin_col %||% x_col
  has_x <- .tv_nzchar(xsim_col)
  has_r <- .tv_nzchar(repl_col)
  non_replicate <- has_x || has_r
  built <- .tv_with_warnings({
    if (.tv_nzchar(pred_col)) {
      vpc <- tidyvpc::observed(obs, x = !!rlang::sym(x_col),
                               yobs = !!rlang::sym(yobs_col),
                               pred = !!rlang::sym(pred_col))
    } else {
      vpc <- tidyvpc::observed(obs, x = !!rlang::sym(x_col),
                               yobs = !!rlang::sym(yobs_col))
    }
    vpc <- .tv_simulated(vpc, sim, ysim_col, xsim_col, repl_col)
    if (predcorrect) vpc <- tidyvpc::predcorrect(vpc)
    if (.tv_nzchar(stratify_formula)) {
      fml <- stats::as.formula(stratify_formula)
      vpc <- if (non_replicate) tidyvpc::stratify(vpc, fml, data.sim = sim)
             else tidyvpc::stratify(vpc, fml)
    }
    if (isTRUE(binless)) {
      vpc <- tidyvpc::binless(vpc, x = !!rlang::sym(x_col))
    } else {
      vpc <- tidyvpc::binning(vpc, bin = !!rlang::sym(bin_col), nbins = nbins)
    }
    tidyvpc::vpcstats(vpc)
  })
  vpc <- built$value
  .tidyvpc_update(handle, list(obs = obs, sim = sim, vpc_obj = vpc))
  obs_call <- paste0(
    "observed(obs, x = ", x_col, ", yobs = ", yobs_col,
    if (.tv_nzchar(pred_col)) paste0(", pred = ", pred_col) else "",
    ")"
  )
  sim_call <- paste0(
    "  simulated(sim, ysim = ", ysim_col,
    if (has_x) paste0(", xsim = ", xsim_col) else "",
    if (has_r) paste0(", repl = ", repl_col) else "",
    ")"
  )
  .tv_record(paste0(handle, " <- ", obs_call, " %>%\n", sim_call))
  if (predcorrect) .tv_record(paste0(handle, " <- predcorrect(", handle, ")"))
  if (.tv_nzchar(stratify_formula)) {
    .tv_record(paste0(handle, " <- stratify(", handle, ", ",
                      deparse(stats::as.formula(stratify_formula)),
                      if (non_replicate) ", data.sim = sim" else "", ")"))
  }
  if (binless) {
    .tv_record(paste0(handle, " <- binless(", handle, ", x = ", x_col, ")"))
  } else {
    .tv_record(paste0(handle, " <- binning(", handle, ", bin = ", bin_col,
                      ", nbins = ", nbins, ")"))
  }
  .tv_record(paste0(handle, " <- vpcstats(", handle, ")"))
  list(handle = handle, class = class(vpc), non_replicate = non_replicate,
       warnings = built$warnings, repro_script = .tv_repro_path())
}

.tv_stratify <- function(handle, stratify_formula, data_sim = FALSE) {
  sess <- .tidyvpc_get(handle)
  if (is.null(sess$vpc_obj)) {
    stop("Build a VPC first with tidyvpc_build_vpc.", call. = FALSE)
  }
  fml <- stats::as.formula(stratify_formula)
  built <- .tv_with_warnings(
    if (isTRUE(data_sim)) tidyvpc::stratify(sess$vpc_obj, fml, data.sim = sess$sim)
    else tidyvpc::stratify(sess$vpc_obj, fml)
  )
  .tidyvpc_update(handle, list(obs = sess$obs, sim = sess$sim, vpc_obj = built$value))
  .tv_record(paste0(handle, " <- stratify(", handle, ", ", deparse(fml),
                    if (isTRUE(data_sim)) ", data.sim = sim" else "", ")"))
  list(handle = handle, warnings = built$warnings, repro_script = .tv_repro_path())
}

.tv_build_npde <- function(handle, x_col = "TIME", yobs_col = "DV",
                           ysim_col = "DV") {
  sess <- .tidyvpc_get(handle)
  obs <- sess$obs
  sim <- sess$sim
  vpc <- tidyvpc::observed(obs, x = !!rlang::sym(x_col),
                           yobs = !!rlang::sym(yobs_col))
  vpc <- tidyvpc::simulated(vpc, sim, ysim = !!rlang::sym(ysim_col))
  vpc <- tidyvpc::npde(vpc)
  .tidyvpc_update(handle, list(obs = obs, sim = sim, vpc_obj = vpc))
  .tv_record(paste0(handle, " <- npde(", handle, ")"))
  list(handle = handle, repro_script = .tv_repro_path())
}

# ---- plot -------------------------------------------------------------------

.tv_plot_vpc <- function(handle, out_dir = NULL, width = 7, height = 5,
                         dpi = 150) {
  sess <- .tidyvpc_get(handle)
  if (is.null(sess$vpc_obj)) {
    stop("Build a VPC first with tidyvpc_build_vpc.", call. = FALSE)
  }
  p <- plot(sess$vpc_obj)
  fig_dir <- .tidyvpc_figures_dir(out_dir)
  path <- .tidyvpc_plot_path(fig_dir, "vpc", handle)
  .tv_save_plot(p, path, width = width, height = height, dpi = dpi)
  .tv_record(.tv_call("plot", list(.tv_sym(handle)), var = "p"))
  .tv_record_ggsave(path, width = width, height = height, dpi = dpi)
  c(.tv_plot_payload(path, "Visual predictive check", key = paste0("vpc_", handle)),
    list(handle = handle))
}

.tv_list_sessions <- function() {
  list(
    sessions = .tidyvpc_sessions_overview(),
    repro_script = .tv_repro_path(),
    report_rmd = .tv_report_path()
  )
}

# ---- builder ----------------------------------------------------------------

.tidyvpc_tool_catalog <- function() {
  list(
    list(group = "data", tool = function() .tvptool(
      .tv_load_from_dir, "tidyvpc_load_from_dir",
      "Load predcheck0 and predout/simout CSVs from an RsNLME VPC run directory.",
      arguments = list(
        run_dir = .tvts("Path to the collected VPC job run directory.", TRUE),
        obs_file = .tvts("Observed table file name (default predcheck0.csv)."),
        sim_file = .tvts("Simulation file name (default predout.csv or simout.csv).")
      )
    )),
    list(group = "data", tool = function() .tvptool(
      .tv_load_from_rds, "tidyvpc_load_from_rds",
      "Load observed and simulated tables from vpc.rds saved by RsNLME.",
      arguments = list(
        vpc_rds = .tvts("Path to vpc.rds under artifacts/.", TRUE)
      )
    )),
    list(group = "build", tool = function() .tvptool(
      .tv_build_vpc, "tidyvpc_build_vpc",
      "Build a tidyvpc object: observed + simulated + binning/binless + vpcstats.",
      arguments = list(
        handle = .tvts("VPC data handle from a load tool.", TRUE),
        x_col = .tvts("Time/x column in observed data (default TIME)."),
        yobs_col = .tvts("Observed DV column (default DV)."),
        ysim_col = .tvts("Simulated DV column (default DV)."),
        bin_col = .tvts("Binning column (default same as x_col)."),
        nbins = .tvti("Number of bins for binning (default 12)."),
        binless = .tvtb("Use binless VPC instead of binning."),
        predcorrect = .tvtb("Apply prediction correction before binning."),
        pred_col = .tvts("Population prediction column for predcorrect()."),
        stratify_formula = .tvts("Optional formula text, e.g. '~ SEX'."),
        xsim_col = .tvts("Simulated x column for non-replicate simulated data (e.g. TIME)."),
        repl_col = .tvts("Replicate id column for non-replicate simulated data (e.g. REP).")
      )
    )),
    list(group = "build", tool = function() .tvptool(
      .tv_stratify, "tidyvpc_stratify",
      "Stratify an existing VPC object by covariate formula.",
      arguments = list(
        handle = .tvts("VPC handle with a built vpc object.", TRUE),
        stratify_formula = .tvts("Formula text, e.g. '~ SEX'.", TRUE),
        data_sim = .tvtb("Pass simulated data as data.sim for non-replicate stratification.")
      )
    )),
    list(group = "build", tool = function() .tvptool(
      .tv_build_npde, "tidyvpc_build_npde",
      "Build an NPDE object when appropriate.",
      arguments = list(
        handle = .tvts("VPC data handle from a load tool.", TRUE),
        x_col = .tvts("Time column (default TIME)."),
        yobs_col = .tvts("Observed DV column (default DV)."),
        ysim_col = .tvts("Simulated DV column (default DV).")
      )
    )),
    list(group = "plot", tool = function() .tvptool(
      .tv_plot_vpc, "tidyvpc_plot_vpc",
      "Plot the VPC, save PNG under figures/, register in report Rmd.",
      arguments = list(
        handle = .tvts("VPC handle with a built vpc object.", TRUE),
        out_dir = .tvts("Optional output directory for PNG."),
        width = .tvti("Plot width in inches (default 7)."),
        height = .tvti("Plot height in inches (default 5)."),
        dpi = .tvti("PNG resolution (default 150).")
      )
    )),
    list(group = "meta", tool = function() .tvptool(
      .tv_list_sessions, "tidyvpc_list_sessions",
      "List active tidyvpc handles and repro/report paths.",
      arguments = list()
    ))
  )
}

#' tidyvpc MCP tools for the Certara host
#'
#' @param groups Tool groups to include: data, build, plot, meta.
#' @return List of ellmer tool definitions, or empty when ellmer is absent.
#' @export
tidyvpc_mcp_tools <- function(groups = c("data", "build", "plot", "meta")) {
  if (!requireNamespace("ellmer", quietly = TRUE)) return(list())
  groups <- match.arg(groups, several.ok = TRUE)
  catalog <- .tidyvpc_tool_catalog()
  tools <- lapply(catalog, function(entry) {
    if (entry$group %in% groups) entry$tool() else NULL
  })
  Filter(Negate(is.null), tools)
}
