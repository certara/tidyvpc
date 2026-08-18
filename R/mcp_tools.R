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
.tvtn <- function(desc, required = FALSE) {
  if (!requireNamespace("ellmer", quietly = TRUE)) return(NULL)
  ellmer::type_number(desc, required = required)
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

# Near-zero PRED threshold used by pred_quality to flag pre-Tlag / absorption
# distortion in prediction-corrected VPCs. Documented (not magic): values at
# or below this epsilon are counted in fraction_near_zero.
.tv_pred_near_zero_epsilon <- 1e-6

.tv_find_col <- function(nms, candidates) {
  hit <- intersect(candidates, nms)
  if (length(hit)) hit[[1]] else NULL
}

# Attach PRED from simulation replicate 0 onto the observed table when the
# observed table lacks it (RsNLME convention: PRED lives on predout/
# REPLICATE==0, not on predcheck0). Join keys are explicit ID + TIME (with
# common aliases), never row order. Returns list(obs, pred_available,
# pred_source, pred_col, join_note).
.tv_attach_pred <- function(obs, sim) {
  pred_col <- .tv_find_col(names(obs), c("PRED", "Pred", "pred"))
  if (!is.null(pred_col)) {
    return(list(obs = obs, pred_available = TRUE, pred_source = "observed",
                pred_col = pred_col, join_note = NULL))
  }
  sim_pred <- .tv_find_col(names(sim), c("PRED", "Pred", "pred"))
  if (is.null(sim_pred)) {
    return(list(obs = obs, pred_available = FALSE, pred_source = "none",
                pred_col = NULL, join_note = paste(
                  "No PRED column in observed or simulated tables;",
                  "prediction-corrected VPC (predcorrect) is unavailable",
                  "until PRED is provided.")))
  }
  repl_col <- .tv_find_col(names(sim), c("REPLICATE", "Replicate", "REP", "Rep",
                                         "repl", "replicate"))
  if (is.null(repl_col)) {
    return(list(obs = obs, pred_available = FALSE, pred_source = "none",
                pred_col = NULL, join_note = paste(
                  "Simulation table has PRED but no REPLICATE/REP column,",
                  "so replicate-0 PRED cannot be joined onto observed.")))
  }
  id_obs <- .tv_find_col(names(obs), c("ID", "id", "Subject", "SUBJECT"))
  id_sim <- .tv_find_col(names(sim), c("ID", "id", "Subject", "SUBJECT"))
  time_obs <- .tv_find_col(names(obs), c("TIME", "Time", "time", "IVAR", "TAD"))
  time_sim <- .tv_find_col(names(sim), c("TIME", "Time", "time", "IVAR", "TAD"))
  if (is.null(id_obs) || is.null(id_sim) || is.null(time_obs) || is.null(time_sim)) {
    return(list(obs = obs, pred_available = FALSE, pred_source = "none",
                pred_col = NULL, join_note = paste(
                  "Cannot join PRED from simulation replicate 0: need ID and",
                  "TIME (or IVAR) columns on both observed and simulated tables.")))
  }
  sim_df <- as.data.frame(sim, stringsAsFactors = FALSE)
  repl <- suppressWarnings(as.numeric(sim_df[[repl_col]]))
  # Prefer replicate 0 (RsNLME predout convention). When datasets number
  # replicates from 1 (e.g. tidyvpc::sim_data), fall back to the lowest
  # present replicate — PRED is the population prediction and is identical
  # across replicates for a given ID+TIME.
  rep0 <- sim_df[!is.na(repl) & abs(repl) < 1e-9, , drop = FALSE]
  used_repl <- 0
  if (!nrow(rep0)) {
    ok <- !is.na(repl)
    if (!any(ok)) {
      return(list(obs = obs, pred_available = FALSE, pred_source = "none",
                  pred_col = NULL, join_note = paste(
                    "Simulation table has PRED but", repl_col,
                    "is non-numeric; cannot attach population predictions.")))
    }
    used_repl <- min(repl[ok])
    rep0 <- sim_df[!is.na(repl) & abs(repl - used_repl) < 1e-9, , drop = FALSE]
  }
  if (!nrow(rep0)) {
    return(list(obs = obs, pred_available = FALSE, pred_source = "none",
                pred_col = NULL, join_note = paste(
                  "Simulation table has PRED but no usable", repl_col,
                  "rows; cannot attach population predictions.")))
  }
  keys_obs <- data.frame(
    .tv_join_id = as.character(obs[[id_obs]]),
    .tv_join_time = suppressWarnings(as.numeric(obs[[time_obs]])),
    stringsAsFactors = FALSE
  )
  keys_sim <- data.frame(
    .tv_join_id = as.character(rep0[[id_sim]]),
    .tv_join_time = suppressWarnings(as.numeric(rep0[[time_sim]])),
    PRED = suppressWarnings(as.numeric(rep0[[sim_pred]])),
    stringsAsFactors = FALSE
  )
  # One PRED per ID+TIME; if duplicates exist, take the first.
  keys_sim <- keys_sim[!duplicated(keys_sim[c(".tv_join_id", ".tv_join_time")]), ,
                       drop = FALSE]
  merged <- merge(keys_obs, keys_sim, by = c(".tv_join_id", ".tv_join_time"),
                  all.x = TRUE, sort = FALSE)
  # merge() reorders rows; restore observed order via match on the join keys.
  ord <- match(
    paste(keys_obs$.tv_join_id, keys_obs$.tv_join_time, sep = "\r"),
    paste(merged$.tv_join_id, merged$.tv_join_time, sep = "\r")
  )
  obs_out <- obs
  obs_out[["PRED"]] <- merged$PRED[ord]
  pred_source <- if (abs(used_repl) < 1e-9) {
    "sim_replicate_0"
  } else {
    sprintf("sim_replicate_%s", format(used_repl, scientific = FALSE, trim = TRUE))
  }
  list(
    obs = obs_out,
    pred_available = TRUE,
    pred_source = pred_source,
    pred_col = "PRED",
    join_note = sprintf(paste(
      "Attached PRED from simulation %s==%s onto observed via %s+%s",
      "(population prediction; replicate 0 preferred when present)."),
      repl_col, format(used_repl, scientific = FALSE, trim = TRUE),
      id_obs, time_obs)
  )
}

.tv_pred_quality <- function(obs, pred_col,
                             epsilon = .tv_pred_near_zero_epsilon) {
  if (is.null(pred_col) || !pred_col %in% names(obs)) {
    return(list(
      n = 0L, n_missing = 0L, fraction_missing = NA_real_,
      n_non_numeric = 0L, fraction_non_numeric = NA_real_,
      n_near_zero = 0L, fraction_near_zero = NA_real_,
      epsilon = epsilon,
      note = "No PRED column available for quality assessment."
    ))
  }
  raw <- obs[[pred_col]]
  n <- length(raw)
  # Treat literal "." (NONMEM-style missing) and NA as missing.
  as_chr <- trimws(as.character(raw))
  is_dot <- !is.na(as_chr) & as_chr == "."
  num <- suppressWarnings(as.numeric(raw))
  is_missing <- is.na(raw) | is_dot | (is.na(num) & !is.na(as_chr) & as_chr == "")
  is_non_numeric <- !is_missing & is.na(num)
  is_near_zero <- !is.na(num) & abs(num) <= epsilon
  list(
    n = as.integer(n),
    n_missing = as.integer(sum(is_missing)),
    fraction_missing = if (n) sum(is_missing) / n else NA_real_,
    n_non_numeric = as.integer(sum(is_non_numeric)),
    fraction_non_numeric = if (n) sum(is_non_numeric) / n else NA_real_,
    n_near_zero = as.integer(sum(is_near_zero)),
    fraction_near_zero = if (n) sum(is_near_zero) / n else NA_real_,
    epsilon = epsilon,
    note = paste(
      "fraction_near_zero counts |PRED| <=", format(epsilon, scientific = TRUE),
      "- high values (typical pre-Tlag) can distort prediction-corrected VPCs."
    )
  )
}

.tv_load_response <- function(handle, obs, sim, pred_info, extra = NULL) {
  out <- list(
    handle = handle,
    observed = .tv_col_summary(obs),
    simulated = .tv_col_summary(sim),
    pred_available = pred_info$pred_available,
    pred_source = pred_info$pred_source,
    pred_col = pred_info$pred_col,
    repro_script = .tv_repro_path()
  )
  if (!is.null(pred_info$join_note)) out$pred_note = pred_info$join_note
  if (length(extra)) out <- c(out, extra)
  out
}

# Replicate-mode tidyvpc is position-based: sim rows must be ordered by
# replicate then match observed ID+TIME within each replicate. Sort here so
# engine output written in a different row order cannot silently produce
# wrong bands. Multi-ObsName tables are warned, not mixed.
.tv_id_candidates <- function() c("ID", "id", "Subject", "SUBJECT")
.tv_time_candidates <- function() c("TIME", "Time", "time", "IVAR", "TAD")
.tv_repl_candidates <- function() {
  c("REPLICATE", "Replicate", "REP", "Rep", "repl", "replicate")
}
.tv_obsname_candidates <- function() c("ObsName", "OBSNAME", "obsname")

.tv_sort_keys_obs <- function(df) {
  cols <- character(0)
  for (cand in list(.tv_obsname_candidates(), .tv_id_candidates(),
                    .tv_time_candidates())) {
    hit <- .tv_find_col(names(df), cand)
    if (!is.null(hit)) cols <- c(cols, hit)
  }
  cols
}

.tv_sort_keys_sim <- function(df) {
  cols <- character(0)
  for (cand in list(.tv_repl_candidates(), .tv_obsname_candidates(),
                    .tv_id_candidates(), .tv_time_candidates())) {
    hit <- .tv_find_col(names(df), cand)
    if (!is.null(hit)) cols <- c(cols, hit)
  }
  cols
}

.tv_sort_df <- function(df, keys) {
  if (!length(keys) || !NROW(df)) return(df)
  cols <- lapply(keys, function(k) df[[k]])
  if (any(vapply(cols, is.null, logical(1)))) return(df)
  ord <- do.call(order, cols)
  out <- df[ord, , drop = FALSE]
  if (!inherits(out, "data.table")) rownames(out) <- NULL
  out
}

.tv_obsname_warning <- function(obs, sim) {
  vals <- character(0)
  for (df in list(obs, sim)) {
    col <- .tv_find_col(names(df), .tv_obsname_candidates())
    if (!is.null(col)) {
      v <- unique(as.character(df[[col]][!is.na(df[[col]])]))
      vals <- union(vals, v)
    }
  }
  if (length(vals) <= 1L) return(NULL)
  sprintf(paste(
    "ObsName has multiple values (%s). tidyvpc models one endpoint per VPC;",
    "split the tables by ObsName (see vignettes/categorical_data.Rmd) rather",
    "than mixing endpoints."),
    paste(vals, collapse = ", "))
}

.tv_order_check <- function(obs, sim) {
  id_obs <- .tv_find_col(names(obs), .tv_id_candidates())
  time_obs <- .tv_find_col(names(obs), .tv_time_candidates())
  id_sim <- .tv_find_col(names(sim), .tv_id_candidates())
  time_sim <- .tv_find_col(names(sim), .tv_time_candidates())
  if (is.null(id_obs) || is.null(time_obs) || is.null(id_sim) ||
      is.null(time_sim)) {
    return(list(ok = NA, note = "ID/TIME columns missing; check_order skipped."))
  }
  obs2 <- data.frame(ID = obs[[id_obs]], TIME = obs[[time_obs]],
                     stringsAsFactors = FALSE)
  sim2 <- data.frame(ID = sim[[id_sim]], TIME = sim[[time_sim]],
                     stringsAsFactors = FALSE)
  nrep <- tryCatch(tidyvpc::check_order(obs2, sim2), error = function(e) e)
  if (inherits(nrep, "error")) {
    return(list(ok = FALSE, error = conditionMessage(nrep)))
  }
  list(ok = TRUE, n_replicates = nrep)
}

.tv_prepare_tables <- function(obs, sim) {
  obs <- .tv_sort_df(obs, .tv_sort_keys_obs(obs))
  sim <- .tv_sort_df(sim, .tv_sort_keys_sim(sim))
  list(obs = obs, sim = sim,
       obsname_warning = .tv_obsname_warning(obs, sim),
       order_check = .tv_order_check(obs, sim),
       obs_keys = .tv_sort_keys_obs(obs),
       sim_keys = .tv_sort_keys_sim(sim))
}

.tv_record_sort <- function(prep) {
  if (length(prep$obs_keys)) {
    .tv_record(sprintf(
      "obs <- obs[order(%s), ]",
      paste(sprintf("obs[['%s']]", prep$obs_keys), collapse = ", ")))
  }
  if (length(prep$sim_keys)) {
    .tv_record(sprintf(
      "sim <- sim[order(%s), ]",
      paste(sprintf("sim[['%s']]", prep$sim_keys), collapse = ", ")))
  }
}

.tv_prep_extra <- function(prep) {
  extra <- list(order_check = prep$order_check)
  if (!is.null(prep$obsname_warning)) {
    extra$warnings <- list(prep$obsname_warning)
  }
  extra
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
  prep <- .tv_prepare_tables(obs, sim_info$data)
  obs <- prep$obs
  sim <- prep$sim
  pred_info <- .tv_attach_pred(obs, sim)
  obs <- pred_info$obs
  meta <- list(run_dir = run_dir, obs_path = obs_path, sim_path = sim_info$path,
               pred_source = pred_info$pred_source, pred_col = pred_info$pred_col)
  handle <- .tidyvpc_store(
    list(obs = obs, sim = sim, vpc_obj = NULL),
    meta = meta
  )
  .tv_record(.tv_call("read.csv", list(obs_path), var = "obs"))
  .tv_record(.tv_call("read.csv", list(sim_info$path), var = "sim"))
  .tv_record_sort(prep)
  if (isTRUE(pred_info$pred_available) &&
      startsWith(as.character(pred_info$pred_source %||% ""), "sim_replicate_")) {
    repl_val <- sub("^sim_replicate_", "", pred_info$pred_source)
    .tv_record(paste(
      sprintf("# Attach PRED from simulation replicate %s onto observed (ID+TIME join)",
              repl_val),
      sprintf(
        "rep0 <- sim[sim[[intersect(c('REPLICATE','REP'), names(sim))[1]]] == %s, ]",
        repl_val),
      "obs <- merge(obs, rep0[, c('ID','TIME','PRED')], by = c('ID','TIME'), all.x = TRUE)",
      sep = "\n"
    ))
  }
  .tv_load_response(handle, obs, sim, pred_info, .tv_prep_extra(prep))
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
  prep <- .tv_prepare_tables(obs, sim)
  obs <- prep$obs
  sim <- prep$sim
  pred_info <- .tv_attach_pred(obs, sim)
  obs <- pred_info$obs
  meta <- list(run_dir = dirname(vpc_rds), vpc_rds = vpc_rds,
               pred_source = pred_info$pred_source, pred_col = pred_info$pred_col)
  handle <- .tidyvpc_store(list(obs = obs, sim = sim, vpc_obj = NULL), meta = meta)
  .tv_record(.tv_call("readRDS", list(vpc_rds), var = "vpc_job"))
  .tv_record("obs <- vpc_job$predcheck0")
  .tv_record("sim <- if (!is.null(vpc_job$predout)) vpc_job$predout else vpc_job$simout")
  .tv_record_sort(prep)
  if (isTRUE(pred_info$pred_available) &&
      startsWith(as.character(pred_info$pred_source %||% ""), "sim_replicate_")) {
    repl_val <- sub("^sim_replicate_", "", pred_info$pred_source)
    .tv_record(paste(
      sprintf("# Attach PRED from simulation replicate %s onto observed (ID+TIME join)",
              repl_val),
      sprintf(
        "rep0 <- sim[sim[[intersect(c('REPLICATE','REP'), names(sim))[1]]] == %s, ]",
        repl_val),
      "obs <- merge(obs, rep0[, c('ID','TIME','PRED')], by = c('ID','TIME'), all.x = TRUE)",
      sep = "\n"
    ))
  }
  .tv_load_response(handle, obs, sim, pred_info, .tv_prep_extra(prep))
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
  prep <- .tv_prepare_tables(sess$obs, sess$sim)
  obs <- prep$obs
  sim <- prep$sim
  .tidyvpc_update(handle, list(obs = obs, sim = sim, vpc_obj = sess$vpc_obj))
  extra_warns <- character(0)
  if (!is.null(prep$obsname_warning)) extra_warns <- prep$obsname_warning
  bin_col <- bin_col %||% x_col
  has_x <- .tv_nzchar(xsim_col)
  has_r <- .tv_nzchar(repl_col)
  non_replicate <- has_x || has_r

  # Auto-select PRED when prediction correction is requested and the caller
  # did not name a column. Loaders attach PRED onto observed when missing, so
  # detecting it on the stored obs table is sufficient.
  if (isTRUE(predcorrect) && !.tv_nzchar(pred_col)) {
    detected <- .tv_find_col(names(obs), c("PRED", "Pred", "pred"))
    if (is.null(detected)) {
      stop(paste(
        "predcorrect=TRUE but no PRED column is available on the observed",
        "table. Reload with tidyvpc_load_from_dir/rds (which attaches PRED",
        "from simulation replicate 0 when present) or pass pred_col",
        "explicitly."), call. = FALSE)
    }
    pred_col <- detected
  }

  pred_quality <- NULL
  if (isTRUE(predcorrect) || .tv_nzchar(pred_col)) {
    pred_quality <- .tv_pred_quality(obs, pred_col)
  }

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
  out <- list(handle = handle, class = class(vpc), non_replicate = non_replicate,
              pred_col = pred_col,
              warnings = c(extra_warns, built$warnings),
              order_check = prep$order_check,
              repro_script = .tv_repro_path())
  if (!is.null(pred_quality)) out$pred_quality <- pred_quality
  out
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

# ---- QPC (opt-in scalar score) ----------------------------------------------

.tv_qpc_default_weights <- function() {
  c(med_cov = 0.35, tail_cov = 0.20, mae = 0.15, drift = 0.10,
    sharp = 0.10, interval = 0.10)
}

# Weight overrides are exposed as flat scalar arguments (ellmer has no MCP
# object/map type here), then merged onto tidyvpc::qpcstats()'s own defaults so
# a caller can override one weight without having to restate all six.
.tv_qpc_weights <- function(w_med_cov = NULL, w_tail_cov = NULL, w_mae = NULL,
                            w_drift = NULL, w_sharp = NULL, w_interval = NULL) {
  w <- .tv_qpc_default_weights()
  overrides <- c(med_cov = w_med_cov, tail_cov = w_tail_cov, mae = w_mae,
                 drift = w_drift, sharp = w_sharp, interval = w_interval)
  if (length(overrides)) w[names(overrides)] <- as.numeric(overrides)
  w
}

# data.table -> list of plain-list rows, JSON-serializable and independent of
# data.table/factor internals.
.tv_qpc_rows <- function(dt) {
  if (is.null(dt) || !nrow(dt)) return(list())
  df <- as.data.frame(dt, stringsAsFactors = FALSE)
  lapply(seq_len(nrow(df)), function(i) {
    lapply(as.list(df[i, , drop = FALSE]), function(v) {
      if (is.factor(v)) as.character(v) else v
    })
  })
}

# Wraps tidyvpc::qpcstats() on an already-built VPC handle (see
# tidyvpc_build_vpc, typically with binless = TRUE) and returns the composite
# qpc_score (lower is better) plus its component penalties and provenance.
# Secondary, informational metric only: no Certara MCP workflow (Darwin
# qualification, sequential LRT, or otherwise) invokes it automatically or
# treats it as an acceptance gate - see tidyvpc.workflow.qpc_scoring in the KB.
# Weight overrides (w_*) are flat scalars merged onto qpcstats()'s own
# defaults, since ellmer has no MCP object/map argument type here.
.tv_qpc_score <- function(handle, alpha = 0.05,
                          w_med_cov = NULL, w_tail_cov = NULL, w_mae = NULL,
                          w_drift = NULL, w_sharp = NULL, w_interval = NULL,
                          sharp_ref = NULL, interval_ref = NULL) {
  sess <- .tidyvpc_get(handle)
  if (is.null(sess$vpc_obj)) {
    stop("Build a VPC first with tidyvpc_build_vpc (binless recommended for qpc_score).",
         call. = FALSE)
  }
  w <- .tv_qpc_weights(w_med_cov, w_tail_cov, w_mae, w_drift, w_sharp, w_interval)
  built <- .tv_with_warnings(
    tidyvpc::qpcstats(sess$vpc_obj, alpha = alpha, w = w,
                      sharp_ref = sharp_ref, interval_ref = interval_ref)
  )
  scored <- built$value
  .tidyvpc_update(handle, list(obs = sess$obs, sim = sess$sim, vpc_obj = scored))

  w_arg <- paste0("c(", paste(sprintf("%s = %s", names(w), w), collapse = ", "), ")")
  .tv_record(paste0(
    handle, " <- qpcstats(", handle, ", alpha = ", alpha, ", w = ", w_arg,
    if (!is.null(sharp_ref)) paste0(", sharp_ref = ", sharp_ref) else "",
    if (!is.null(interval_ref)) paste0(", interval_ref = ", interval_ref) else "",
    ")"
  ))

  rows <- .tv_qpc_rows(scored$qpc.stats)
  is_overall <- vapply(rows, function(r) identical(r$qpc_scope, "overall"), logical(1))
  overall <- if (any(is_overall)) rows[is_overall][[1]] else NULL
  strata <- rows[!is_overall]

  list(
    handle = handle,
    qpc_score = overall$qpc_score %||% NA_real_,
    overall = overall,
    strata = if (length(strata)) strata else NULL,
    alpha = alpha,
    weights = as.list(w),
    sharp_ref = sharp_ref,
    interval_ref = interval_ref,
    warnings = built$warnings,
    note = paste(
      "qpc_score is an opt-in, secondary predictive-check metric (lower is",
      "better) - it is never an automatic acceptance gate. Use it alongside",
      "visual VPC review and diagnostics, not as a replacement for OFV",
      "parity or acceptance-gate checks."
    ),
    repro_script = .tv_repro_path()
  )
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
    list(group = "stats", tool = function() .tvptool(
      .tv_qpc_score, "qpc_score",
      paste(
        "Opt-in scalar QPC score (tidyvpc::qpcstats) on a built VPC - a",
        "secondary predictive-check metric, never an automatic gate."
      ),
      arguments = list(
        handle = .tvts("VPC handle with a built vpc object (binless recommended).", TRUE),
        alpha = .tvtn("Miscoverage level for interval scoring (default 0.05)."),
        w_med_cov = .tvtn("Weight override for the median-coverage penalty (default 0.35)."),
        w_tail_cov = .tvtn("Weight override for the tail-coverage penalty (default 0.20)."),
        w_mae = .tvtn("Weight override for the mean-absolute-error penalty (default 0.15)."),
        w_drift = .tvtn("Weight override for the residual-drift penalty (default 0.10)."),
        w_sharp = .tvtn("Weight override for the sharpness penalty (default 0.10)."),
        w_interval = .tvtn("Weight override for the interval-score penalty (default 0.10)."),
        sharp_ref = .tvtn("Reference value to scale sharpness for cross-model comparability."),
        interval_ref = .tvtn("Reference value to scale interval score for cross-model comparability.")
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
#' @param groups Tool groups to include: `"data"`, `"build"`, `"plot"`,
#'   `"stats"` (the opt-in `qpc_score` tool), `"meta"`. The Certara host's
#'   `execution`/`diagnostics` profiles expose `data`/`build`/`plot`/`meta`
#'   via an explicit tidyvpc entry so the VPC lifecycle is reachable;
#'   `"stats"` remains opt-in under the `"full"` profile (or an explicit
#'   group request) so `qpc_score` is never part of a default workflow.
#' @return List of ellmer tool definitions, or empty when ellmer is absent.
#' @export
tidyvpc_mcp_tools <- function(groups = c("data", "build", "plot", "stats", "meta")) {
  if (!requireNamespace("ellmer", quietly = TRUE)) return(list())
  groups <- match.arg(groups, several.ok = TRUE)
  catalog <- .tidyvpc_tool_catalog()
  tools <- lapply(catalog, function(entry) {
    if (entry$group %in% groups) entry$tool() else NULL
  })
  Filter(Negate(is.null), tools)
}
