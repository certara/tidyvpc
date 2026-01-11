#' Compute Quantitative Predictive Check (QPC) statistics
#'
#' Compute quantitative predictive quality metrics that summarize agreement
#' between observed and simulated data in a VPC. QPC statistics numerically encode
#' features typically assessed visually (coverage, deviation, trend, and sharpness),
#' and include a composite \code{qpc_score} (lower is better) suitable for automated
#' model comparison and optimization.
#'
#' @param o A \code{tidyvpcobj} produced by \code{\link{vpcstats}} (typically using
#'   \code{\link{binless}} VPC summaries).
#' @param alpha Numeric. Miscoverage level for interval scoring (default \code{0.05}
#'   corresponds to a 95\% prediction interval).
#' @param w Named numeric vector of weights used to combine component penalties into
#'   \code{qpc_score}. Names must include:
#'   \code{med_cov}, \code{tail_cov}, \code{mae}, \code{drift}, \code{sharp}, \code{interval}.
#'   Lower \code{qpc_score} is better.
#' @param sharp_ref Numeric or \code{NULL}. Reference value used to scale the sharpness
#'   (interval width) penalty. If \code{NULL} (default), a self-normalizing bounded
#'   transform is used so that a single VPC can be scored without external calibration.
#'   Set \code{sharp_ref} when you need scores to be comparable across a population
#'   of models (e.g., Darwin searches, benchmarking across multiple datasets/runs).
#' @param interval_ref Numeric or \code{NULL}. Reference value used to scale the
#'   interval-score penalty. If \code{NULL} (default), a self-normalizing bounded
#'   transform is used. Set \code{interval_ref} for cross-model or cross-run comparability
#'   (e.g., Darwin optimization or evaluation studies).
#' @param ... Additional arguments (reserved for future extensions).
#'
#' @return Returns \code{tidyvpcobj} with an additional \code{qpc.stats}
#' \code{data.table} containing QPC summary metrics and \code{qpc_score}.
#'
#' @details
#' \strong{When should I set \code{sharp_ref} / \code{interval_ref}?}
#' \itemize{
#'   \item \emph{Single-model / single-VPC scoring (default):} leave both as \code{NULL}.
#'   This produces stable, bounded penalties without requiring any prior run for calibration.
#'   \item \emph{Population scoring / optimization:} provide references when comparing many
#'   models (e.g., Darwin search) where you want the sharpness and interval-score penalties
#'   to be on a consistent scale across models/runs. A common choice is to compute
#'   \code{sharp_ref} and \code{interval_ref} from a representative run (e.g., median or 75th
#'   percentile values across all evaluated models).
#' }
#'
#' @examples
#' \dontrun{
#' vpc <- observed(obs_data, y = DV, x = TIME) %>%
#'   simulated(sim_data, y = DV) %>%
#'   binless(optimize = TRUE) %>%
#'   predcorrect(pred = PRED) %>%
#'   vpcstats()
#'
#' # Default: single-model scoring (no calibration required)
#' vpc <- qpcstats(vpc)
#'
#' # Population scoring (e.g., Darwin run): anchor penalties for comparability
#' vpc <- qpcstats(
#'   vpc,
#'   sharp_ref = 0.15,
#'   interval_ref = 2.5
#' )
#'
#' vpc$qpc.stats
#' }
#'
#' @seealso \code{\link{vpcstats}}, \code{\link{binless}}, \code{\link{predcorrect}}
#' @export
qpcstats <- function(o,
                    alpha = 0.05,
                    w = c(med_cov = 0.35, tail_cov = 0.20, mae = 0.15, drift = 0.10,
                          sharp = 0.10, interval = 0.10),
                    sharp_ref = NULL,
                    interval_ref = NULL,
                    ...) {
  UseMethod("qpcstats")
}

#' @export
qpcstats.tidyvpcobj <- function(o,
                                alpha = 0.05,
                                w = c(med_cov = 0.35, tail_cov = 0.20, mae = 0.15, drift = 0.10,
                                      sharp = 0.10, interval = 0.10),
                                sharp_ref = NULL,
                                interval_ref = NULL,
                                ...) {
  
  req_names <- c("med_cov", "tail_cov", "mae", "drift", "sharp", "interval")
  if (!is.numeric(w) || is.null(names(w)) || !all(req_names %in% names(w))) {
    stop("`w` must be a named numeric vector containing: ",
         paste(req_names, collapse = ", "), call. = FALSE)
  }
  
  if (!is.null(o$vpc.type) && identical(o$vpc.type, "categorical")) {
    stop("`qpcstats()` is currently supported for continuous VPCs only.", call. = FALSE)
  }
  
  if (is.null(o$stats)) stop("`o$stats` is missing. Run `vpcstats()` before `qpcstats()`.", call. = FALSE)
  
  qpc.stats <- qc_features_continuous(
    stats = o$stats,
    alpha = alpha,
    w = w,
    sharp_ref = sharp_ref,
    interval_ref = interval_ref
  )
  
  update(o, qpc.stats = qpc.stats)
}

qc_features_continuous <- function(stats,
                                   alpha = 0.05,
                                   w = c(med_cov = 0.35, tail_cov = 0.20, mae = 0.15, drift = 0.10,
                                         sharp = 0.10, interval = 0.10),
                                   sharp_ref = NULL,
                                   interval_ref = NULL,
                                   .include_overall = TRUE) {
  q <- qkey <- band_w <- in_band <- y <- lo <- hi <- md <- dev_abs <- dev_mid <- interval_score <-
    width_rel_md <- resid <- rho_resid_x <- rho_abs <- mae_midwidth <- coverage <- sharpness_rel <-
    md_abs_median <- name <- feat <- .qpc_group <- qpc_scope <- n <- NULL
  . <- list
  stopifnot(is.numeric(alpha), length(alpha) == 1, is.finite(alpha), alpha > 0, alpha < 1)
  
  s <- data.table::as.data.table(stats)
  
  xcol <- if ("xbin" %in% names(s)) {
    "xbin"
  } else if ("x" %in% names(s)) {
    "x"
  } else {
    stop("`stats` must contain either `x` (binless) or `xbin` (binning).", call. = FALSE)
  }
  
  required <- c(xcol, "qname", "y", "md", "lo", "hi")
  missing <- setdiff(required, names(s))
  if (length(missing) > 0) {
    stop("`stats` is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  
  # Identify strata columns: anything that isn't the standard VPC stats columns
  core_cols <- unique(c("x", "xbin", "bin", "qname", "y", "md", "lo", "hi"))
  strat_cols <- setdiff(names(s), core_cols)
  
  q_chr <- as.character(s[["qname"]])
  s[, q := suppressWarnings(as.numeric(sub("^q", "", q_chr)))]
  if (all(is.na(s$q))) {
    stop("`stats$qname` could not be parsed as numeric quantiles (e.g. q0.05, q0.5, q0.95).", call. = FALSE)
  }
  # Canonical quantile label for stable grouping/feature naming (handles q0.5 vs q0.50)
  s[, qkey := sprintf("q%0.2f", q)]
  
  # ---- Per-row metrics ----
  s[, band_w := pmax(hi - lo, .Machine$double.eps)]
  s[, in_band := ifelse(is.na(y) | is.na(lo) | is.na(hi), NA_integer_, as.integer(y >= lo & y <= hi))]
  s[, dev_abs := abs(y - md)]
  s[, dev_mid := dev_abs / (band_w / 2)]
  s[, interval_score := band_w +
      (2 / alpha) * pmax(lo - y, 0) +
      (2 / alpha) * pmax(y - hi, 0)]
  s[, width_rel_md := band_w / pmax(abs(md), 1e-8)]
  s[, resid := (y - md)]
  
  # ---- Curve-level summaries ----
  by_q <- c(strat_cols, "q", "qkey")
  per_q <- s[, {
    x <- get(xcol)
    r <- resid
    ord <- order(x)
    tv <- if (.N < 2) 0 else sum(abs(diff(r[ord])), na.rm = TRUE)
    rho <- suppressWarnings(
      tryCatch(
        stats::cor(r, x, method = "spearman", use = "complete.obs"),
        error = function(e) NA_real_
      )
    )
    .(
      n = .N,
      coverage = mean(in_band, na.rm = TRUE),
      mae_midwidth = mean(dev_mid, na.rm = TRUE),
      max_dev_mid = {
        if (all(is.na(dev_mid))) NA_real_ else max(dev_mid, na.rm = TRUE)
      },
      rho_resid_x = rho,
      tv_resid = tv,
      sharpness_rel = mean(width_rel_md, na.rm = TRUE),
      interval_score = mean(interval_score, na.rm = TRUE),
      band_w_median = stats::median(band_w, na.rm = TRUE),
      md_abs_median = stats::median(abs(md), na.rm = TRUE)
    )
  }, by = by_q]
  per_q[, n := as.double(n)]
  
  # ---- Select median/tails by numeric q (robust to qname formatting) ----
  bound01 <- function(x) pmin(pmax(x, 0), 1)
  bmae <- function(x) bound01(pmin(x, 2) / 2)
  
  # Guardrail: cor can return NA/NaN for constant vectors; treat as no drift evidence
  per_q[, rho_abs := abs(rho_resid_x)]
  per_q[!is.finite(rho_abs), rho_abs := 0]
  
  # Compute penalties per stratum (or globally if no strata)
  by_strat <- strat_cols
  if (length(by_strat) == 0) {
    per_q[, .qpc_group := 1L]
    by_strat <- ".qpc_group"
  }
  
  penalties <- per_q[, {
    qs <- q
    q_med <- qs[which.min(abs(qs - 0.5))]
    q_low <- min(qs, na.rm = TRUE)
    q_high <- max(qs, na.rm = TRUE)
    
    cov_med <- coverage[q == q_med][1]
    cov_low <- coverage[q == q_low][1]
    cov_high <- coverage[q == q_high][1]
    
    coverage_penalty_med <- bound01(1 - cov_med)
    coverage_penalty_tails <- mean(c(bound01(1 - cov_low), bound01(1 - cov_high)), na.rm = TRUE)
    
    mae_penalty_all <- mean(bmae(mae_midwidth), na.rm = TRUE)
    rho_penalty_all <- mean(bound01(rho_abs), na.rm = TRUE)
    
    sharp_med <- sharpness_rel[q == q_med][1]
    sharpness_penalty <- if (is.null(sharp_ref)) {
      1 - exp(-pmax(sharp_med, 0))
    } else {
      bound01(pmax(sharp_med, 0) / pmax(sharp_ref, 1e-8))
    }
    
    is_avg <- mean(interval_score, na.rm = TRUE)
    md_scale <- md_abs_median[q == q_med][1]
    interval_norm <- is_avg / pmax(md_scale, 1e-8)
    interval_penalty <- if (is.null(interval_ref)) {
      1 - exp(-pmax(interval_norm, 0))
    } else {
      bound01(pmax(is_avg, 0) / pmax(interval_ref, 1e-8))
    }
    
    qpc_score <- w["med_cov"]  * coverage_penalty_med +
      w["tail_cov"] * coverage_penalty_tails +
      w["mae"]      * mae_penalty_all +
      w["drift"]    * rho_penalty_all +
      w["sharp"]    * sharpness_penalty +
      w["interval"] * interval_penalty
    
    .(
      qpc_score = qpc_score,
      coverage_penalty_med = coverage_penalty_med,
      coverage_penalty_tails = coverage_penalty_tails,
      mae_penalty_all = mae_penalty_all,
      rho_penalty_all = rho_penalty_all,
      sharpness_penalty = sharpness_penalty,
      interval_penalty = interval_penalty,
      interval_score_avg = is_avg,
      interval_norm = interval_norm
    )
  }, by = by_strat]
  
  # ---- Wide diagnostics by quantile curve (per stratum) ----
  to_wide <- copy(per_q)
  # keep only columns we want to pivot
  id_cols <- unique(c(strat_cols, "qkey"))
  measure_cols <- setdiff(names(to_wide), c(strat_cols, "q", "qkey", "rho_abs", ".qpc_group"))
  long <- data.table::melt(to_wide,
                           id.vars = id_cols,
                           measure.vars = measure_cols,
                           variable.name = "feat",
                           value.name = "val")
  long[, name := paste0(feat, "_", qkey)]
  
  if (length(strat_cols) == 0) {
    long[, .qpc_group := 1L]
    wide <- data.table::dcast(long, .qpc_group ~ name, value.var = "val")
    wide[, .qpc_group := NULL]
  } else {
    wide <- data.table::dcast(long, stats::as.formula(paste(paste(strat_cols, collapse = " + "), "~ name")),
                              value.var = "val")
  }

  out <- if (length(strat_cols) == 0) {
    pen <- data.table::copy(penalties)
    pen[, .qpc_group := NULL]
    out0 <- cbind(wide, pen)
    data.table::setDT(out0)
    out0
  } else {
    wide[penalties, on = strat_cols]
  }

  # Add an explicit overall row when stratified
  if (isTRUE(.include_overall) && length(strat_cols) > 0) {
    overall_input <- s[, required, with = FALSE]
    overall_stats <- qc_features_continuous(
      stats = overall_input,
      alpha = alpha,
      w = w,
      sharp_ref = sharp_ref,
      interval_ref = interval_ref,
      .include_overall = FALSE
    )
    overall_stats[, (strat_cols) := "__ALL__"]
    overall_stats[, qpc_scope := "overall"]
    out[, qpc_scope := "stratum"]
    out <- data.table::rbindlist(list(out, overall_stats), fill = TRUE)
  } else {
    out[, qpc_scope := "overall"]
  }

  out[]
}


# Discretize qpc_score to 1..10 with fixed anchors (lower = better).
# anchors = c(best_score_anchor, worst_score_anchor)
qc_discrete_1to10 <- function(score, anchors = c(0.05, 0.5)) {
  stopifnot(length(anchors) == 2)
  a <- min(anchors); b <- max(anchors)
  if (!is.finite(a) || !is.finite(b) || a == b) return(rep(1L, length(score)))
  
  # Build k-1 internal boundaries evenly between anchors
  k <- 10L
  cuts <- seq(a, b, length.out = k - 1)
  
  # Map using intervals; clamp outside anchors
  bin <- findInterval(score, cuts, left.open = TRUE) + 1L
  pmin(pmax(bin, 1L), k)
}
