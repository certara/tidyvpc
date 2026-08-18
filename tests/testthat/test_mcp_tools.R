# MCP tools builder and handlers (standalone; Certara.R host is optional).

test_that("tidyvpc_mcp_tools returns tools when ellmer is installed", {
  skip_if_not_installed("ellmer")
  tools <- tidyvpc_mcp_tools()
  expect_gt(length(tools), 0L)
  nms <- vapply(tools, function(t) t@name, character(1))
  expect_true(all(c("tidyvpc_load_from_rds", "tidyvpc_build_vpc",
                    "tidyvpc_plot_vpc") %in% nms))
})

test_that("build and plot from obs/sim data", {
  skip_if_not_installed("ellmer")
  .tidyvpc_state_reset()
  root <- file.path(tempdir(), "tidyvpc_plot_test")
  dir.create(root, showWarnings = FALSE, recursive = TRUE)
  on.exit({
    unlink(root, recursive = TRUE)
    .tidyvpc_state_reset()
  }, add = TRUE)
  obs <- tidyvpc::obs_data[tidyvpc::obs_data$MDV == 0, ]
  sim <- tidyvpc::sim_data[tidyvpc::sim_data$MDV == 0, ]
  h <- .tidyvpc_store(list(obs = obs, sim = sim, vpc_obj = NULL))
  built <- .tv_build_vpc(h, x_col = "TIME", yobs_col = "DV", ysim_col = "DV",
                         bin_col = "NTIME", nbins = 6L)
  expect_equal(built$handle, h)
  res <- .tv_plot_vpc(h, out_dir = root)
  expect_true(file.exists(res$plot_path))
  expect_true("report_rmd" %in% names(res))
  if (!.certara_mcp_host()) {
    expect_true(is.na(res$repro_script) || !nzchar(res$repro_script))
  }
})

test_that("qpc_score computes an opt-in score on a binless VPC", {
  skip_if_not_installed("ellmer")
  .tidyvpc_state_reset()
  on.exit(.tidyvpc_state_reset(), add = TRUE)
  obs <- tidyvpc::obs_data[tidyvpc::obs_data$MDV == 0, ]
  sim <- tidyvpc::sim_data[tidyvpc::sim_data$MDV == 0, ]
  h <- .tidyvpc_store(list(obs = obs, sim = sim, vpc_obj = NULL))
  .tv_build_vpc(h, x_col = "TIME", yobs_col = "DV", ysim_col = "DV",
               binless = TRUE)

  res <- .tv_qpc_score(h)
  expect_equal(res$handle, h)
  expect_true(is.numeric(res$qpc_score) && is.finite(res$qpc_score))
  expect_equal(res$overall$qpc_scope, "overall")
  expect_null(res$strata)
  expect_equal(res$weights$med_cov, 0.35)
  expect_null(res$sharp_ref)
  expect_match(res$note, "never an automatic acceptance gate")

  res2 <- .tv_qpc_score(h, w_med_cov = 0.5, sharp_ref = 0.15, interval_ref = 2.5)
  expect_equal(res2$weights$med_cov, 0.5)
  expect_equal(res2$sharp_ref, 0.15)
})

test_that("qpc_score requires a built vpc object", {
  .tidyvpc_state_reset()
  on.exit(.tidyvpc_state_reset(), add = TRUE)
  h <- .tidyvpc_store(list(obs = data.frame(), sim = data.frame(), vpc_obj = NULL))
  expect_error(.tv_qpc_score(h), "Build a VPC first")
})

test_that("tidyvpc_mcp_tools exposes qpc_score only for stats/full groups", {
  skip_if_not_installed("ellmer")
  all_tools <- tidyvpc_mcp_tools()
  nms_all <- vapply(all_tools, function(t) t@name, character(1))
  expect_true("qpc_score" %in% nms_all)

  scoped <- tidyvpc_mcp_tools(groups = c("data", "build", "plot", "meta"))
  nms_scoped <- vapply(scoped, function(t) t@name, character(1))
  expect_false("qpc_score" %in% nms_scoped)
})

test_that("load_from_rds attaches PRED from sim replicate 0 when observed lacks it", {
  .tidyvpc_state_reset()
  obs <- as.data.frame(tidyvpc::obs_data[tidyvpc::obs_data$MDV == 0, ])
  sim <- as.data.frame(tidyvpc::sim_data[tidyvpc::sim_data$MDV == 0, ])
  expect_false("PRED" %in% names(obs))
  expect_true("PRED" %in% names(sim))
  # Prefer RsNLME-style replicate 0 when present; also exercise the
  # first-replicate fallback used by tidyvpc::sim_data (REP starts at 1).
  path0 <- tempfile(fileext = ".rds")
  sim0 <- sim
  sim0$REP[sim0$REP == 1L] <- 0L
  saveRDS(list(predcheck0 = obs, predout = sim0), path0)
  path1 <- tempfile(fileext = ".rds")
  saveRDS(list(predcheck0 = obs, predout = sim), path1)
  on.exit({
    unlink(c(path0, path1))
    .tidyvpc_state_reset()
  }, add = TRUE)

  res0 <- .tv_load_from_rds(path0)
  expect_true(isTRUE(res0$pred_available))
  expect_equal(res0$pred_source, "sim_replicate_0")
  expect_equal(res0$pred_col, "PRED")
  sess0 <- .tidyvpc_get(res0$handle)
  expect_true("PRED" %in% names(sess0$obs))
  expect_true(any(!is.na(sess0$obs$PRED)))

  res1 <- .tv_load_from_rds(path1)
  expect_true(isTRUE(res1$pred_available))
  expect_equal(res1$pred_source, "sim_replicate_1")
  expect_equal(res1$pred_col, "PRED")
})

test_that("predcorrect auto-selects PRED and returns pred_quality", {
  .tidyvpc_state_reset()
  on.exit(.tidyvpc_state_reset(), add = TRUE)
  obs <- as.data.frame(tidyvpc::obs_data[tidyvpc::obs_data$MDV == 0, ])
  sim <- as.data.frame(tidyvpc::sim_data[tidyvpc::sim_data$MDV == 0, ])
  path <- tempfile(fileext = ".rds")
  saveRDS(list(predcheck0 = obs, predout = sim), path)
  on.exit(unlink(path), add = TRUE)
  res <- .tv_load_from_rds(path)
  built <- .tv_build_vpc(res$handle, x_col = "TIME", yobs_col = "DV",
                         ysim_col = "DV", bin_col = "NTIME", nbins = 6L,
                         predcorrect = TRUE)
  expect_equal(built$pred_col, "PRED")
  expect_true(is.list(built$pred_quality))
  expect_true(is.numeric(built$pred_quality$fraction_near_zero))
  expect_equal(built$pred_quality$epsilon, .tv_pred_near_zero_epsilon)
})

test_that("predcorrect without PRED errors clearly at the MCP layer", {
  .tidyvpc_state_reset()
  on.exit(.tidyvpc_state_reset(), add = TRUE)
  obs <- data.frame(ID = 1L, TIME = 0, DV = 1, NTIME = 0)
  sim <- data.frame(ID = 1L, REP = 1L, TIME = 0, DV = 1)
  h <- .tidyvpc_store(list(obs = obs, sim = sim, vpc_obj = NULL))
  expect_error(.tv_build_vpc(h, predcorrect = TRUE), "no PRED column")
})

test_that("load_from_rds creates a session handle", {
  .tidyvpc_state_reset()
  obs <- tidyvpc::obs_data[tidyvpc::obs_data$MDV == 0, ]
  sim <- tidyvpc::sim_data[tidyvpc::sim_data$MDV == 0, ]
  path <- tempfile(fileext = ".rds")
  saveRDS(list(predcheck0 = obs, predout = sim), path)
  on.exit({
    unlink(path)
    .tidyvpc_state_reset()
  }, add = TRUE)
  res <- .tv_load_from_rds(path)
  expect_equal(res$handle, "vpc1")
  expect_equal(res$observed$n_rows, nrow(obs))
  expect_equal(res$simulated$n_rows, nrow(sim))
})

test_that("Certara.R host records repro and report when MCP API is available", {
  skip_if_not(.certara_mcp_host(), "Certara.R MCP host API not available")
  skip_if_not_installed("ellmer")
  .tidyvpc_state_reset()
  .certara_mcp_reset()
  root <- file.path(tempdir(), "tidyvpc_host_test")
  project_dir <- .certara_mcp_fn("mcp_session_project_dir")
  if (!is.null(project_dir)) project_dir(root)
  on.exit({
    unlink(root, recursive = TRUE)
    .tidyvpc_state_reset()
    .certara_mcp_reset()
  }, add = TRUE)
  obs <- tidyvpc::obs_data[tidyvpc::obs_data$MDV == 0, ]
  sim <- tidyvpc::sim_data[tidyvpc::sim_data$MDV == 0, ]
  path <- tempfile(fileext = ".rds")
  saveRDS(list(predcheck0 = obs, predout = sim), path)
  on.exit(unlink(path), add = TRUE)
  res <- .tv_load_from_rds(path)
  txt <- .certara_mcp_fn("mcp_repro_read")()
  expect_match(txt, "obs <- vpc_job$predcheck0", fixed = TRUE)
  expect_match(txt, "sim <- if (!is.null(vpc_job$predout))", fixed = TRUE)
  expect_silent(parse(text = txt))
  h <- res$handle
  .tv_build_vpc(h, x_col = "TIME", yobs_col = "DV", ysim_col = "DV",
                bin_col = "NTIME", nbins = 6L)
  .tv_plot_vpc(h, out_dir = root)
  txt <- .certara_mcp_fn("mcp_repro_read")()
  expect_match(txt, "vpcstats", fixed = FALSE)
  report <- .certara_mcp_fn("mcp_report_read")()
  expect_match(report, "include_graphics", fixed = FALSE)
})

test_that("shuffled simulated rows yield the same vpcstats after sort", {
  .tidyvpc_state_reset()
  on.exit(.tidyvpc_state_reset(), add = TRUE)
  obs <- as.data.frame(tidyvpc::obs_data[tidyvpc::obs_data$MDV == 0, ])
  sim <- as.data.frame(tidyvpc::sim_data[tidyvpc::sim_data$MDV == 0, ])
  set.seed(42)
  sim_shuf <- sim[sample.int(nrow(sim)), , drop = FALSE]
  rownames(sim_shuf) <- NULL

  h1 <- .tidyvpc_store(list(obs = obs, sim = sim, vpc_obj = NULL))
  h2 <- .tidyvpc_store(list(obs = obs, sim = sim_shuf, vpc_obj = NULL))
  .tv_build_vpc(h1, x_col = "TIME", yobs_col = "DV", ysim_col = "DV",
                bin_col = "NTIME", nbins = 6L)
  .tv_build_vpc(h2, x_col = "TIME", yobs_col = "DV", ysim_col = "DV",
                bin_col = "NTIME", nbins = 6L)
  s1 <- as.data.frame(.tidyvpc_get(h1)$vpc_obj$stats)
  s2 <- as.data.frame(.tidyvpc_get(h2)$vpc_obj$stats)
  expect_equal(s1, s2, tolerance = 1e-8)
})

test_that("multi-ObsName tables warn instead of mixing endpoints", {
  .tidyvpc_state_reset()
  on.exit(.tidyvpc_state_reset(), add = TRUE)
  obs <- as.data.frame(tidyvpc::obs_data[tidyvpc::obs_data$MDV == 0, ])
  sim <- as.data.frame(tidyvpc::sim_data[tidyvpc::sim_data$MDV == 0, ])
  obs$ObsName <- ifelse(as.integer(obs$ID) %% 2L == 0L, "CObs", "EObs")
  sim$ObsName <- ifelse(as.integer(sim$ID) %% 2L == 0L, "CObs", "EObs")
  expect_match(.tv_obsname_warning(obs, sim), "multiple values")

  h <- .tidyvpc_store(list(obs = obs, sim = sim, vpc_obj = NULL))
  built <- .tv_build_vpc(h, x_col = "TIME", yobs_col = "DV", ysim_col = "DV",
                         bin_col = "NTIME", nbins = 6L)
  expect_true(any(grepl("ObsName has multiple values", unlist(built$warnings))))
})
