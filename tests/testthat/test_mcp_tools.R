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
