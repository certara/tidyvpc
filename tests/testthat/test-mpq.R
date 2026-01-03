test_that("mpqstats errors when stats are missing", {
  o <- structure(list(), class = "tidyvpcobj")
  expect_error(
    mpqstats(o),
    regexp = "`o\\$stats` is missing. Run `vpcstats\\(\\)` before `mpqstats\\(\\)`\\.",
    fixed = FALSE
  )
})

test_that("mpqstats errors for categorical VPCs", {
  skip_on_cran()
  obs_cat_data <- tidyvpc::obs_cat_data
  sim_cat_data <- tidyvpc::sim_cat_data

  vpc <- observed(obs_cat_data, x = agemonths, y = zlencat)
  vpc <- simulated(vpc, sim_cat_data, y = DV)
  vpc <- binless(vpc)
  vpc <- suppressWarnings(vpcstats(vpc, vpc.type = "categorical"))

  expect_error(
    mpqstats(vpc),
    regexp = "`mpqstats\\(\\)` is currently supported for continuous VPCs only\\.",
    fixed = FALSE
  )
})

test_that("mpqstats works for continuous binless VPCs", {
  skip_on_cran()
  obs_data <- tidyvpc::obs_data[MDV == 0]
  sim_data <- tidyvpc::sim_data[MDV == 0]

  vpc <- observed(obs_data, x = TIME, y = DV)
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- binless(vpc)
  vpc <- suppressWarnings(vpcstats(vpc))

  vpc <- mpqstats(vpc)
  expect_true(data.table::is.data.table(vpc$mpq.stats))
  expect_true("qc_score" %in% names(vpc$mpq.stats))
  expect_true(any(is.finite(vpc$mpq.stats$qc_score)))
})

test_that("mpqstats works for stratified continuous binless VPCs", {
  skip_on_cran()
  obs_data <- tidyvpc::obs_data[MDV == 0]
  sim_data <- tidyvpc::sim_data[MDV == 0]

  vpc <- observed(obs_data, x = TIME, y = DV)
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- stratify(vpc, ~GENDER)
  vpc <- binless(vpc)
  vpc <- suppressWarnings(vpcstats(vpc))

  vpc <- mpqstats(vpc)
  expect_true(data.table::is.data.table(vpc$mpq.stats))
  expect_true(nrow(vpc$mpq.stats) >= 2) # strata rows + overall
  expect_true(all(c("qc_score", "mpq_scope") %in% names(vpc$mpq.stats)))
})

test_that("mpqstats works for predcorrect continuous binless VPCs", {
  skip_on_cran()
  obs_data <- tidyvpc::obs_data[MDV == 0]
  sim_data <- tidyvpc::sim_data[MDV == 0]

  sim_data <- sim_data[REP %% 2 == 1]
  obs_data$PRED <- sim_data[REP == 1, PRED]

  vpc <- observed(obs_data, x = TIME, y = DV)
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- predcorrect(vpc, pred = PRED)
  vpc <- binless(vpc)
  vpc <- suppressWarnings(vpcstats(vpc))

  vpc <- mpqstats(vpc)
  expect_true(any(is.finite(vpc$mpq.stats$qc_score)))
})

test_that("mpqstats works with censoring (continuous binless)", {
  skip_on_cran()
  obs_data <- tidyvpc::obs_data[MDV == 0]
  sim_data <- tidyvpc::sim_data[MDV == 0]

  obs_data$LLOQ <- 50
  vpc <- observed(obs_data, x = TIME, y = DV)
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- censoring(vpc, blq = (DV < LLOQ), lloq = LLOQ)
  vpc <- binless(vpc)
  vpc <- suppressWarnings(vpcstats(vpc))

  vpc <- mpqstats(vpc)
  expect_true(any(is.finite(vpc$mpq.stats$qc_score)))
})

test_that("mpqstats works for continuous binning VPCs", {
  skip_on_cran()
  obs_data <- tidyvpc::obs_data[MDV == 0]
  sim_data <- tidyvpc::sim_data[MDV == 0]

  vpc <- observed(obs_data, x = TIME, y = DV) %>%
    simulated(sim_data, y = DV) %>%
    binning(bin = NTIME) %>%
    vpcstats()

  vpc <- mpqstats(vpc)
  expect_true(any(is.finite(vpc$mpq.stats$qc_score)))
})

test_that("mpqstats works for stratified+predcorrect continuous binning VPCs", {
  skip_on_cran()
  obs_data <- tidyvpc::obs_data[MDV == 0]
  sim_data <- tidyvpc::sim_data[MDV == 0]

  obs_data$PRED <- sim_data[REP == 1, PRED]

  vpc <- observed(obs_data, x = TIME, y = DV) %>%
    simulated(sim_data, y = DV) %>%
    stratify(~GENDER) %>%
    predcorrect(pred = PRED) %>%
    binning(bin = NTIME) %>%
    vpcstats()

  vpc <- mpqstats(vpc)
  expect_true(any(is.finite(vpc$mpq.stats$qc_score)))
})

test_that("mpqstats deterministic: variance inflation should not improve qc_score", {
  stats0 <- data.table::data.table(
    x = rep(1:5, 3),
    qname = factor(rep(c("q0.05", "q0.5", "q0.95"), each = 5)),
    y = 10,
    md = 10,
    lo = 9,
    hi = 11
  )
  o0 <- structure(list(stats = stats0), class = "tidyvpcobj")
  o0 <- mpqstats(o0)
  qc0 <- o0$mpq.stats[mpq_scope == "overall", qc_score][1]
  expect_true(is.finite(qc0))

  # Inflate intervals massively (still perfect coverage)
  stats1 <- data.table::copy(stats0)
  stats1[, `:=`(lo = 0, hi = 20)]
  o1 <- structure(list(stats = stats1), class = "tidyvpcobj")
  o1 <- mpqstats(o1)
  qc1 <- o1$mpq.stats[mpq_scope == "overall", qc_score][1]
  expect_true(is.finite(qc1))

  expect_gte(qc1, qc0)
})

test_that("mpqstats deterministic: monotonic drift increases drift penalty", {
  stats_base <- data.table::data.table(
    x = rep(1:10, 3),
    qname = factor(rep(c("q0.05", "q0.5", "q0.95"), each = 10)),
    md = 10,
    lo = 9,
    hi = 11
  )
  stats_base[, y := md]
  o_base <- structure(list(stats = stats_base), class = "tidyvpcobj")
  o_base <- mpqstats(o_base)
  rho0 <- o_base$mpq.stats[mpq_scope == "overall", rho_penalty_all][1]

  stats_drift <- data.table::copy(stats_base)
  stats_drift[, y := md + x / 10] # increasing bias vs x
  o_drift <- structure(list(stats = stats_drift), class = "tidyvpcobj")
  o_drift <- mpqstats(o_drift)
  rho1 <- o_drift$mpq.stats[mpq_scope == "overall", rho_penalty_all][1]

  expect_true(is.finite(rho0) && is.finite(rho1))
  expect_gte(rho1, rho0)
})

test_that("mpqstats deterministic: qname formatting q0.5 vs q0.50 is tolerated", {
  stats0 <- data.table::data.table(
    x = rep(1:5, 3),
    qname = factor(rep(c("q0.05", "q0.50", "q0.95"), each = 5)),
    y = 10,
    md = 10,
    lo = 9,
    hi = 11
  )
  stats1 <- data.table::copy(stats0)
  stats1[qname == "q0.50", qname := "q0.5"]
  stats1[, qname := factor(as.character(qname))]

  o <- structure(list(stats = stats1), class = "tidyvpcobj")
  o <- mpqstats(o)
  expect_true(any(grepl("_q0\\.50$", names(o$mpq.stats))))
  expect_true(is.finite(o$mpq.stats[mpq_scope == "overall", qc_score][1]))
})





