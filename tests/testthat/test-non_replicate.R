test_that("non-replicate data warnings and errors are given", {
  obs <- tidyvpc::obs_data[MDV == 0]
  sim <- tidyvpc::sim_data[MDV == 0]
  sim$TIME <- sim$TIME + rnorm(1)
  
  vpc <- observed(obs, x = TIME, yobs = DV)
  expect_warning(simulated(vpc, sim, xsim = TIME, ysim = DV),
                 regexp = "Values of `xsim` do not match observed data x-values.")
  
  expect_warning(simulated(vpc, sim[NTIME != 1.50], ysim = DV),
                 regexp = "The simulated data is not a replicate of the observed data. Ensure that you filtered your observed data to remove MDV rows. If this is intentional, use the `xsim` and `repl` arguments for non-replicate support.")
})

test_that("binning works with non-replicate data", {
  obs <- tidyvpc::obs_data[MDV == 0]
  sim <- tidyvpc::sim_data[MDV == 0]
  sim$GENDER <- rep(obs$GENDER, nrow(sim) / nrow(obs))
  sim$STUDY <- rep(obs$STUDY, nrow(sim) / nrow(obs))
  sim_non_rep <- sim[TIME < 10]
  nrep <- nrow(sim_non_rep) / nrow(obs)
  
  expect_false(nrep == as.integer(nrep))
  
  vpc <-
    observed(obs, x = TIME, yobs = DV) %>%
    simulated(sim_non_rep,
              xsim = TIME,
              ysim = DV,
              repl = REP) %>%
    binning(bin = "jenks", nbins = 4)
  obs_bins <- vpc$.stratbin
  sim_bins <- vpc$.stratbinrepl
  expect_equal(unique(obs_bins), unique(sim_bins[!is.na(bin),!c("repl"), with = FALSE]))
  
  vpc <-
    observed(obs, x = TIME, yobs = DV) %>%
    simulated(sim_non_rep,
              xsim = TIME,
              ysim = DV,
              repl = REP) %>%
    stratify( ~ GENDER, data.sim = sim_non_rep) %>%
    binning(bin = "jenks", nbins = 4)
  obs_bins <- vpc$.stratbin
  sim_bins <- vpc$.stratbinrepl
  expect_equal(unique(obs_bins), unique(sim_bins[!is.na(bin),!c("repl"), with = FALSE]))
  
  vpc <-
    observed(obs, x = TIME, yobs = DV) %>%
    simulated(sim_non_rep,
              xsim = TIME,
              ysim = DV,
              repl = REP) %>%
    binning(bin = "centers", centers = c(1, 3, 5, 7, 9)) %>%
    vpcstats()
  expect_s3_class(vpc, "tidyvpcobj")
})
