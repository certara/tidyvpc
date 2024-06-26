test_that("obs bins equal stats bins", {
  obs_data <- as.data.table(tidyvpc::obs_data)
  sim_data <- as.data.table(tidyvpc::sim_data)

  ## Subest MDV = 0
  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]

  unique_bins_obs <- as.factor(unique(obs_data$NTIME))
  #Assign observed and simulated data to tidyvpc object
  vpc <- observed(obs_data, x = TIME, y = DV )

  vpc <- simulated(vpc, sim_data, y = DV)

  vpc <- binning(vpc, bin = NTIME)

  vpc <- vpcstats(vpc)

  unique_bins_vpc <- unique(vpc$stats$bin)

  #Check that bins match for binning on xvar NTIME
  expect_equal(unique_bins_obs, unique_bins_vpc)

})


test_that("cat obs vpcstats is correct", {
  obs_cat_data <- as.data.table(tidyvpc::obs_cat_data)
  sim_cat_data <- as.data.table(tidyvpc::sim_cat_data)

  vpc <- observed(obs_cat_data, x = agemonths, y = zlencat )
  vpc <- simulated(vpc, sim_cat_data, y = DV)
  vpc <- binning(vpc, bin = round(agemonths, 0))
  vpc <- vpcstats(vpc, vpc.type = "categorical")

  location <- system.file("extdata/Binning","cat_stats.csv",package="tidyvpc")

  stats <- fread(location, colClasses = c(pname = "factor"))
  stats$bin <- as.factor(stats$bin)

  setkeyv(stats, c("xbin"))


  #Check for equality, dispatches to data.table::all.equal method
  expect_identical(all.equal(vpc$stats, stats), TRUE)

})



test_that("cat obs strat vpcstats is correct", {
  obs_cat_data <- as.data.table(tidyvpc::obs_cat_data)
  sim_cat_data <- as.data.table(tidyvpc::sim_cat_data)

  vpc <- observed(obs_cat_data, x = agemonths, y = zlencat )
  vpc <- simulated(vpc, sim_cat_data, y = DV)
  vpc <- stratify(vpc, ~ Country_ID_code)
  vpc <- binning(vpc, bin = round(agemonths, 0))
  vpc <- vpcstats(vpc, vpc.type = "categorical")

  location <- system.file("extdata/Binning","cat_strat_stats.csv",package="tidyvpc")

  stats <- fread(location, colClasses = c(pname = "factor"))
  stats$bin <- as.factor(stats$bin)

  setkeyv(stats, c(names(vpc$strat), "xbin"))


  #Check for equality, dispatches to data.table::all.equal method
  expect_identical(all.equal(vpc$stats, stats), TRUE)

})

test_that("binning methods are valid", {
  ## Subest MDV = 0
  obs <- obs_data[MDV == 0]
  sim <- sim_data[MDV == 0]

  vpc <- observed(obs, x = TIME, y = DV )
  vpc <- simulated(vpc, sim, y = DV)

  centers <- c(0,1,5,8,12)
  vpc <- binning(vpc, bin = "centers", centers = centers)
  expect_equal(vpc$xbin$bin, as.factor(centers))

  vpc <- binning(vpc, bin = "breaks", breaks = c(1,3,6,9,11))
  expect_true(length(levels(vpc$xbin$bin)) == 11)

  vpc <- binning(vpc, bin = "breaks", breaks = c(1,3,6,9,11))
  expect_true(length(levels(vpc$xbin$bin)) == 11)

  vpc <- binning(vpc, bin = "pam", nbins = 6)
  expect_true(max(vpc$xbin$xbin) < 12)

  vpc <- binning(vpc, bin = "ntile", nbins = 6)
  expect_true(nrow(vpc$xbin) == 6)

  vpc <- binning(vpc, bin = "eqcut", nbins = 12)
  expect_true(nrow(vpc$xbin) == 12)

  vpc <- binning(vpc, bin = "sd", nbins = 4)
  expect_true(nrow(vpc$xbin) == 6)

})


test_that("binning by stratum works", {
  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]
  obs_data$PRED <- sim_data[REP == 1, PRED]

  vpc <- observed(obs_data, x=TIME, y=DV)
  vpc <- simulated(vpc, sim_data, y=DV)
  vpc <- stratify(vpc, ~ GENDER + STUDY)
  vpc <- binning(vpc, stratum = list(GENDER = "M", STUDY = "Study A"), bin = "jenks", nbins = 5, by.strata = T)
  vpc <- binning(vpc, stratum = list(GENDER = "F", STUDY = "Study A"), bin = "centers", centers = c(0.5,3,5,10,15), by.strata = T)
  vpc <- binning(vpc, stratum = list(GENDER = "M", STUDY = "Study B"), bin = "kmeans", by.strata = T)
  vpc <- binning(vpc, stratum = list(GENDER = "F", STUDY = "Study B"), bin = "pam", nbins = 5, by.strata = T)
  vpc <- predcorrect(vpc, pred=PRED)
  vpc <- vpcstats(vpc)

  expect_true(inherits(vpc, "tidyvpcobj") && vpc$bin.by.strata)

})


test_that("binning errors are valid", {

  obs <- obs_data[MDV == 0]
  sim <- sim_data[MDV == 0]

  vpc <- observed(obs, x = TIME, y = DV )
  vpc <- simulated(vpc, sim, y = DV)
  expect_true(inherits(binning(vpc, xbin = NTIME), "tidyvpcobj"))
  expect_error(binning(vpc, xbin = c(1:5)))

})

test_that("binning can be used after predcorrect", {
  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]
  obs_data$PRED <- sim_data[REP == 1, PRED]

  vpc <- observed(obs_data, x = TIME, y = DV )
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- stratify(vpc, ~ GENDER)
  vpc <- predcorrect(vpc, pred = PRED)
  vpc <- binning(vpc, bin = NTIME)
  vpc <- vpcstats(vpc)

  location <- system.file("extdata/Binning","predcor_strat_stats.csv",package="tidyvpc")
  stats <- fread(location, colClasses = c(qname = "factor"))
  stats[, bin := factor(bin, levels = levels(vpc$stats$bin))]
  setkeyv(stats, c(names(vpc$strat), "xbin"))

  expect_equal(vpc$stats, stats)
})

test_that("binning can be used before predcorrect", {
  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]
  obs_data$PRED <- sim_data[REP == 1, PRED]

  vpc <- observed(obs_data, x = TIME, y = DV )
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- stratify(vpc, ~ GENDER)
  vpc <- binning(vpc, bin = NTIME)
  vpc <- predcorrect(vpc, pred = PRED)
  vpc <- vpcstats(vpc)

  location <- system.file("extdata/Binning","predcor_strat_stats.csv",package="tidyvpc")
  stats <- fread(location, colClasses = c(qname = "factor"))
  stats[, bin := factor(bin, levels = levels(vpc$stats$bin))]
  setkeyv(stats, c(names(vpc$strat), "xbin"))

  expect_equal(vpc$stats, stats)
})

test_that("binning works with single-value groups (#51)", {
  d_obs <-
    data.frame(
      group = rep(c("Patient", "Healthy"), each = 5),
      conc = c(rep(0, 5), 1:5),
      value = 1:10
    )

  d_sim <-
    d_obs[rep(1:nrow(d_obs), 5), ]

  value <-
    observed(d_obs, x = conc, yobs = value) %>%
    simulated(d_sim, xsim = conc, ysim = value) %>%
    stratify(~group) %>%
    binning(bin = "jenks")
  expect_s3_class(value, "tidyvpcobj")
})
