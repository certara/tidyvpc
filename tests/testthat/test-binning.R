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
  testthat::expect_equal(unique_bins_obs, unique_bins_vpc)
  
})


test_that("cat obs vpcstats is correct", {
  obs_cat_data <- as.data.table(tidyvpc::obs_cat_data)
  sim_cat_data <- as.data.table(tidyvpc::sim_cat_data)
  
  vpc <- observed(obs_cat_data, x = agemonths, y = zlencat )
  vpc <- simulated(vpc, sim_cat_data, y = DV)
  vpc <- binning(vpc, bin = round(agemonths, 0))
  vpc <- vpcstats(vpc, vpc.type = "categorical")
  
  location=system.file("extdata/Binning","cat_stats.csv",package="tidyvpc")
  
  stats <- fread(location, colClasses = c(pname = "factor"))
  stats$bin <- as.factor(stats$bin)
  
  setkeyv(stats, c("xbin"))
  
  
  #Check for equality, dispatches to data.table::all.equal method
  testthat::expect_identical(all.equal(vpc$stats, stats), TRUE)

})



test_that("cat obs strat vpcstats is correct", {
  obs_cat_data <- as.data.table(tidyvpc::obs_cat_data)
  sim_cat_data <- as.data.table(tidyvpc::sim_cat_data)
  
  vpc <- observed(obs_cat_data, x = agemonths, y = zlencat )
  vpc <- simulated(vpc, sim_cat_data, y = DV)
  vpc <- stratify(vpc, ~ Country_ID_code)
  vpc <- binning(vpc, bin = round(agemonths, 0))
  vpc <- vpcstats(vpc, vpc.type = "categorical")
  
  location=system.file("extdata/Binning","cat_strat_stats.csv",package="tidyvpc")
  
  stats <- fread(location, colClasses = c(pname = "factor"))
  stats$bin <- as.factor(stats$bin)

  setkeyv(stats, c(names(vpc$strat), "xbin"))
  
  
  #Check for equality, dispatches to data.table::all.equal method
  testthat::expect_identical(all.equal(vpc$stats, stats), TRUE)
  
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
