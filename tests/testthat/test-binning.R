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
