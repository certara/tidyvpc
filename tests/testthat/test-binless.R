test_that("binless vpcstats are correct", {
  obs_data <- as.data.table(tidyvpc::obs_data)
  sim_data <- as.data.table(tidyvpc::sim_data)
  
  ## Subest MDV = 0
  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]
  
  #Assign observed and simulated data to tidyvpc object
  vpc <- observed(obs_data, x = TIME, y = DV )
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- binless(vpc)
  vpc <- suppressWarnings(vpcstats(vpc))
  
  location=system.file("extdata/Binless","stats.csv",package="tidyvpc")
  
  stats <- fread(location, colClasses = c(qname = "factor"))
  
  #Check for equality, dispatches to data.table::all.equal method
  testthat::expect_identical(all.equal(vpc$stats, stats), TRUE)
  
})


test_that("binless stratification vpcstats are correct", {
  obs_data <- as.data.table(tidyvpc::obs_data)
  sim_data <- as.data.table(tidyvpc::sim_data)
  
  ## Subest MDV = 0
  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]
  
  #Assign observed and simulated data to tidyvpc object
  vpc <- observed(obs_data, x = TIME, y = DV )
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- stratify(vpc, ~ GENDER + STUDY)
  vpc <- binless(vpc)
  vpc <-  suppressWarnings(vpcstats(vpc))
  
  location=system.file("extdata/Binless","strat_stats.csv",package="tidyvpc")
  
  stats <- fread(location, colClasses = c(qname = "factor"))
  
  #Check for equality, dispatches to data.table::all.equal method
  testthat::expect_identical(all.equal(vpc$stats, stats), TRUE)
  
})