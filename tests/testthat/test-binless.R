test_that("cont vpc binless vpcstats are correct", {
  obs_data <- tidyvpc::obs_data
  sim_data <- tidyvpc::sim_data
  
  ## Subest MDV = 0
  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]
  
  vpc <- observed(obs_data, x = TIME, y = DV )
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- binless(vpc)
  vpc <- suppressWarnings(vpcstats(vpc))
  
  location=system.file("extdata/Binless","stats.csv",package="tidyvpc")
  
  stats <- fread(location, colClasses = c(qname = "factor"))
  
  #Check for equality, dispatches to data.table::all.equal method
  testthat::expect_identical(all.equal(vpc$stats, stats), TRUE)
  
})


test_that("cont vpc binless stratification vpcstats are correct", {
  obs_data <- tidyvpc::obs_data
  sim_data <- tidyvpc::sim_data
  
  ## Subest MDV = 0
  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]
  
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

test_that("cat vpc binless vpcstats are correct", {
  obs_cat_data <- tidyvpc::obs_cat_data
  sim_cat_data <- tidyvpc::sim_cat_data


  vpc <- observed(obs_cat_data, x = agemonths, y = zlencat )
  vpc <- simulated(vpc, sim_cat_data, y = DV)
  vpc <- binless(vpc)
  vpc <- suppressWarnings(vpcstats(vpc, vpc.type = "categorical"))

  location=system.file("extdata/Binless","cat_stats.csv",package="tidyvpc")

  stats <- fread(location, colClasses = c(pname = "factor"))
  setkeyv(stats, c("x"))
  

  #Check for equality, dispatches to data.table::all.equal method
  testthat::expect_identical(all.equal(vpc$stats, stats), TRUE)

})


test_that("cat vpc binless stratification vpcstats are correct", {
  obs_cat_data <- tidyvpc::obs_cat_data
  sim_cat_data <- tidyvpc::sim_cat_data
  
  
  vpc <- observed(obs_cat_data, x = agemonths, y = zlencat )
  vpc <- simulated(vpc, sim_cat_data, y = DV)
  vpc <- stratify(vpc, ~ Country_ID_code)
  vpc <- binless(vpc)
  vpc <- suppressWarnings(vpcstats(vpc, vpc.type = "categorical"))
  
  location=system.file("extdata/Binless","cat_strat_stats.csv",package="tidyvpc")
  
  stats <- fread(location, colClasses = c(pname = "factor"))
  setkeyv(stats, c(names(vpc$strat), "x"))
  

  #Check for equality, dispatches to data.table::all.equal method
  testthat::expect_identical(all.equal(vpc$stats, stats), TRUE)

})