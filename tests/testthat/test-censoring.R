test_that("censoring output is valid", {
  
  #The below are a tests that check for valid stratification operations and blq/alq output
  obs_data <- as.data.table(tidyvpc::obs_data)
  sim_data <- as.data.table(tidyvpc::sim_data)
  
  # Subest MDV = 0
  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]
  
  # Add LLOQ
  obs_data$LLOQ <- obs_data[, ifelse(STUDY == "Study A", 50, 25)]
  
  #Assign observed and simulated data to tidyvpc object
  vpc <- observed(obs_data, x = TIME, y = DV )
  
  vpc <- simulated(vpc, sim_data, y = DV)
  
  vpc <- censoring(vpc, blq = DV < LLOQ, lloq = LLOQ)
  
  vpc <- stratify(vpc, ~STUDY) 
  
  vpc <- binning(vpc, bin = NTIME) 
    
  vpc <- vpcstats(vpc, qpred = c(0.1, 0.5, 0.9))
  
  #Generate logical for blq in observed data
  obs_strat_blq <- obs_data[, num_blq := ifelse(DV < LLOQ, TRUE, FALSE), by = STUDY]
  #Check sum of TRUE logicals in obs_strat_
  testthat::expect_equal(sum(vpc$obs$blq), sum(obs_strat_blq$num_blq))
  
  vpc <- observed(obs_data, x = TIME, y = DV )
  
  vpc <- simulated(vpc, sim_data, y = DV)
  
  vpc <- censoring(vpc, alq = DV > LLOQ, uloq = LLOQ)
  
  vpc <- stratify(vpc, ~STUDY) 
  
  vpc <- binning(vpc, bin = NTIME) 
  
  vpc <- vpcstats(vpc, qpred = c(0.1, 0.5, 0.9))
  
  #Generate logical for blq in observed data
  obs_strat_alq <- obs_data[, num_alq := ifelse(DV > LLOQ, TRUE, FALSE), by = STUDY]
  #Check sum of TRUE logicals in obs_strat_
  testthat::expect_equal(sum(vpc$obs$alq), sum(obs_strat_alq$num_alq))
  
})
