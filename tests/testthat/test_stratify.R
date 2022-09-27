test_that("stratification formulas work", {

  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]
  
  obs_data$DUMMY_COV <- obs_data$ID %% 2
  
  vpc <- observed(obs_data, x = TIME, y = DV )
  
  vpc <- simulated(vpc, sim_data, y = DV)
  
  vpc <- binning(vpc, bin = NTIME)
  
  vpc <- stratify(vpc, DUMMY_COV ~ GENDER + STUDY)
  
  expect_true(length(as.list(vpc$strat.formula)) == 3)

  obs_data$xleft <- obs_data$ID %% 2
  
  vpc <- observed(obs_data, x = TIME, y = DV )
  
  vpc <- simulated(vpc, sim_data, y = DV)
  
  vpc <- binning(vpc, bin = NTIME)
  
  expect_error(stratify(vpc, ~ xleft))
  expect_error(stratify(vpc, NA))
  
  
})
