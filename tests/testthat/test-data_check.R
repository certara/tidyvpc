test_that("obs and sim data length checks", {
  obs_data <- as.data.table(tidyvpc::obs_data)
  sim_data <- as.data.table(tidyvpc::sim_data)

  ## Subest MDV = 0
  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]
  
  #Assign observed and simulated data to tidyvpc object
  vpc <- observed(obs_data, x = TIME, y = DV )
  
  vpc <- simulated(vpc, sim_data, y = DV)
  
  #Check for column length
  expect_length(obs_data, length(vpc$data))
  
  #Check for row length
  expect_equal(nrow(obs_data), nrow((vpc$data)))
  
  
  
  
})
