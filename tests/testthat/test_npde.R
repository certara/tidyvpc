test_that("npde colnames are correct", {
  obs <- obs_data[MDV==0]
  sim <- sim_data[MDV==0]
  
  npde <- observed(obs, x=NULL, y=DV) %>%
    simulated(sim, y=DV) %>%
    npde(id=ID)
  
  testthat::expect_true(
    all(colnames(npde$npdeobs) == c("id", "iter","epred", "eres",  "ewres", "npd", "npde"))
  )
  
  testthat::expect_true(
    all(colnames(npde$npdesim) == c("id", "iter","epred", "eres",  "ewres", "npd", "npde"))
  )

})


test_that("npde results are correct", {
  #skip_on_cran()
  
  obs <- obs_data[MDV==0]
  sim <- sim_data[MDV==0]
  
  npde <- observed(obs, x=NULL, y=DV)
  npde <- simulated(npde, sim, y=DV)
  npde <- npde(npde, id=ID)

  location=system.file("extdata/NPDE","npdeobs.csv", package="tidyvpc")

  stats <- fread(location)
  
  testthat::expect_equal(npde$npdeobs, stats)
  
  vpc <- observed(npde$npdeobs, x=epred, y=npde) 
  vpc <- simulated(vpc, npde$npdesim, y=npde)
  vpc <- binning(vpc, "eqcut", nbins=10)
  vpc <- vpcstats(vpc)
  
  testthat::expect_true(class(vpc)[1]=="tidyvpcobj")
  
  vpc_plot <- tidyvpc:::plot.tidyvpcobj(vpc, point.alpha = 0.25)

  testthat::expect_true(class(vpc_plot)[1]=="gg")
  
})
