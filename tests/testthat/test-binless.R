get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

test_that("cont vpc binless vpcstats are correct", {
  skip_on_cran()

  obs_data <- tidyvpc::obs_data
  sim_data <- tidyvpc::sim_data

  ## Subest MDV = 0
  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]

  vpc <- observed(obs_data, x = TIME, y = DV )
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- binless(vpc)
  vpc <- suppressWarnings(vpcstats(vpc))


  os <- get_os()

  if(os == "windows"){
    location <-system.file("extdata/Binless","stats.csv",package="tidyvpc")
  } else {
    location <-system.file("extdata/Binless","stats_l.csv",package="tidyvpc")
  }

  stats <- fread(location, colClasses = c(qname = "factor"))

  #str(stats)

  #str(vpc$stats)
  #Check for equality, dispatches to data.table::all.equal method
  expect_equal(
    vpc$stats,
    stats,
    tolerance = 0.003
  )
})

test_that("cont vpc binless stratification vpcstats are correct", {
  skip_on_cran()

  obs_data <- tidyvpc::obs_data
  sim_data <- tidyvpc::sim_data

  ## Subest MDV = 0
  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]

  vpc <- observed(obs_data, x = TIME, y = DV )
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- stratify(vpc, ~ GENDER + STUDY)
  vpc <- binless(vpc)
  vpc <- suppressWarnings(vpcstats(vpc))

  os <- get_os()

  if(os == "windows"){
    location <-system.file("extdata/Binless","strat_stats.csv",package="tidyvpc")
  } else {
    location <-system.file("extdata/Binless","strat_stats_l.csv",package="tidyvpc")
  }

  stats <- fread(location, colClasses = c(qname = "factor"))

  #Check for equality, dispatches to data.table::all.equal method
  expect_equal(
    vpc$stats,
    stats,
    tolerance = 0.001
  )
})

test_that("cont vpc predcorrect binless vpcstats are correct", {
  skip_on_cran()

  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0 ]
  sim_data <- sim_data[REP %% 2 == 1]
  obs_data$PRED <- sim_data[REP == 1, PRED]
  vpc <- observed(obs_data, x=TIME, y=DV)
  vpc <- simulated(vpc, sim_data, y=DV)
  vpc <- predcorrect(vpc, pred = PRED)
  vpc <- binless(vpc)
  vpc <- suppressWarnings(vpcstats(vpc))

  os <- get_os()

  if(os == "windows"){
    location <-system.file("extdata/Binless","predcor_stats.csv",package="tidyvpc")
  } else {
    location <-system.file("extdata/Binless","predcor_stats_l.csv",package="tidyvpc")
  }

  stats <- fread(location, colClasses = c(qname = "factor"))

  expect_equal(vpc$stats, 
               stats, 
               tolerance = 0.003)
})

test_that("cont vpc binless censoring vpcstats are correct", {
  skip_on_cran()
  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]

  obs_data$LLOQ <- 50

  vpc <- observed(obs_data, x = TIME, y = DV )
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- censoring(vpc, blq=(DV < LLOQ), lloq=LLOQ)
  vpc <- binless(vpc)
  vpc <- suppressWarnings(vpcstats(vpc))
  expect_true(any(is.na(vpc$stats$y)))

  obs_data$LLOQ <- ifelse(obs_data$GENDER == "M", 50, 25)

  vpc <- observed(obs_data, x = TIME, y = DV )
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- censoring(vpc, blq=(DV < LLOQ), lloq=LLOQ)
  vpc <- stratify(vpc, ~ GENDER)
  vpc <- binless(vpc)

  vpc <- suppressWarnings(vpcstats(vpc))
  expect_true(any(is.na(vpc$stats$y)) && !is.null(vpc$stats$GENDER))
})

test_that("cat vpc binless vpcstats are correct", {
  skip_on_cran()
  obs_cat_data <- tidyvpc::obs_cat_data
  sim_cat_data <- tidyvpc::sim_cat_data

  vpc <- observed(obs_cat_data, x = agemonths, y = zlencat )
  vpc <- simulated(vpc, sim_cat_data, y = DV)
  vpc <- binless(vpc)
  vpc <- suppressWarnings(vpcstats(vpc, vpc.type = "categorical"))

  location <-system.file("extdata/Binless","cat_stats.csv",package="tidyvpc")

  stats <- fread(location, colClasses = c(pname = "factor"))
  setkeyv(stats, c("x"))

  #Check for equality, dispatches to data.table::all.equal method
  expect_equal(vpc$stats, stats)
})

test_that("cat vpc binless stratification vpcstats are correct", {
  skip_on_cran()
  obs_cat_data <- tidyvpc::obs_cat_data
  sim_cat_data <- tidyvpc::sim_cat_data

  vpc <- observed(obs_cat_data, x = agemonths, y = zlencat )
  vpc <- simulated(vpc, sim_cat_data, y = DV)
  vpc <- stratify(vpc, ~ Country_ID_code)
  vpc <- binless(vpc)
  vpc <- suppressWarnings(vpcstats(vpc, vpc.type = "categorical"))

  location <-system.file("extdata/Binless","cat_strat_stats.csv",package="tidyvpc")

  stats <- fread(location, colClasses = c(pname = "factor"))
  setkeyv(stats, c(names(vpc$strat), "x"))


  #Check for equality, dispatches to data.table::all.equal method
  expect_equal(vpc$stats, stats)
})


test_that("binless.tidyvpcobj returns correct errors and warnings", {
  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]
  obs_data$PRED <- sim_data[REP == 1, PRED]
  vpc <- observed(obs_data, x=TIME, y=DV)
  vpc <- simulated(vpc, sim_data, y=DV)

  # setting optmize = FALSE without specifying lambda or sp
  expect_error(binless(vpc, optimize = FALSE),
               regexp = "Set optimize = TRUE if no lambda or sp arguments specified",
               fixed = TRUE)
  
  #loess.ypc argument is deprecated
  expect_warning(binless(vpc, loess.ypc = TRUE),
                 regexp = "The loess.ypc argument is deprecated and will be ignored. Usage of `binless()` with `predcorrect()` will now perform LOESS prediction corrected VPC by default.",
                 fixed = TRUE)
  
})

test_that("binless.tidyvpcobj uses supplied lambda and span if optimize = FALSE", {
  # continuous VPC
  skip_on_cran()
  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]
  obs_data$PRED <- sim_data[REP == 1, PRED]
  vpc <- observed(obs_data, x=TIME, y=DV)
  vpc <- simulated(vpc, sim_data, y=DV)
  vpc <- stratify(vpc, ~ GENDER)
  
  user_lambda <- data.frame(GENDER_F = c(2,4,2), GENDER_M = c(1.9,3,2.25))
  
  vpc <- binless(vpc, lambda = user_lambda, span = c(0.5, 0.8))
  vpc <- predcorrect(vpc, pred = PRED)
  
  expect_true(vpc$vpc.method$loess.ypc)
  expect_equal(vpc$vpc.method$lambda, user_lambda)
  expect_equal(vpc$vpc.method$span, c(0.5, 0.8))
  
  vpc <- suppressWarnings(vpcstats(vpc))
  expect_s3_class(vpc, "tidyvpcobj")
})


