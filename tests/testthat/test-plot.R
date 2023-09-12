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

test_that("plot.tidyvpcobj plots binning without stats", {
  testthat::skip_if_not(get_os() == "windows")
  testthat::skip_on_cran()

  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]
  obs_data$PRED <- sim_data[REP == 1, PRED]

  vpc <- observed(obs_data, x = TIME, y = DV)
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- binning(vpc, bin = NTIME)

  options(warn = -1)
  vdiffr::expect_doppelganger("Bins without stats",
                              plot(vpc))

  vpc <- predcorrect(vpc, pred = PRED)
  vdiffr::expect_doppelganger("Bins ypc without stats",
                              plot(vpc))
  options(warn = 0)

})


test_that("plot.tidyvpcobj plots censoring", {
  testthat::skip_if_not(get_os() == "windows")
  testthat::skip_on_cran()

  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]
  obs_data$LLOQ <- obs_data[, ifelse(STUDY == "Study A", 50, 25)]
  obs_data$ULOQ <- obs_data[, ifelse(STUDY == "Study A", 125, 100)]

  vpc <- observed(obs_data, x = TIME, y = DV)
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- censoring(vpc, blq = DV < LLOQ, lloq = LLOQ, alq = DV > ULOQ, uloq = ULOQ)
  vpc <-stratify(vpc, ~ STUDY)
  vpc <- binning(vpc, bin = NTIME)
  vpc <- vpcstats(vpc, qpred = c(0.1, 0.5, 0.9))

  options(warn = -1)
  vdiffr::expect_doppelganger("Censored plot with bql",
                              plot(vpc, censoring.type = "blq"))

  vdiffr::expect_doppelganger("Censored plot with aql",
                              plot(vpc, censoring.type = "alq"))

  vdiffr::expect_doppelganger("Censored plot with bql aql",
                              plot(vpc, censoring.type = "both"))

  plot_list <- plot(vpc, censoring.type = "both", censoring.output = "list")
  testthat::expect_true(length(plot_list) == 3)
  testthat::expect_true(all(sapply(plot_list, ggplot2::is.ggplot)))

  plot_grid <- plot(vpc, censoring.type = "both", censoring.output = "grid", nrow = 1, ncol = 3)
  testthat::expect_true(inherits(plot_grid, "egg"))
  options(warn = 0)

})

test_that("plot.tidyvpcobj plots stratified", {
  testthat::skip_if_not(get_os() == "windows")
  testthat::skip_on_cran()

  obs_data <- obs_data[MDV == 0]
  sim_data <- sim_data[MDV == 0]

  options(warn = -1)
  #two-sided strat formula
  vpc <- observed(obs_data, x = TIME, y = DV)
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- stratify(vpc, GENDER ~ STUDY)
  vpc <- binning(vpc, bin = NTIME)
  vpc <- vpcstats(vpc, qpred = c(0.1, 0.5, 0.9), quantile.type = 6)

  vdiffr::expect_doppelganger("Two sided strat formula with facet_grid",
                              plot(vpc))

  #one-sided strat formula
  vpc <- observed(obs_data, x = TIME, y = DV)
  vpc <- simulated(vpc, sim_data, y = DV)
  vpc <- stratify(vpc, ~ GENDER + STUDY)
  vpc <- binless(vpc)
  vpc <- vpcstats(vpc, qpred = c(0.1, 0.5, 0.9), quantile.type = 6)

  vdiffr::expect_doppelganger("One sided strat formula with facet_wrap",
                              plot(vpc))
  options(warn = 0)
})

test_that("plotting shows a finite width with single-value groups (related to #51)", {
  testthat::skip_if_not(get_os() == "windows")
  testthat::skip_on_cran()

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
    binning(bin = "jenks") %>%
    vpcstats()

  vdiffr::expect_doppelganger(
    "single-value group",
    plot(value)
  )
  options(warn = 0)
})
