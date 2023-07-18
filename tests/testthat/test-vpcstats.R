test_that("simulated.tidyvpcobj detects row count mismatches", {
  vpcobj_o <- observed(o = data.frame(x = 0:1, y = c(0, 2), pred = c(0, 2.5)), x = x, yobs = y)
  expect_silent(
    simulated(vpcobj_o, data = data.frame(x = 0:1, y = c(0, 3)), x = x, y = y)
  )
  expect_error(
    simulated(vpcobj_o, data = data.frame(x = 0:2, y = c(0, 3, 4)), x = x, y = y),
    regexp = "The number of simulated rows is not a multiple of the number of observed rows.  Ensure that you filtered your observed data to remove MDV rows.",
    fixed = TRUE
  )
})

test_that("simulated.tidyvpcobj detects x-variable mismatches", {
  vpcobj_o <- observed(o = data.frame(x = 0:1, y = c(0, 2), pred = c(0, 2.5)), x = x, yobs = y)
  expect_silent(
    simulated(vpcobj_o, data = data.frame(x = c(0:1, 0:1), y = c(0, 3, 0, 4)), xsim = x, y = y)
  )
  expect_error(
    simulated(vpcobj_o, data = data.frame(x = c(0:2, 1), y = c(0, 3, 0, 4)), xsim = x, y = y),
    regexp = "Values of `xsim` do not match observed data x-values.  Ensure that you filtered your observed data to remove MDV rows.",
    fixed = TRUE
  )
})

test_that("predcorrect.tidyvpcobj", {
  # Prevent division by zero for prediction correction
  vpcobj_o <- observed(o = data.frame(x = 0:1, y = c(0, 2), pred = c(0, 2.5)), x = x, yobs = y)
  vpcobj_s <- simulated(vpcobj_o, data = data.frame(x = 0:1, y = c(0, 3)), x = x, y = y)
  vpcobj_predcorr <- predcorrect(vpcobj_s, pred = pred)
  expect_equal(vpcobj_predcorr$obs$ypc, c(0, 1))
})
