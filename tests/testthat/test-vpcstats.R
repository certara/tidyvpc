test_that("simulated.tidyvpcobj detects row count mismatches", {
  vpcobj_o <-
    observed(o = data.frame(
      x = 0:1,
      y = c(0, 2),
      pred = c(0, 2.5)
    ),
    x = x,
    yobs = y)
  expect_silent(simulated(
    vpcobj_o,
    data = data.frame(x = 0:1, y = c(0, 3)),
    xsim = x,
    ysim = y
  ))
  expect_error(
    simulated(
      vpcobj_o,
      data = data.frame(x = 0:2, y = c(0, 3, 4)),
      x = x,
      ysim = y
    ),
    regexp = "The number of simulated rows is not a multiple of the number of observed rows.  Ensure that you filtered your observed data to remove MDV rows.",
    fixed = TRUE
  )
})

test_that("simulated.tidyvpcobj detects x-variable mismatches", {
  vpcobj_o <-
    observed(o = data.frame(
      x = 0:1,
      y = c(0, 2),
      pred = c(0, 2.5)
    ),
    x = x,
    yobs = y)
  expect_silent(simulated(
    vpcobj_o,
    data = data.frame(x = c(0:1, 0:1), y = c(0, 3, 0, 4)),
    xsim = x,
    ysim = y
  ))
  expect_error(
    simulated(
      vpcobj_o,
      data = data.frame(x = c(0:2, 1), y = c(0, 3, 0, 4)),
      xsim = x,
      ysim = y
    ),
    regexp = "Values of `xsim` do not match observed data x-values.  Ensure that you filtered your observed data to remove MDV rows.",
    fixed = TRUE
  )
})

test_that("predcorrect.tidyvpcobj", {
  # Prevent division by zero for prediction correction
  vpcobj_o <-
    observed(o = data.frame(
      x = 0:1,
      y = c(0, 2),
      pred = c(0, 2.5)
    ),
    x = x,
    yobs = y)
  vpcobj_s <-
    simulated(
      vpcobj_o,
      data = data.frame(x = 0:1, y = c(0, 3)),
      xsim = x,
      ysim = y
    )
  vpcobj_b <- binning(vpcobj_s, bin = x)
  vpcobj_predcorr <- predcorrect(vpcobj_b, pred = pred, varcorr = TRUE)
  expect_equal(vpcobj_predcorr$obs$ypc, c(0, 2))
})

test_that("predcorrect.tidyvpcobj with variance correction", {
  vpcobj_o <-
    observed(o = data.frame(
      x = 0:1,
      y = c(0, 2, 1, 3),
      pred = c(1, 2, 0, 4)
    ),
    x = x,
    yobs = y)
  vpcobj_s <-
    simulated(
      vpcobj_o,
      data = data.frame(x = 0:1, y = c(0, 1, 1, 2, 1, 3, 0, 4)),
      xsim = x,
      ysim = y
    )
  vpcobj_b <- binning(vpcobj_s, bin = x)
  vpcobj_bp <- predcorrect(vpcobj_b, pred = pred, varcorr = TRUE)
  vpcobj_p <- predcorrect(vpcobj_s, pred = pred, varcorr = TRUE)
  vpcobj_pb <- binning(vpcobj_p, bin = x)
  expect_equal(vpcobj_bp$sim$ypcvc, vpcobj_pb$sim$ypcvc)
})