test_that("predcorrect.tidyvpcobj", {
  # Prevent division by zero for prediction correction
  vpcobj_o <- observed(o = data.frame(x = 0:1, y = c(0, 2), pred = c(0, 2.5)), x = x, yobs = y)
  vpcobj_s <- simulated(vpcobj_o, data = data.frame(x = 0:1, y = c(0, 3)), x = x, y = y)
  vpcobj_predcorr <- predcorrect(vpcobj_s, pred = pred)
  expect_equal(vpcobj_predcorr$obs$ypc, c(0, 1))
})
