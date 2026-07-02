# MCP session handle cache.

test_that("tidyvpc handle store and get", {
  .tidyvpc_state_reset()
  h <- .tidyvpc_store(list(obs = data.frame(TIME = 1, DV = 1),
                           sim = data.frame(DV = 1), vpc_obj = NULL))
  expect_equal(h, "vpc1")
  sess <- .tidyvpc_get(h)
  expect_equal(nrow(sess$obs), 1L)
})

test_that("unknown handle errors with guidance", {
  .tidyvpc_state_reset()
  expect_error(.tidyvpc_get("vpc99"), "tidyvpc_load_from_dir")
})
