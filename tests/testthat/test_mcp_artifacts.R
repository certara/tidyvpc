# MCP inst/mcp artifacts ship with the package.

test_that("manifest and capabilities are present", {
  expect_true(file.exists(system.file("mcp/tools/manifest.json", package = "tidyvpc")))
  expect_true(file.exists(system.file("mcp/capabilities.json", package = "tidyvpc")))
})

test_that("KB manifest has required fields", {
  mpath <- system.file("mcp/kb/manifest.json", package = "tidyvpc")
  skip_if(!nzchar(mpath), "KB not generated yet")
  raw <- paste(readLines(mpath, warn = FALSE), collapse = "\n")
  expect_match(raw, '"package"\\s*:\\s*"tidyvpc"')
  expect_match(raw, '"schema_version"')
  expect_match(raw, '"entry_count"')
})

test_that("KB manifest validates with Certara.R host validator", {
  skip_if_not(.certara_mcp_host(), "Certara.R MCP host API not available")
  skip_if_not_installed("jsonlite")
  mpath <- system.file("mcp/kb/manifest.json", package = "tidyvpc")
  skip_if(!nzchar(mpath), "KB not generated yet")
  manifest <- jsonlite::fromJSON(mpath, simplifyVector = FALSE)
  validate <- get0(".validate_kb_manifest", envir = asNamespace("Certara.R"),
                   mode = "function", inherits = FALSE)
  skip_if(is.null(validate), "Certara.R KB validator not available")
  expect_length(validate(manifest), 0L)
})
