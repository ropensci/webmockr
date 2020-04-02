context("webmockr_reset")

stub_registry_clear()
request_registry_clear()
enable()

test_that("webmockr_reset works", {
  # before any stubs creatd
  expect_equal(length(stub_registry()$request_stubs), 0)
  expect_equal(length(request_registry()$request_signatures$hash), 0)
  expect_null(webmockr_reset())
  expect_equal(length(stub_registry()$request_stubs), 0)
  expect_equal(length(request_registry()$request_signatures$hash), 0)

  # after a stub creatd
  stub_request("get", "https://scottchamberlain.info")
  crul::HttpClient$new("https://scottchamberlain.info")$get()
  expect_equal(length(stub_registry()$request_stubs), 1)
  expect_equal(length(request_registry()$request_signatures$hash), 1)
  webmockr_reset()
  expect_equal(length(stub_registry()$request_stubs), 0)
  expect_equal(length(request_registry()$request_signatures$hash), 0)
})

test_that("webmockr_reset fails well", {
  expect_error(webmockr_reset(4), "unused argument")
})

disable()
