test_that("last_request works when no requests found", {
  request_registry_clear()

  expect_null(last_request())
})

test_that("last_request works when requests are found", {
  request_registry_clear()

  enable()
  stub_request("head", "https://nytimes.com")
  crul::ok("https://nytimes.com")
  last_request()

  expect_s3_class(last_request(), "RequestSignature")
})
