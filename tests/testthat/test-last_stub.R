test_that("last_stub works when no stubs found", {
  stub_registry_clear()

  expect_null(last_stub())
})

test_that("last_stub works when stubs are found", {
  stub_registry_clear()

  stub_request("head", "https://nytimes.com")

  expect_s3_class(last_stub(), "StubbedRequest")
})
