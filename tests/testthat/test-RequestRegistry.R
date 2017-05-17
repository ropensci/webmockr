context("RequestRegistry")

test_that("RequestRegistry: structure", {
  expect_is(RequestRegistry, "R6ClassGenerator")

  aa <- RequestRegistry$new()

  expect_is(aa, "RequestRegistry")
  expect_is(aa$clone, "function")
  expect_is(aa$print, "function")
  expect_is(aa$register_request, "function")
  expect_null(aa$request)
  expect_is(aa$request_signatures, "HashCounter")
  expect_is(aa$reset, "function")
})

test_that("RequestRegistry: behaves as expected", {
  aa <- RequestRegistry$new()
  aa$reset()

  expect_length(aa$request_signatures$hash, 0)

  aa$register_request(request = "GET https://scottchamberlain.info")
  aa$register_request(request = "GET https://scottchamberlain.info")

  expect_length(aa$request_signatures$hash, 1)
  expect_equal(
    aa$request_signatures$hash$`GET https://scottchamberlain.info`,
    2
  )

  expect_output(
    print(aa), "Registered Requests"
  )
  expect_output(
    print(aa), "GET https://scottchamberlain.info was made"
  )

  # reset the request registry
  aa$reset()
  expect_length(aa$request_signatures$hash, 0)
})

test_that("RequestRegistry fails well", {
  x <- RequestRegistry$new()

  expect_error(x$register_request(), "'key' required")
})
