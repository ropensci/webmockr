context("to_return")

stub_registry()$remove_all_request_stubs()

test_that("no stubs exist before stub_request called", {
  expect_equal(length(stub_registry()$request_stubs), 0)
})

aa <- stub_request("get", "https://httpbin.org/get") %>%
  to_return(status = 200, body = "stuff", headers = list(a = 5))

test_that("stub_request bits are correct", {

  expect_is(aa, "StubbedRequest")
  expect_null(aa$body)
  expect_null(aa$host)
  expect_null(aa$response)
  expect_null(aa$query)
  expect_null(aa$request_headers)

  expect_is(aa$method, "character")
  expect_equal(aa$method, "get")
  expect_is(aa$uri, "character")
  expect_equal(aa$uri, "https://httpbin.org/get")

  # to_return expected stuff
  expect_is(aa$response_headers, "list")
  expect_named(aa$response_headers, "a")
  expect_equal(aa$response_headers$a, 5)

  expect_is(aa$responses_sequences, "list")
  expect_named(aa$responses_sequences, c("status", "body", "headers"))
  expect_equal(aa$responses_sequences$status, 200)
  expect_equal(aa$responses_sequences$body, "stuff")
})

test_that("stubs exist after stub_request called", {
  expect_equal(length(stub_registry()$request_stubs), 1)
})

test_that("stub_request fails well", {
  expect_error(to_return(), "argument \".data\" is missing")
  expect_error(to_return(5), ".data must be of class StubbedRequest")

  zzz <- stub_request("get", "https://httpbin.org/get")

  # status
  expect_error(to_return(zzz, status = "foo"), "must be of class numeric")

  # headers
  expect_error(to_return(zzz, headers = list(5, 6)), "'headers' must be a named list")
  expect_error(to_return(zzz, headers = list(a = 5, 6)), "'headers' must be a named list")
})
