context("to_raise")

stub_registry()$remove_all_request_stubs()

test_that("no stubs exist before stub_request called", {
  expect_equal(length(stub_registry()$request_stubs), 0)
})

library(fauxpas)
aa <- stub_request("get", "https://httpbin.org/get") %>% to_raise(HTTPAccepted)

test_that("stub_request bits are correct", {

  expect_is(aa, "StubbedRequest")
  expect_null(aa$body)
  expect_null(aa$host)
  expect_null(aa$response)
  expect_null(aa$query)
  expect_null(aa$request_headers)
  expect_null(aa$response_headers)
  # expect_false(aa$timeout) # timeout will be removed in StubbedRequest

  expect_is(aa$method, "character")
  expect_equal(aa$method, "get")
  expect_is(aa$uri, "character")
  expect_equal(aa$uri, "https://httpbin.org/get")

  # to_raise expected stuff
  rr <- aa$responses_sequences[[1]]
  expect_true(rr$raise)
  expect_is(rr$exceptions, "list")
  expect_is(rr$exceptions[[1]], "R6ClassGenerator")
  expect_equal(rr$exceptions[[1]]$classname, "HTTPAccepted")
  expect_equal(rr$exceptions[[1]]$new()$status_code, 202)
})

test_that("stubs exist after stub_request called", {
  expect_equal(length(stub_registry()$request_stubs), 1)
})

test_that("stub_request fails well", {
  expect_error(to_raise(), "argument \".data\" is missing")
  expect_error(to_raise(5), ".data must be of class StubbedRequest")

  # exception clases
  zzz <- stub_request("get", "https://httpbin.org/get")
  expect_error(to_raise(zzz, "foo"),
               "all objects must be error classes from fauxpas")
})
