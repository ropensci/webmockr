context("HttpLibAdapaterRegistry")

test_that("HttpLibAdapaterRegistry: works", {
  expect_is(HttpLibAdapaterRegistry, "R6ClassGenerator")

  aa <- HttpLibAdapaterRegistry$new(method = "get", uri = "https:/httpbin.org/get")

  expect_is(aa, "HttpLibAdapaterRegistry")

  expect_null(aa$auth)
  expect_null(aa$body)
  expect_null(aa$headers)
  expect_null(aa$proxies)

  expect_is(aa$method, "character")
  expect_equal(aa$method, "get")

  expect_is(aa$uri, "character")
  expect_equal(aa$uri, "https:/httpbin.org/get")

  expect_is(aa$to_s, "function")
  expect_equal(aa$to_s(), "GET https:/httpbin.org/get")
})

test_that("HttpLibAdapaterRegistry: different methods work", {
  aa <- HttpLibAdapaterRegistry$new(method = "get", uri = "https:/httpbin.org/get")
  aa$headers <- list(Accept = "application/json")
  aa$body <- list(foo = "bar")

  expect_is(aa$method, "character")
  expect_is(aa$uri, "character")
  expect_is(aa$headers, "list")
  expect_is(aa$body, "list")
})

test_that("HttpLibAdapaterRegistry fails well", {
  expect_error(HttpLibAdapaterRegistry$new(), "argument \"method\" is missing")
  expect_error(HttpLibAdapaterRegistry$new(method = "adf"),
               "'arg' should be one of")
  expect_error(HttpLibAdapaterRegistry$new(method = "get"),
               "argument \"uri\" is missing")
})
