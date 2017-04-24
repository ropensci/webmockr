context("StubbedRequest")

test_that("StubbedRequest: works", {
  expect_is(StubbedRequest, "R6ClassGenerator")

  aa <- StubbedRequest$new(method = "get", uri = "https:/httpbin.org/get")

  expect_is(aa, "StubbedRequest")

  expect_null(aa$host)
  expect_null(aa$query)
  expect_null(aa$body)
  expect_null(aa$request_headers)
  expect_null(aa$response_headers)
  expect_null(aa$response)
  expect_null(aa$response_sequences)

  expect_is(aa$method, "character")
  expect_equal(aa$method, "get")

  expect_is(aa$uri, "character")
  expect_equal(aa$uri, "https:/httpbin.org/get")

  expect_is(aa$uri_parts, "list")
  expect_equal(aa$uri_parts$domain, "https")
  expect_equal(aa$uri_parts$path, "httpbin.org/get")

  expect_is(aa$to_s, "function")
  expect_equal(aa$to_s(), "get: https:/httpbin.org/get")

  # with
  expect_is(aa$with, "function")
  expect_null(aa$query)
  aa$with(query = list(foo = "bar"))
  expect_is(aa$query, "list")
  expect_named(aa$query, "foo")

  # to_return
  expect_is(aa$to_return, "function")
  expect_null(aa$body)
  aa$to_return(
    status = 404,
    body = list(hello = "world"),
    response_headers = list(a = 5)
  )
  expect_is(aa$responses_sequences, "list")
  expect_is(aa$responses_sequences$body, "list")
  expect_named(aa$responses_sequences$body, "hello")
})

test_that("StubbedRequest: different methods work", {
  expect_equal(
    StubbedRequest$new(method = "any", uri = "https:/httpbin.org/get")$method,
    "any"
  )
  expect_equal(
    StubbedRequest$new(method = "get", uri = "https:/httpbin.org/get")$method,
    "get"
  )
  expect_equal(
    StubbedRequest$new(method = "head", uri = "https:/httpbin.org/get")$method,
    "head"
  )
  expect_equal(
    StubbedRequest$new(method = "post", uri = "https:/httpbin.org/get")$method,
    "post"
  )
  expect_equal(
    StubbedRequest$new(method = "put", uri = "https:/httpbin.org/get")$method,
    "put"
  )
  expect_equal(
    StubbedRequest$new(method = "patch", uri = "https:/httpbin.org/get")$method,
    "patch"
  )
  expect_equal(
    StubbedRequest$new(method = "delete", uri = "https:/httpbin.org/get")$method,
    "delete"
  )
})

test_that("StubbedRequest fails well", {
  # doesn't fail, doesn't require any inputs
  expect_is(StubbedRequest$new(), "StubbedRequest")

  # method not in acceptable set
  expect_error(StubbedRequest$new(method = "adf"),
               "'arg' should be one of")
})
