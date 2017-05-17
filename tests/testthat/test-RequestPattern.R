context("RequestPattern")

test_that("RequestPattern: structure is correct", {
  expect_is(RequestPattern, "R6ClassGenerator")

  aa <- RequestPattern$new(method = "get", uri = "https://httpbin.org/get")

  expect_is(aa, "RequestPattern")
  expect_null(aa$body_pattern)
  expect_null(aa$headers_pattern)
  expect_is(aa$clone, "function")
  expect_is(aa$initialize, "function")
  expect_is(aa$matches, "function")
  expect_is(aa$method_pattern, "MethodPattern")
  expect_is(aa$to_s, "function")
  expect_is(aa$uri_pattern, "UriPattern")
})

test_that("RequestPattern: behaves as expected", {
  aa <- RequestPattern$new(method = "get", uri = "https://httpbin.org/get")
  rs1 <- RequestSignature$new(method = "get", uri = "https://httpbin.org/get")
  rs2 <- RequestSignature$new(method = "post", uri = "https://httpbin.org/get")

  expect_true(aa$matches(rs1))
  expect_false(aa$matches(rs2))

  expect_is(aa$to_s(), "character")
  expect_match(aa$to_s(), "GET")
  expect_match(aa$to_s(), "httpbin.org/get")
})

test_that("RequestPattern fails well", {
  x <- RequestPattern$new(method = "get", uri = "https://httpbin.org/get")

  expect_error(x$matches(), "argument \"request_signature\" is missing")
  expect_error(x$matches("adfadf"),
               "request_signature must be of class RequestSignature")
})


context("MethodPattern")
test_that("MethodPattern: structure is correct", {
  expect_is(MethodPattern, "R6ClassGenerator")

  aa <- MethodPattern$new(pattern = "get")

  expect_is(aa, "MethodPattern")
  expect_is(aa$pattern, "character")
  expect_equal(aa$pattern, "get")
  expect_true(aa$matches(method = "get"))
  expect_false(aa$matches(method = "post"))

  expect_error(
    expect_is(aa$matches(), "function"),
    "argument \"method\" is missing"
  )
})


context("HeadersPattern")
test_that("HeadersPattern: structure is correct", {
  expect_is(HeadersPattern, "R6ClassGenerator")

  aa <- HeadersPattern$new(pattern = list(a = 5))

  expect_is(aa, "HeadersPattern")
  expect_is(aa$pattern, "list")
  expect_named(aa$pattern, "a")
  expect_true(aa$matches(headers = list(a = 5)))
  expect_false(aa$matches(headers = list(a = 6)))

  expect_error(
    expect_is(aa$matches(), "function"),
    "argument \"headers\" is missing"
  )
})
