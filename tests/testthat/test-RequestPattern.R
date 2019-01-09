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
  rs3 <- RequestSignature$new(
    method = "get",
    uri = "https:/httpbin.org/get",
    options = list(headers = list(`User-Agent` = "foobar", stuff = "things"))
  )

  expect_true(aa$matches(rs1))
  expect_false(aa$matches(rs2))
  expect_false(aa$matches(rs3))

  expect_is(aa$to_s(), "character")
  expect_match(aa$to_s(), "GET")
  expect_match(aa$to_s(), "httpbin.org/get")
})

test_that("RequestPattern: uri_regex", {
  x <- RequestPattern$new(method = "get", uri_regex = ".+ossref.org")
  expect_is(x$uri_pattern, "UriPattern")
  expect_equal(x$uri_pattern$to_s(), "http://.+ossref.org")
  expect_equal(x$to_s(), "GET http://.+ossref.org")
})

test_that("RequestPattern fails well", {
  expect_error(RequestPattern$new(), "one of uri or uri_regex is required")
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
  expect_false(aa$matches(list()))

  # with pattern empty
  bb <- HeadersPattern$new(pattern = list())
  expect_true(bb$matches(list()))

  expect_error(
    expect_is(aa$matches(), "function"),
    "argument \"headers\" is missing"
  )

  expect_equal(aa$to_s(), list(a = 5))
})



context("BodyPattern")
test_that("BodyPattern: structure is correct", {
  expect_is(BodyPattern, "R6ClassGenerator")

  bb <- RequestSignature$new(
    method = "get",
    uri = "https:/httpbin.org/get",
    options = list(
      body = list(foo = "bar", a = 5)
    )
  )

  aa <- BodyPattern$new(pattern = list(foo = "bar"))
  expect_is(aa, "BodyPattern")
  expect_is(aa$pattern, "list")
  expect_named(aa$pattern, "foo")
  expect_false(aa$matches(bb$body))

  aaa <- BodyPattern$new(pattern = list(foo = "bar", a = 5))
  expect_true(aaa$matches(bb$body))

  # with pattern empty
  bb <- BodyPattern$new(pattern = list())
  expect_true(bb$matches(list()))

  expect_error(
    aa$matches(),
    "argument \"body\" is missing"
  )

  expect_equal(aa$to_s(), list(foo = "bar"))
})



context("UriPattern")
test_that("UriPattern: structure is correct", {
  expect_is(UriPattern, "R6ClassGenerator")

  aa <- UriPattern$new(pattern = "http://foobar.com")

  expect_is(aa, "UriPattern")
  expect_is(aa$pattern, "character")
  expect_false(aa$regex)
  expect_match(aa$pattern, "foobar")
  # matches w/o slash
  expect_true(aa$matches("http://foobar.com"))
  # and matches w/ slash
  expect_true(aa$matches("http://foobar.com/"))

  # fails well
  expect_error(
    expect_is(aa$matches(), "function"),
    "argument \"uri\" is missing"
  )

  # regex usage
  z <- UriPattern$new(regex_pattern = ".+ample\\..")

  expect_is(z, "UriPattern")
  expect_is(z$pattern, "character")
  expect_true(z$regex)
  expect_true(z$matches("http://sample.org"))
  expect_true(z$matches("http://example.com"))
  expect_false(z$matches("http://tramples.net"))

  # add query params usage
  z <- UriPattern$new(pattern = "http://foobar.com")
  expect_equal(z$pattern, "http://foobar.com")
  z$add_query_params(list(pizza = "cheese", cheese = "cheddar"))
  expect_equal(z$pattern, "http://foobar.com?pizza=cheese&cheese=cheddar")
})
