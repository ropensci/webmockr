context("wi_th")

test_that("wi_th: with just headers", {
  aa <- stub_request("get", "https://httpbin.org/get") %>%
    wi_th(headers = list("User-Agent" = "R"))

  expect_is(aa, "StubbedRequest")
  expect_null(aa$body)
  expect_null(aa$host)
  expect_null(aa$query)
  expect_is(aa$request_headers, "list")
  expect_null(aa$response)
  expect_null(aa$response_headers)
  expect_null(aa$responses_sequences)

  expect_is(aa$method, "character")
  expect_equal(aa$method, "get")
  expect_is(aa$uri, "character")
  expect_equal(aa$uri, "https://httpbin.org/get")
  expect_equal(aa$request_headers, list("User-Agent" = "R"))
})

test_that("wi_th: with headers and query", {
  aa <- stub_request("get", "https://httpbin.org/get") %>%
    wi_th(
      query = list(hello = "world"),
      headers = list("User-Agent" = "R"))

  expect_is(aa$query, "list")
  expect_is(aa$request_headers, "list")

  expect_output(print(aa), "hello=world")
  expect_output(print(aa), "User-Agent=R")
})

test_that("wi_th fails well", {
  expect_error(wi_th(), "argument \".data\" is missing")
  expect_error(wi_th(5), ".data must be of class StubbedRequest")

  zzz <- stub_request("get", "https://httpbin.org/get")

  # query
  expect_error(wi_th(zzz, query = list(5, 6)),
               "'query' must be a named list")
  expect_error(wi_th(zzz, query = list(a = 5, 6)),
               "'query' must be a named list")

  # headers
  expect_error(wi_th(zzz, headers = list(5, 6)),
               "'headers' must be a named list")
  expect_error(wi_th(zzz, headers = list(a = 5, 6)),
               "'headers' must be a named list")

  # only accepts certain set of named things
  expect_error(wi_th(zzz, a = 5),
    "'wi_th' only accepts query, body, headers")
})

test_that("wi_th .list works", {
  req <- stub_request("post", "https://httpbin.org/post")
  expect_equal(
    wi_th(req, .list = list(body = list(foo = "bar"))),
    wi_th(req, body = list(foo = "bar"))
  )
  expect_equal(
    wi_th(req, .list = list(query = list(a = 3445))),
    wi_th(req, query = list(a = 3445))
  )
  expect_equal(wi_th(req, .list = ), wi_th(req))

  expect_error(wi_th(req, .list = 4), ".list must be of class list")
  expect_error(wi_th(req, .list = list(a = 5)),
    "'wi_th' only accepts query, body, headers")
})

# cleanup
stub_registry_clear()

context("wi_th_: defunct")
test_that("wi_th_: defunct", {
  expect_error(wi_th_(), "wi_th")
})
