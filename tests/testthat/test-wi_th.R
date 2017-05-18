context("wi_th")

test_that("wi_th: with just headers", {
  aa <- stub_request("get", url="https://httpbin.org/get") %>%
    wi_th(headers = list('User-Agent' = 'R'))

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
  expect_equal(aa$request_headers, list('User-Agent' = 'R'))
})

test_that("wi_th: with headers and query", {
  aa <- stub_request("get", url="https://httpbin.org/get") %>%
    wi_th(
      query = list(hello = "world"),
      headers = list('User-Agent' = 'R'))

  expect_is(aa$query, "list")
  expect_is(aa$request_headers, "list")

  expect_output(print(aa), "hello=world")
  expect_output(print(aa), "User-Agent=R")
})

test_that("wi_th fails well", {
  expect_error(wi_th(), "argument \".data\" is missing")
})
