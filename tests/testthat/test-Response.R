context("Response")

aa <- Response$new()

test_that("Response: bits are correct prior to having data", {
  expect_is(Response, "R6ClassGenerator")

  expect_is(aa, "Response")
  expect_null(aa$body, "function")
  expect_null(aa$content, "function")
  expect_null(aa$exception, "function")
  expect_is(aa$get_body, "function")
  expect_is(aa$get_exception, "function")
  expect_is(aa$get_request_headers, "function")
  expect_is(aa$get_respone_headers, "function")
  expect_is(aa$get_status, "function")
  expect_is(aa$get_url, "function")
  expect_is(aa$print, "function")
  expect_is(aa$set_body, "function")
  expect_is(aa$set_exception, "function")
  expect_is(aa$set_request_headers, "function")
  expect_is(aa$set_response_headers, "function")
  expect_is(aa$set_status, "function")
  expect_is(aa$set_url, "function")
  expect_null(aa$should_timeout, "function")

  expect_null(aa$request_headers)
  expect_null(aa$response_headers)
  expect_equal(aa$status_code, 200)
  expect_null(aa$url)
  expect_null(aa$name)
})


test_that("Response: bits are correct after having data", {
  aa <- Response$new()
  aa$set_url("https://httpbin.org/get")
  aa$set_request_headers(list('Content-Type' = "application/json"))
  aa$set_response_headers(list('Host' = "httpbin.org"))
  aa$set_status(404)
  aa$set_body("hello world")
  aa$set_exception("exception")

  expect_is(aa, "Response")

  expect_null(aa$should_timeout)

  expect_is(aa$request_headers, "list")
  expect_named(aa$request_headers, "Content-Type")
  expect_is(aa$response_headers, "list")
  expect_named(aa$response_headers, "Host")

  expect_equal(aa$status_code, 404)
  expect_equal(aa$url, "https://httpbin.org/get")
  expect_null(aa$name)

  expect_equal(aa$body, "hello world")
  expect_is(aa$content, "raw")
  expect_equal(aa$exception, "exception")
  expect_equal(aa$get_body(), "hello world")
  expect_equal(aa$get_exception(), "exception")
  expect_equal(aa$get_request_headers()[[1]], "application/json")
  expect_equal(aa$get_respone_headers()[[1]], "httpbin.org")
  expect_equal(aa$get_status(), 404)
  expect_equal(aa$get_url(), "https://httpbin.org/get")

  expect_output(aa$print(), "<webmockr response>")
  expect_output(aa$print(), "headers")
  expect_output(aa$print(), "request headers")

  aa$set_body(body = "stuff")
  expect_equal(aa$body, "stuff")

  aa$set_exception(exception = "stop, wait, listen")
  expect_equal(aa$exception, "stop, wait, listen")

  aa$set_request_headers(headers = list(a = "howdy"))
  expect_equal(aa$request_headers[[1]], "howdy")

  aa$set_response_headers(headers = list(b = 6))
  expect_equal(aa$get_respone_headers()[[1]], "6")

  aa$set_status(status = 410)
  expect_equal(aa$status_code, 410)

  aa$set_url(url = "foobar.com")
  expect_equal(aa$url, "foobar.com")
})

test_that("Response fails well", {
  expect_error(aa$set_body(), "argument \"body\" is missing")
  expect_error(aa$set_exception(), "argument \"exception\" is missing")
  expect_error(aa$set_request_headers(), "argument \"headers\" is missing")
  expect_error(aa$set_response_headers(), "argument \"headers\" is missing")
  expect_error(aa$set_status(), "argument \"status\" is missing")
  expect_error(aa$set_url(), "argument \"url\" is missing")
})
