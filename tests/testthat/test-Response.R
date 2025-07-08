aa <- Response$new()

test_that("Response: bits are correct prior to having data", {
  expect_s3_class(Response, "R6ClassGenerator")

  expect_s3_class(aa, "Response")
  expect_null(aa$body)
  expect_null(aa$content)
  expect_null(aa$exception)
  expect_type(aa$get_body, "closure")
  expect_type(aa$get_exception, "closure")
  expect_type(aa$get_request_headers, "closure")
  expect_type(aa$get_respone_headers, "closure")
  expect_type(aa$get_status, "closure")
  expect_type(aa$get_url, "closure")
  expect_type(aa$print, "closure")
  expect_type(aa$set_body, "closure")
  expect_type(aa$set_exception, "closure")
  expect_type(aa$set_request_headers, "closure")
  expect_type(aa$set_response_headers, "closure")
  expect_type(aa$set_status, "closure")
  expect_type(aa$set_url, "closure")
  expect_null(aa$should_timeout, "closure")

  expect_null(aa$request_headers)
  expect_null(aa$response_headers)
  expect_null(aa$response_headers_all)
  expect_equal(aa$status_code, 200)
  expect_null(aa$url)
  expect_null(aa$name)
})


test_that("Response: bits are correct after having data", {
  aa <- Response$new()
  aa$set_url(hb("/get"))
  aa$set_request_headers(list("Content-Type" = "application/json"))
  aa$set_response_headers(list("Host" = "hb.cran.dev"))
  aa$set_status(404)
  aa$set_body("hello world")
  aa$set_exception("exception")

  expect_s3_class(aa, "Response")

  expect_null(aa$should_timeout)

  expect_type(aa$request_headers, "list")
  expect_named(aa$request_headers, "Content-Type")
  expect_type(aa$response_headers, "list")
  expect_named(aa$response_headers, "Host")
  # response_headers_all doesn't exist in Response, it's specific to crul
  expect_null(aa$response_headers_all)

  expect_equal(aa$status_code, 404)
  expect_equal(aa$url, hb("/get"))
  expect_null(aa$name)

  expect_equal(aa$body, charToRaw("hello world"))
  expect_type(aa$content, "raw")
  expect_equal(aa$exception, "exception")
  expect_equal(rawToChar(aa$get_body()), "hello world")
  expect_equal(aa$get_exception(), "exception")
  expect_equal(aa$get_request_headers()[[1]], "application/json")
  expect_equal(aa$get_respone_headers()[[1]], "hb.cran.dev")
  expect_equal(aa$get_status(), 404)
  expect_equal(aa$get_url(), hb("/get"))

  expect_output(aa$print(), "<webmockr response>")
  expect_output(aa$print(), "headers")
  expect_output(aa$print(), "request headers")

  # set_body: char gets converted to raw in $content
  aa$set_body(body = "stuff")
  expect_type(aa$body, "raw")
  expect_type(aa$content, "raw")
  expect_length(aa$body, 5)
  expect_length(aa$content, 5)

  # set_body: raw remains as raw in $content
  aa$set_body(body = charToRaw("stuff"))
  expect_type(aa$body, "raw")
  expect_type(aa$content, "raw")
  expect_length(aa$content, 5)

  # set_body: other types return raw(0) in $content
  aa$set_body(body = NULL)
  expect_equal(aa$body, raw())
  expect_type(aa$content, "raw")
  expect_length(aa$content, 0)

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
  # body must be length 1
  expect_error(aa$set_body(letters), "is not TRUE")
  expect_error(aa$set_exception(), "argument \"exception\" is missing")
  expect_error(aa$set_request_headers(), "argument \"headers\" is missing")
  expect_error(aa$set_response_headers(), "argument \"headers\" is missing")
  expect_error(aa$set_status(), "argument \"status\" is missing")
  expect_error(aa$set_url(), "argument \"url\" is missing")
})
