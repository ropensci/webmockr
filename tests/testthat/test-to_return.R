context("to_return: works as expected")
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
  expect_named(aa$responses_sequences, c("status", "body", "headers", "body_raw"))
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


stub_registry_clear()
enable()
context("to_return: response headers returned all lowercase")
test_that("to_return (response) headers are all lowercase, crul", {
  stub <- stub_request(uri = "http://httpbin.org/get") %>%
    to_return(headers = list("Foo-Bar" = "baz"))
  cli <- crul::HttpClient$new(url = "http://httpbin.org/")
  x <- cli$get("get")

  expect_is(x$response_headers, "list")
  expect_named(x$response_headers, "foo-bar")
})

stub_registry_clear()
test_that("to_return (response) headers are all lowercase, httr", {
  loadNamespace("httr")
  stub <- stub_request(uri = "http://httpbin.org/get") %>%
    to_return(headers = list("Foo-Bar" = "baz"))
  x <- httr::GET("http://httpbin.org/get")

  expect_is(x$headers, "list")
  expect_named(x$headers, "foo-bar")
})
disable()



stub_registry_clear()
enable()
context("to_return: response header values are all character")
test_that("to_return response header values are all character, crul", {
  cli <- crul::HttpClient$new(url = "http://httpbin.org/")
  
  stub_request(uri = "http://httpbin.org/get") %>%
    to_return(headers = list("Foo-Bar" = 10))
  x <- cli$get("get")

  expect_is(x$response_headers, "list")
  expect_named(x$response_headers, "foo-bar")
  expect_is(x$response_headers$`foo-bar`, "character")
  expect_equal(x$response_headers$`foo-bar`, "10")

  stub_registry_clear()
  stub_request(uri = "http://httpbin.org/get") %>%
    to_return(headers = list(
      a = 10, b = 234233434, c = 2344.342342, 
      d = "brown", e = as.factor("blue")
    ))
  z <- cli$get("get")

  expect_is(z$response_headers, "list")
  expect_named(z$response_headers, letters[1:5])
  invisible(
    vapply(z$response_headers, function(z) expect_is(z, "character"), "")
  )
  expect_equal(z$response_headers$c, "2344.342342")
  expect_equal(z$response_headers$e, "blue")
})

stub_registry_clear()

test_that("to_return response header values are all character, httr", {
  loadNamespace("httr")
  
  stub_request(uri = "http://httpbin.org/get") %>%
    to_return(headers = list("Foo-Bar" = 10))
  x <- httr::GET("http://httpbin.org/get")

  expect_is(x$headers, "list")
  expect_named(x$headers, "foo-bar")
  expect_is(x$headers$`foo-bar`, "character")
  expect_equal(x$headers$`foo-bar`, "10")

  stub_registry_clear()
  stub_request(uri = "http://httpbin.org/get") %>%
    to_return(headers = list(
      a = 10, b = 234233434, c = 2344.342342, 
      d = "brown", e = as.factor("blue")
    ))
  z <- httr::GET("http://httpbin.org/get")

  expect_is(z$headers, "list")
  expect_named(z$headers, letters[1:5])
  invisible(
    vapply(z$headers, function(z) expect_is(z, "character"), "")
  )
  expect_equal(z$headers$c, "2344.342342")
  expect_equal(z$headers$e, "blue")
})
disable()
