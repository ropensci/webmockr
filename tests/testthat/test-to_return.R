context("to_return: works as expected")
stub_registry()$remove_all_request_stubs()

test_that("no stubs exist before stub_request called", {
  expect_equal(length(stub_registry()$request_stubs), 0)
})

aa <- stub_request("get", hb("/get")) %>%
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
  expect_equal(aa$uri, hb("/get"))

  # to_return expected stuff
  expect_is(aa$response_headers, "list")
  expect_named(aa$response_headers, "a")
  expect_equal(aa$response_headers$a, 5)

  expect_is(aa$responses_sequences, "list")
  expect_identical(
    sort(names(aa$responses_sequences[[1]])),
    sort(c(
      "status",
      "body",
      "headers",
      "body_raw",
      "timeout",
      "raise",
      "exceptions"
    ))
  )
  expect_equal(aa$responses_sequences[[1]]$status, 200)
  expect_equal(aa$responses_sequences[[1]]$body, "stuff")
})

test_that("stubs exist after stub_request called", {
  expect_equal(length(stub_registry()$request_stubs), 1)
})

test_that("stub_request fails well", {
  expect_error(to_return(), "argument \".data\" is missing")
  expect_error(to_return(5), "must be of class StubbedRequest")

  # status
  zzz <- stub_request("get", hb("/get"))
  expect_error(
    sw(to_return(zzz, status = "foo")),
    "must be of class numeric"
  )

  # headers
  zzz <- stub_request("get", hb("/get"))
  expect_error(
    sw(to_return(zzz, headers = list(5, 6))),
    "'headers' must be a named list"
  )
  zzz <- stub_request("get", hb("/get"))
  expect_error(
    sw(to_return(zzz, headers = list(a = 5, 6))),
    "'headers' must be a named list"
  )

  zzz <- stub_request("get", hb("/get"))
  expect_error(
    sw(to_return(zzz, .list = 4)),
    "must be of class list"
  )
})


stub_registry_clear()
enable()
context("to_return: response headers returned all lowercase")
test_that("to_return (response) headers are all lowercase, crul", {
  stub <- stub_request(uri = hb("/get")) %>%
    to_return(headers = list("Foo-Bar" = "baz"))
  cli <- crul::HttpClient$new(url = hb())
  x <- cli$get("get")

  expect_is(x$response_headers, "list")
  expect_named(x$response_headers, "foo-bar")
})

stub_registry_clear()
test_that("to_return (response) headers are all lowercase, httr", {
  loadNamespace("httr")
  stub <- stub_request(uri = hb("/get")) %>%
    to_return(headers = list("Foo-Bar" = "baz"))
  x <- httr::GET(hb("/get"))

  expect_is(x$headers, "list")
  expect_named(x$headers, "foo-bar")
})
disable()

stub_registry_clear()
enable()
test_that("to_return (response) headers are all lowercase, httr2", {
  skip_if_not_installed("httr2")
  loadNamespace("httr2")
  stub <- stub_request(uri = hb("/get")) %>%
    to_return(headers = list("Foo-Bar" = "baz"))
  req <- httr2::request(hb("/get"))
  x <- httr2::req_perform(req)

  expect_is(x$headers, "httr2_headers")
  expect_named(x$headers, "foo-bar")
})
disable()


stub_registry_clear()
enable()
test_that("to_return (response) header is the correct class, httr2", {
  skip_if_not_installed("httr2")
  loadNamespace("httr2")
  stub <- stub_request(uri = hb("/get")) %>%
    to_return(headers = list("Foo-Bar" = "baz"))
  req <- httr2::request(hb("/get"))
  x <- httr2::req_perform(req)

  expect_is(x$headers, "httr2_headers")
})
disable()


stub_registry_clear()
enable()
context("to_return: response header values are all character")
test_that("to_return response header values are all character, crul", {
  cli <- crul::HttpClient$new(url = hb())

  stub_request(uri = hb("/get")) %>%
    to_return(headers = list("Foo-Bar" = 10))
  x <- cli$get("get")

  expect_is(x$response_headers, "list")
  expect_named(x$response_headers, "foo-bar")
  expect_is(x$response_headers$`foo-bar`, "character")
  expect_equal(x$response_headers$`foo-bar`, "10")

  stub_registry_clear()
  stub_request(uri = hb("/get")) %>%
    to_return(
      headers = list(
        a = 10,
        b = 234233434,
        c = 2344.342342,
        d = "brown",
        e = as.factor("blue")
      )
    )
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

  stub_request(uri = hb("/get")) %>%
    to_return(headers = list("Foo-Bar" = 10))
  x <- httr::GET(hb("/get"))

  expect_is(x$headers, "list")
  expect_named(x$headers, "foo-bar")
  expect_is(x$headers$`foo-bar`, "character")
  expect_equal(x$headers$`foo-bar`, "10")

  stub_registry_clear()
  stub_request(uri = hb("/get")) %>%
    to_return(
      headers = list(
        a = 10,
        b = 234233434,
        c = 2344.342342,
        d = "brown",
        e = as.factor("blue")
      )
    )
  z <- httr::GET(hb("/get"))

  expect_is(z$headers, "list")
  expect_named(z$headers, letters[1:5])
  invisible(
    vapply(z$headers, function(z) expect_is(z, "character"), "")
  )
  expect_equal(z$headers$c, "2344.342342")
  expect_equal(z$headers$e, "blue")
})
disable()

enable()
test_that("to_return response header values are all character, httr2", {
  skip_if_not_installed("httr2")
  loadNamespace("httr2")

  stub_request(uri = hb("/get")) %>%
    to_return(headers = list("Foo-Bar" = 10))
  req <- httr2::request(hb("/get"))
  x <- httr2::req_perform(req)

  expect_is(x$headers, "httr2_headers")
  expect_named(x$headers, "foo-bar")
  expect_is(x$headers$`foo-bar`, "character")
  expect_equal(x$headers$`foo-bar`, "10")

  stub_registry_clear()
  stub_request(uri = hb("/get")) %>%
    to_return(
      headers = list(
        a = 10,
        b = 234233434,
        c = 2344.342342,
        d = "brown",
        e = as.factor("blue")
      )
    )
  req <- httr2::request(hb("/get"))
  z <- httr2::req_perform(req)

  expect_is(z$headers, "httr2_headers")
  expect_named(z$headers, letters[1:5])
  invisible(
    vapply(z$headers, function(z) expect_is(z, "character"), "")
  )
  expect_equal(z$headers$c, "2344.342342")
  expect_equal(z$headers$e, "blue")
})
disable()


context("to_return_: defunct")
test_that("to_return_: defunct", {
  expect_error(to_return_(), "to_return", class = "error")
})


stub_to_return_status_code <- function() {
  stub_registry()$request_stubs[[1]]$responses_sequences[[1]]$status
}
stub_registry_clear()
enable()
test_that("stub_request status accepts numeric or integer values", {
  stub_status_type_a <- stub_request("get", hb("/get"))
  expect_s3_class(to_return(stub_status_type_a, status = 200), "StubbedRequest")
  expect_type(stub_to_return_status_code(), "double") # numeric = double

  stub_registry_clear()
  stub_status_type_b <- stub_request("get", hb("/get"))
  expect_s3_class(
    to_return(stub_status_type_b, status = 200L),
    "StubbedRequest"
  )
  expect_type(stub_to_return_status_code(), "integer")
})
disable()
