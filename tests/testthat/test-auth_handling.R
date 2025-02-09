# from https://github.com/ropensci/webmockr/issues/108

# httr
stub_registry()$remove_all_request_stubs()
skip_if_not_installed("httr")
library("httr")
enable("httr")

test_that("auth handling: httr", {
  stub_request("get", "http://stuff.com")

  # auth well-formed
  expect_is(
    GET("http://stuff.com", authenticate("adf", "adf")),
    "response"
  )

  # user name invalid according to RFC, but we can't know that
  expect_is(
    GET("http://stuff.com", authenticate("foo:bar", "adf")),
    "response"
  )

  # malformed: url as username
  expect_error(
    GET("http://stuff.com", authenticate("http://", "foo.com"))
  )
})


# crul
disable()
stub_registry()$remove_all_request_stubs()
skip_if_not_installed("crul")
library("crul")
enable("crul")

test_that("auth handling: httr", {
  stub_request("get", "http://stuff.com")

  # auth well-formed
  x <- HttpClient$new("http://stuff.com")
  x$auth <- auth("adf", "adf")
  expect_is(x$get(), "HttpResponse")

  # user name invalid according to RFC, but we can't know that
  y <- HttpClient$new("http://stuff.com")
  y$auth <- auth("foo:bar", "adf")
  expect_is(y$get(), "HttpResponse")

  # malformed: url as username
  z <- HttpClient$new("http://stuff.com")
  z$auth <- auth("http://", "foo.com")
  expect_error(z$get())
})

stub_registry()$remove_all_request_stubs()
disable()
