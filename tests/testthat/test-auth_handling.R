# from https://github.com/ropensci/webmockr/issues/108

# httr
stub_registry()$remove_all_request_stubs()
skip_if_not_installed("httr")
suppressPackageStartupMessages(library("httr", warn.conflicts = FALSE))
enable("httr", quiet = TRUE)

test_that("auth handling: httr", {
  stub_request("get", "http://stuff.com")

  # auth well-formed
  expect_s3_class(
    GET("http://stuff.com", authenticate("adf", "adf")),
    "response"
  )

  # user name invalid according to RFC, but we can't know that
  expect_s3_class(
    GET("http://stuff.com", authenticate("foo:bar", "adf")),
    "response"
  )

  # malformed: url as username
  expect_error(
    GET("http://stuff.com", authenticate("http://", "foo.com"))
  )
})


# crul
disable(quiet = TRUE)
stub_registry()$remove_all_request_stubs()
skip_if_not_installed("crul")
library("crul")
enable("crul", quiet = TRUE)

test_that("auth handling: httr", {
  stub_request("get", "http://stuff.com")

  # auth well-formed
  x <- HttpClient$new("http://stuff.com")
  x$auth <- auth("adf", "adf")
  expect_s3_class(x$get(), "HttpResponse")

  # user name invalid according to RFC, but we can't know that
  y <- HttpClient$new("http://stuff.com")
  y$auth <- auth("foo:bar", "adf")
  expect_s3_class(y$get(), "HttpResponse")

  # malformed: url as username
  z <- HttpClient$new("http://stuff.com")
  z$auth <- auth("http://", "foo.com")
  expect_error(z$get())
})

stub_registry()$remove_all_request_stubs()
disable(quiet = TRUE)
