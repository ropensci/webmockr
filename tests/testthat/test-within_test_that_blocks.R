context("within test_that blocks: httr")
library("httr")
test_that("httr: without pipe", {
  httr_mock()
  enable()

  dat_json <- '{"foo":"bar"}'
  stub <- stub_request("get", uri = "https://httpbin.org/get")
  to_return(stub,
    body = dat_json,
    headers = list("Content-Type" = "application/json; charset=utf-8")
  )
  res <- GET("https://httpbin.org/get")
  expect_true(inherits(res, "response"))
  expect_is(content(res), "list")
  expect_named(content(res), "foo")
  expect_equal(content(res)$foo, "bar")
  disable()
  httr_mock(FALSE)
})

test_that("httr: with pipe", {
  enable()
  dat_json <- '{"foo":"bar"}'
  stub <- stub_request("get", uri = "https://httpbin.org/get") %>%
    to_return(body = dat_json,
      headers = list("Content-Type" = "application/json; charset=utf-8"))
  res <- GET("https://httpbin.org/get")
  expect_true(inherits(res, "response"))
  expect_is(content(res), "list")
  expect_named(content(res), "foo")
  expect_equal(content(res)$foo, "bar")
  disable()
})
unloadNamespace("httr")

context("within test_that blocks: crul")

test_that("crul works", {

  enable()
  dat_json <- '{"foo":"bar"}'
  stub <- stub_request("get", uri = "https://httpbin.org/get")
  to_return(stub,
    body = dat_json,
    headers = list("Content-Type" = "application/json; howdy")
  )
  res <- crul::HttpClient$new("https://httpbin.org")$get("get")
  expect_true(inherits(res, "HttpResponse"))
  expect_is(res$parse("UTF-8"), "character")
  expect_is(jsonlite::fromJSON(res$parse("UTF-8")), "list")
  expect_named(jsonlite::fromJSON(res$parse("UTF-8")), "foo")
  expect_equal(jsonlite::fromJSON(res$parse("UTF-8"))$foo, "bar")
  disable()
})

