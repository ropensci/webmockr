suppressPackageStartupMessages(library("httr", warn.conflicts = FALSE))
test_that("httr: without pipe", {
  httr_mock()
  enable(quiet = TRUE)

  dat_json <- '{"foo":"bar"}'
  stub <- stub_request("get", uri = hb("/get"))
  to_return(
    stub,
    body = dat_json,
    headers = list("Content-Type" = "application/json; charset=utf-8")
  )
  res <- GET(hb("/get"))
  expect_s3_class(res, "response")
  expect_type(content(res), "list")
  expect_named(content(res), "foo")
  expect_equal(content(res)$foo, "bar")
  disable(quiet = TRUE)
  httr_mock(FALSE)
})

test_that("httr: with pipe", {
  enable(quiet = TRUE)
  dat_json <- '{"foo":"bar"}'
  stub <- stub_request("get", uri = hb("/get")) %>%
    to_return(
      body = dat_json,
      headers = list("Content-Type" = "application/json; charset=utf-8")
    )
  res <- GET(hb("/get"))
  expect_s3_class(res, "response")
  expect_type(content(res), "list")
  expect_named(content(res), "foo")
  expect_equal(content(res)$foo, "bar")
  disable(quiet = TRUE)
})
unloadNamespace("httr")


test_that("crul works", {
  enable(quiet = TRUE)
  dat_json <- '{"foo":"bar"}'
  stub <- stub_request("get", uri = hb("/get"))
  to_return(
    stub,
    body = dat_json,
    headers = list("Content-Type" = "application/json; howdy")
  )
  res <- crul::HttpClient$new(hb())$get("get")
  expect_s3_class(res, "HttpResponse")
  expect_type(res$parse("UTF-8"), "character")
  expect_type(jsonlite::fromJSON(res$parse("UTF-8")), "list")
  expect_named(jsonlite::fromJSON(res$parse("UTF-8")), "foo")
  expect_equal(jsonlite::fromJSON(res$parse("UTF-8"))$foo, "bar")
  disable(quiet = TRUE)
})
