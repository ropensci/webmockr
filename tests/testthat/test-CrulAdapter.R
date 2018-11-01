context("CrulAdapter")

aa <- CrulAdapter$new()

test_that("CrulAdapter bits are correct", {
  skip_on_cran()

  expect_is(CrulAdapter, "R6ClassGenerator")

  expect_is(aa, "CrulAdapter")
  expect_null(aa$build_crul_request) # pulled out of object, so should be NULL
  expect_null(aa$build_crul_response) # pulled out of object, so should be NULL
  expect_is(aa$disable, "function")
  expect_is(aa$enable, "function")
  expect_is(aa$handle_request, "function")
  expect_is(aa$remove_crul_stubs, "function")
  expect_is(aa$name, "character")

  expect_equal(aa$name, "crul_adapter")
})


test_that("CrulAdapter behaves correctly", {
  skip_on_cran()

  expect_message(aa$enable(), "CrulAdapter enabled!")
  expect_message(aa$disable(), "CrulAdapter disabled!")
})


test_that("build_crul_request/response fail well", {
  skip_on_cran()

  expect_error(build_crul_request(), "argument \"x\" is missing")
  expect_error(build_crul_response(), "argument \"req\" is missing")
})


context("CrulAdapter - with real data")
test_that("CrulAdapter works", {
  skip_on_cran()

  load("crul_obj.rda")
  crul_obj$url$handle <- curl::new_handle()
  res <- CrulAdapter$new()

  # with vcr message
  library(vcr)
  expect_error(
    res$handle_request(crul_obj),
    "There is currently no cassette in use"
  )

  # with webmockr message
  # unload vcr
  unloadNamespace("vcr")
  expect_error(
    res$handle_request(crul_obj),
    "Real HTTP connections are disabled.\nUnregistered request:\n  GET:  http://localhost:9000/get\n\nYou can stub this request with the following snippet:\n\n   stub_request\\('get', uri = 'http://localhost:9000/get'\\)\n============================================================"
  )

  invisible(stub_request("get", "http://localhost:9000/get"))

  aa <- res$handle_request(crul_obj)

  expect_is(res, "CrulAdapter")
  expect_is(aa, "HttpResponse")
  expect_equal(aa$method, "get")
  expect_equal(aa$url, "http://localhost:9000/get")
})
