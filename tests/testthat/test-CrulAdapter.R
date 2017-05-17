context("CrulAdapter")

aa <- CrulAdapter$new()

test_that("CrulAdapter bits are correct", {
  skip_on_cran()

  expect_is(CrulAdapter, "R6ClassGenerator")

  expect_is(aa, "CrulAdapter")
  expect_is(aa$build_crul_request, "function")
  expect_is(aa$build_crul_response, "function")
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


test_that("CrulAdapter fails well", {
  skip_on_cran()

  expect_error(aa$build_crul_request(), "argument \"x\" is missing")
  expect_error(aa$build_crul_response(), "argument \"req\" is missing")
})


context("CrulAdapter - with real data")
test_that("CrulAdapter works", {
  skip_on_cran()

  load("crul_obj.rda")

  res <- CrulAdapter$new()
  expect_error(
    res$handle_request(crul_obj),
    "Real HTTP connections are disabled.\nUnregistered request: GET http://localhost:9000/get\n\nYou can stub this request with the following snippet"
  )

  invisible(stub_request("get", "http://localhost:9000/get"))

  aa <- res$handle_request(crul_obj)

  expect_is(res, "CrulAdapter")
  expect_is(aa, "HttpResponse")
  expect_equal(aa$method, "get")
  expect_equal(aa$url, "http://localhost:9000/get")
})
