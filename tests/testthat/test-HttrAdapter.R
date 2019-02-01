context("HttrAdapter")

skip_if_not_installed("httr")
library("httr")

aa <- HttrAdapter$new()

test_that("HttrAdapter bits are correct", {
  skip_on_cran()

  expect_is(HttrAdapter, "R6ClassGenerator")

  expect_is(aa, "HttrAdapter")
  expect_null(aa$build_httr_request) # pulled out of object, so should be NULL
  expect_null(aa$build_httr_response) # pulled out of object, so should be NULL
  expect_is(aa$disable, "function")
  expect_is(aa$enable, "function")
  expect_is(aa$handle_request, "function")
  expect_is(aa$remove_httr_stubs, "function")
  expect_is(aa$name, "character")

  expect_equal(aa$name, "httr_adapter")
})


test_that("HttrAdapter behaves correctly", {
  skip_on_cran()

  expect_message(aa$enable(), "HttrAdapter enabled!")
  expect_message(aa$disable(), "HttrAdapter disabled!")
})


test_that("build_httr_request/response fail well", {
  skip_on_cran()

  expect_error(build_httr_request(), "argument \"x\" is missing")
  expect_error(build_httr_response(), "argument \"req\" is missing")
})

# library(httr)
# z <- GET("https://httpbin.org/get")
# httr_obj <- z$request
# save(httr_obj, file = "tests/testthat/httr_obj.rda")

context("HttrAdapter: date slot")
test_that("HttrAdapter date slot works", {
  skip_on_cran()
  skip_if_not_installed('vcr')
  library("vcr")

  path <- file.path(tempdir(), "foobar")
  vcr::vcr_configure(dir = path)
  vcr::use_cassette("test-date", GET("https://httpbin.org/get"))
  # list.files(path)
  # readLines(file.path(path, "test-date.yml"))
  vcr::insert_cassette("test-date")

  x <- GET("https://httpbin.org/get")

  # $date is of correct format
  expect_output(print(x), "Date")
  expect_is(x$date, "POSIXct")
  expect_is(format(x$date, "%Y-%m-%d %H:%M"), "character")

  # $headers$date is a different format
  expect_is(x$headers$date, "character")
  expect_error(format(x$headers$date, "%Y-%m-%d %H:%M"), "invalid 'trim'")

  vcr::eject_cassette("test-date")

  # cleanup
  unlink(path, recursive = TRUE)
})

context("HttrAdapter: works with real data")
test_that("HttrAdapter works", {
  skip_on_cran()
  skip_if_not_installed('vcr')

  load("httr_obj.rda")
  # load("tests/testthat//httr_obj.rda")
  res <- HttrAdapter$new()

  # with vcr message
  library("vcr")
  expect_error(
    res$handle_request(httr_obj),
    "There is currently no cassette in use"
  )

  # with webmockr message
  # unload vcr
  unloadNamespace("vcr")
  expect_error(
    res$handle_request(httr_obj),
    "Real HTTP connections are disabled.\nUnregistered request:\n  GET:  https://httpbin.org/get"
  )

  invisible(stub_request("get", "https://httpbin.org/get"))

  aa <- res$handle_request(httr_obj)

  expect_is(res, "HttrAdapter")
  expect_is(aa, "response")
  expect_equal(aa$request$method, "GET")
  expect_equal(aa$url, "https://httpbin.org/get")

  # no response headers
  expect_equal(length(aa$headers), 0)
  expect_equal(length(aa$all_headers), 1)


  # with headers
  # clear registry
  stub_registry_clear()

  # stub with headers
  x <- stub_request("get", "https://httpbin.org/get")
  x <- to_return(x, headers = list("User-Agent" = "foo-bar"))

  aa <- res$handle_request(httr_obj)

  expect_is(res, "HttrAdapter")
  expect_is(aa, "response")
  expect_equal(aa$request$method, "GET")
  expect_equal(aa$url, "https://httpbin.org/get")

  # has headers and all_headers
  expect_equal(length(aa$headers), 1)
  expect_is(aa$headers, "list")
  expect_named(aa$headers, "user-agent")
  expect_equal(length(aa$all_headers), 1)
  expect_is(aa$all_headers, "list")
  expect_named(aa$all_headers, NULL)
  expect_named(aa$all_headers[[1]], c("status", "version", "headers"))


  # stub with redirect headers
  my_url <- "https://doi.org/10.1007/978-3-642-40455-9_52-1"
  x <- stub_request("get", my_url)
  x <- to_return(x, status = 302, headers =
    list(
      status = 302,
      location = "http://link.springer.com/10.1007/978-3-642-40455-9_52-1"
    )
  )

  httr_obj$url <- my_url
  res <- HttrAdapter$new()
  aa <- res$handle_request(httr_obj)

  expect_equal(aa$request$method, "GET")
  expect_equal(aa$url, my_url)
  expect_equal(aa$status_code, 302)

  # has headers and all_headers
  expect_equal(length(aa$headers), 2)
  expect_is(aa$headers, "list")
  expect_equal(sort(names(aa$headers)), c("location", "status"))
  expect_equal(length(aa$all_headers), 1)
  expect_equal(length(aa$all_headers[[1]]), 3)
  expect_is(aa$all_headers, "list")
  expect_is(aa$all_headers[[1]], "list")
  expect_named(aa$all_headers, NULL)
  expect_equal(sort(names(aa$all_headers[[1]])),
    c("headers", "status", "version"))
})
