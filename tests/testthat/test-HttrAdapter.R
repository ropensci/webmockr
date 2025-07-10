skip_if_not_installed("httr")
suppressPackageStartupMessages(library("httr", warn.conflicts = FALSE))

aa <- HttrAdapter$new()

test_that("HttrAdapter bits are correct", {
  skip_on_cran()

  expect_s3_class(HttrAdapter, "R6ClassGenerator")

  expect_s3_class(aa, "HttrAdapter")
  expect_null(aa$build_httr_request) # pulled out of object, so should be NULL
  expect_null(aa$build_httr_response) # pulled out of object, so should be NULL
  expect_type(aa$disable, "closure")
  expect_type(aa$enable, "closure")
  expect_type(aa$handle_request, "closure")
  expect_type(aa$remove_stubs, "closure")
  expect_type(aa$name, "character")

  expect_equal(aa$name, "HttrAdapter")
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
# z <- GET(hb("/get"))
# httr_obj <- z$request
# save(httr_obj, file = "tests/testthat/httr_obj.rda", version = 2)

# test_that("HttrAdapter date slot works", {
#   skip_on_cran()

#   # $date is of correct format
#   expect_output(print(x), "Date")
#   expect_s3_class(x$date, "POSIXct")
#   expect_type(format(x$date, "%Y-%m-%d %H:%M"), "character")

#   # $headers$date is a different format
#   expect_type(x$headers$date, "character")
#   expect_error(format(x$headers$date, "%Y-%m-%d %H:%M"), "invalid 'trim'")
# })

test_that("HttrAdapter insensitive headers work, webmockr flow", {
  skip_on_cran()
  httr_mock()
  stub_registry_clear()
  invisible(
    stub_request("get", uri = hb("/get")) %>%
      to_return(
        body = list(foo = "bar"),
        headers = list("Content-Type" = "application/json")
      )
  )
  x <- httr::GET(hb("/get"))

  expect_equal(x$headers[["content-type"]], "application/json")
  expect_type(httr::content(x), "list")
  expect_type(httr::content(x, "text", encoding = "UTF-8"), "character")

  stub_registry_clear()
  httr_mock(FALSE)
})

test_that("HttrAdapter works", {
  skip_on_cran()

  load("httr_obj.rda")
  # load("tests/testthat/httr_obj.rda")
  res <- HttrAdapter$new()

  # with webmockr message
  invisible(stub_request("get", hb("/get")))

  aa <- res$handle_request(httr_obj)

  expect_s3_class(res, "HttrAdapter")
  expect_s3_class(aa, "response")
  expect_equal(aa$request$method, "GET")
  expect_equal(aa$url, hb("/get"))

  # no response headers
  expect_equal(length(aa$headers), 0)
  expect_equal(length(aa$all_headers), 1)

  # with headers
  # clear registry
  stub_registry_clear()

  # stub with headers
  x <- stub_request("get", hb("/get"))
  x <- to_return(x, headers = list("User-Agent" = "foo-bar"))

  aa <- res$handle_request(httr_obj)

  expect_s3_class(res, "HttrAdapter")
  expect_s3_class(aa, "response")
  expect_equal(aa$request$method, "GET")
  expect_equal(aa$url, hb("/get"))

  # has headers and all_headers
  expect_equal(length(aa$headers), 1)
  expect_type(aa$headers, "list")
  expect_named(aa$headers, "user-agent")
  expect_equal(length(aa$all_headers), 1)
  expect_type(aa$all_headers, "list")
  expect_named(aa$all_headers, NULL)
  expect_named(aa$all_headers[[1]], c("status", "version", "headers"))

  # stub with redirect headers
  my_url <- "https://doi.org/10.1007/978-3-642-40455-9_52-1"
  x <- stub_request("get", my_url)
  x <- to_return(
    x,
    status = 302,
    headers = list(
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
  expect_type(aa$headers, "list")
  expect_equal(sort(names(aa$headers)), c("location", "status"))
  expect_equal(length(aa$all_headers), 1)
  expect_equal(length(aa$all_headers[[1]]), 3)
  expect_type(aa$all_headers, "list")
  expect_type(aa$all_headers[[1]], "list")
  expect_named(aa$all_headers, NULL)
  expect_equal(
    sort(names(aa$all_headers[[1]])),
    c("headers", "status", "version")
  )
})

test_that("HttrAdapter works with httr::authenticate", {
  skip_on_cran()

  httr_mock()
  # httr_mock(FALSE)
  # sm(webmockr_allow_net_connect())
  stub_registry_clear()
  # stub_registry()
  # request_registry()
  z <- stub_request("get", uri = hb("/basic-auth/foo/bar")) %>%
    to_return(
      body = list(foo = "bar"),
      headers = list("Content-Type" = "application/json")
    )
  # x <- httr::GET(hb("/basic-auth/foo/bar"), httr::authenticate("foo", "bar"))
  # httr_obj_auth <- x$request
  # save(httr_obj_auth, file = "tests/testthat/httr_obj_auth.rda", version = 3)
  # load("tests/testthat/httr_obj_auth.rda")

  # mocked httr requests with auth work
  # before the fixes in HttrAdapter: a real request through webmockr would
  #   not work with authenticate
  x <- httr::GET(hb("/basic-auth/foo/bar"), httr::authenticate("foo", "bar"))
  expect_s3_class(x, "response")
  expect_equal(httr::content(x), list(foo = "bar"))
  expect_equal(
    x$headers,
    structure(
      list(`content-type` = "application/json"),
      class = c("insensitive", "list")
    )
  )
  expect_equal(x$status_code, 200)

  # HttrAdapter works on requests with auth
  load("httr_obj_auth.rda")
  zz <- HttrAdapter$new()
  z <- zz$handle_request(httr_obj_auth)
  expect_s3_class(z, "response")
  expect_equal(httr::content(z), list(foo = "bar"))
  expect_equal(
    z$headers,
    structure(
      list(`content-type` = "application/json"),
      class = c("insensitive", "list")
    )
  )
  expect_equal(z$status_code, 200)
})

test_that("httr works with webmockr_allow_net_connect", {
  skip_on_cran()

  httr_mock()
  stub_registry_clear()
  z <- stub_request("get", uri = hb("/get?stuff=things")) %>%
    to_return(body = "yum=cheese")
  x <- httr::GET(hb("/get?stuff=things"))
  expect_true(httr::content(x, "text", encoding = "UTF-8") == "yum=cheese")

  # allow net connect - stub still exists though - so not a real request
  sm(webmockr_allow_net_connect())
  z <- httr::GET(hb("/get?stuff=things"))
  expect_true(httr::content(z, "text", encoding = "UTF-8") == "yum=cheese")

  # allow net connect - stub now gone - so real request should happen
  stub_registry_clear()
  w <- httr::GET(hb("/get?stuff=things"))
  expect_false(httr::content(w, "text", encoding = "UTF-8") == "yum=cheese")

  # disable net connect - now real requests can't be made
  suppressMessages(webmockr_disable_net_connect())
  expect_error(
    httr::GET(hb("/get?stuff=things")),
    "Real HTTP connections are disabled"
  )
})

test_that("httr requests with bodies work", {
  skip_on_cran()

  httr_mock()
  stub_registry_clear()
  z <- stub_request("post", uri = hb("/post")) %>%
    to_return(body = "asdffsdsdf")
  x <- httr::POST(hb("/post"), body = list(stuff = "things"))
  expect_true(httr::content(x, "text", encoding = "UTF-8") == "asdffsdsdf")

  # now with allow net connect
  stub_registry_clear()
  sm(webmockr_allow_net_connect())
  x <- httr::POST(hb("/post"), body = list(stuff = "things"))
  expect_identical(httr::content(x)$form, list(stuff = "things"))

  suppressMessages(webmockr_disable_net_connect())
})

test_that("httr requests with nested list bodies work", {
  skip_on_cran()

  httr_mock()
  stub_registry_clear()
  body <- list(id = " ", method = "x", params = list(pwd = "p", user = "a"))
  z <- stub_request("post", uri = hb("/post")) %>%
    wi_th(body = body) %>%
    to_return(body = "asdffsdsdf")
  x <- httr::POST(hb("/post"), body = body)
  expect_true(httr::content(x, "text", encoding = "UTF-8") == "asdffsdsdf")

  # now with allow net connect
  stub_registry_clear()
  sm(webmockr_allow_net_connect())
  x <- httr::POST(
    hb("/post"),
    body = jsonlite::toJSON(body),
    httr::content_type_json()
  )
  expect_equal(
    jsonlite::fromJSON(rawToChar(x$content))$json,
    body
  )

  suppressMessages(webmockr_disable_net_connect())
})

test_that("httr requests with JSON-encoded bodies work", {
  skip_on_cran()

  on.exit(disable(adapter = "httr", quiet = TRUE))
  enable(adapter = "httr", quiet = TRUE)

  stub_registry_clear()
  body <- list(foo = "bar")
  z <- stub_request("post", uri = hb("/post")) %>%
    wi_th(body = jsonlite::toJSON(body, auto_unbox = TRUE))

  # encoded body works
  res <- httr::POST(hb("/post"), body = body, encode = "json")
  expect_s3_class(res, "response")

  # encoded but modified body fails
  expect_error(
    httr::POST(hb("/post"), body = list(foo = "bar1"), encode = "json"),
    "Unregistered request"
  )

  # unencoded body should work because we serialize internally
  expect_s3_class(httr::POST(hb("/post"), body = body), "response")
})
