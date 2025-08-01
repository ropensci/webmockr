skip_if_not_installed("httr2")
library("httr2")

aa <- Httr2Adapter$new()

test_that("Httr2Adapter bits are correct", {
  skip_on_cran()

  expect_s3_class(Httr2Adapter, "R6ClassGenerator")

  expect_s3_class(aa, "Httr2Adapter")
  expect_null(aa$build_httr_request) # pulled out of object, so should be NULL
  expect_null(aa$build_httr_response) # pulled out of object, so should be NULL
  expect_type(aa$disable, "closure")
  expect_type(aa$enable, "closure")
  expect_type(aa$handle_request, "closure")
  expect_type(aa$remove_stubs, "closure")
  expect_type(aa$name, "character")

  expect_equal(aa$name, "Httr2Adapter")
})


test_that("Httr2Adapter behaves correctly", {
  skip_on_cran()

  expect_message(aa$enable(), "Httr2Adapter enabled!")
  expect_message(aa$disable(), "Httr2Adapter disabled!")
})


test_that("build_httr_request/response fail well", {
  skip_on_cran()

  expect_error(build_httr_request(), "argument \"x\" is missing")
  expect_error(build_httr_response(), "argument \"req\" is missing")
})


# library(httr2)
# z <- request(hb("/get")) %>% req_perform()
# httr2_obj <- z$request
# save(httr2_obj, file = "tests/testthat/httr2_obj.rda", version = 2)

test_that("Httr2Adapter works", {
  skip_on_cran()

  load("httr2_obj.rda")
  # load("tests/testthat/httr2_obj.rda")
  res <- Httr2Adapter$new()

  # with webmockr message
  invisible(stub_request("get", hb("/get")))

  aa <- res$handle_request(httr2_obj)

  expect_s3_class(res, "Httr2Adapter")
  expect_s3_class(aa, "httr2_response")
  expect_null(aa$request$method)
  expect_equal(aa$url, hb("/get"))

  # no response headers
  expect_equal(length(aa$headers), 0)

  # with headers
  # clear registry
  stub_registry_clear()

  # stub with headers
  x <- stub_request("get", hb("/get"))
  x <- to_return(x, headers = list("User-Agent" = "foo-bar"))

  aa <- res$handle_request(httr2_obj)

  expect_s3_class(res, "Httr2Adapter")
  expect_s3_class(aa, "httr2_response")
  expect_null(aa$request$method)
  expect_equal(aa$url, hb("/get"))

  # has headers and all_headers
  expect_equal(length(aa$headers), 1)
  expect_s3_class(aa$headers, "httr2_headers")
  expect_named(aa$headers, "user-agent")

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

  httr2_obj$url <- my_url
  res <- Httr2Adapter$new()
  aa <- res$handle_request(httr2_obj)

  expect_null(aa$request$method)
  expect_equal(aa$url, my_url)
  expect_equal(aa$status_code, 302)

  # has headers and all_headers
  expect_equal(length(aa$headers), 2)
  expect_s3_class(aa$headers, "httr2_headers")
  expect_equal(sort(names(aa$headers)), c("location", "status"))
})

test_that("Httr2Adapter works with req_auth_basic", {
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

  # mocked httr2 requests with auth work
  x <- request(hb("/basic-auth/foo/bar")) %>%
    req_auth_basic("foo", "bar") %>%
    req_perform()
  expect_s3_class(x, "httr2_response")
  expect_s3_class(x$headers, "httr2_headers")
  expect_equal(x$status_code, 200)

  # Httr2Adapter works on requests with auth
  # x <- request(hb("/basic-auth/foo/bar")) %>%
  #   req_auth_basic("foo", "bar") %>%
  #   req_perform()
  # httr2_obj_auth <- x$request
  # save(httr2_obj_auth, file = "tests/testthat/httr2_obj_auth.rda", version = 3)
  # load("tests/testthat/httr2_obj_auth.rda")
  load("httr2_obj_auth.rda")
  zz <- Httr2Adapter$new()
  z <- zz$handle_request(httr2_obj_auth)
  expect_s3_class(z, "httr2_response")
  expect_equal(
    jsonlite::fromJSON(rawToChar(z$body)),
    list(foo = "bar")
  )
  expect_s3_class(z$headers, "httr2_headers")
  expect_equal(z$status_code, 200)
})

test_that("httr2 works with webmockr_allow_net_connect", {
  skip_on_cran()

  enable(quiet = TRUE)
  stub_registry_clear()
  z <- stub_request("get", uri = hb("/get")) %>%
    wi_th(query = list(stuff = "things")) %>%
    to_return(body = "yum=cheese")
  req <- request(hb("/get")) %>% req_url_query(stuff = "things")
  x <- req_perform(req)
  expect_true(resp_body_string(x) == "yum=cheese")

  # disable net connect - now real requests can't be made
  suppressMessages(webmockr_disable_net_connect())
  stub_registry_clear()
  expect_error(
    req_perform(req),
    "Real HTTP connections are disabled"
  )

  # allow net connect - stub still exists though - so not a real request
  sm(webmockr_allow_net_connect())
  z <- stub_request("get", uri = hb("/get")) %>%
    wi_th(query = list(stuff = "things")) %>%
    to_return(body = "yum=cheese")
  req <- request(hb("/get")) %>% req_url_query(stuff = "things")
  z <- req_perform(req)
  expect_true(resp_body_string(z) == "yum=cheese")

  # allow net connect - stub now gone - so real request should happen
  stub_registry_clear()
  req <- request(hb("/get")) %>% req_url_query(stuff = "things")
  httr2::local_mocked_responses(NULL)
  w <- req_perform(req)
  expect_false(resp_body_string(w) == "yum=cheese")
})

test_that("httr2 requests with bodies work", {
  skip_on_cran()

  enable(quiet = TRUE)
  stub_registry_clear()
  z <- stub_request("post", uri = hb("/post")) %>%
    to_return(body = "asdffsdsdf")
  req <- request(hb("/post")) %>%
    req_body_json(list(stuff = "things"))
  x <- req_perform(req)
  expect_true(httr2::resp_body_string(x) == "asdffsdsdf")

  # now with allow net connect
  stub_registry_clear()
  httr2_mock(FALSE)
  sm(webmockr_allow_net_connect())
  req <- request(hb("/post")) %>%
    req_body_json(list(stuff = "things"))
  x <- req_perform(req)
  expect_identical(httr2::resp_body_json(x)$json, list(stuff = "things"))

  suppressMessages(webmockr_disable_net_connect())
})

disable(quiet = TRUE)

test_that("httr2 requests with nested list bodies work", {
  skip_on_cran()

  enable(quiet = TRUE)
  # httr_mock()
  stub_registry_clear()
  body <- list(id = " ", method = "x", params = list(pwd = "p", user = "a"))
  z <- stub_request("post", uri = hb("/post")) %>%
    wi_th(body = body) %>%
    to_return(body = "asdffsdsdf")
  x <- request(hb("/post")) %>%
    req_body_json(body) %>%
    req_perform()
  expect_true(rawToChar(x$body) == "asdffsdsdf")

  # now with allow net connect
  stub_registry_clear()
  sm(webmockr_allow_net_connect())
  response_real <- request(hb("/post")) %>%
    req_body_json(body) %>%
    req_perform()
  expect_equal(
    jsonlite::fromJSON(rawToChar(response_real$body))$json,
    body
  )

  suppressMessages(webmockr_disable_net_connect())
})
