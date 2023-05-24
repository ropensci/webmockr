context("Httr2Adapter")

skip_if_not_installed("httr2")
library("httr2")

aa <- Httr2Adapter$new()

test_that("Httr2Adapter bits are correct", {
  skip_on_cran()

  expect_is(Httr2Adapter, "R6ClassGenerator")

  expect_is(aa, "Httr2Adapter")
  expect_null(aa$build_httr_request) # pulled out of object, so should be NULL
  expect_null(aa$build_httr_response) # pulled out of object, so should be NULL
  expect_is(aa$disable, "function")
  expect_is(aa$enable, "function")
  expect_is(aa$handle_request, "function")
  expect_is(aa$remove_stubs, "function")
  expect_is(aa$name, "character")

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

# test_that("Httr2Adapter: works when vcr is loaded but no cassette is inserted", {
#   skip_on_cran()
#   skip_if_not_installed("vcr")

#   webmockr::enable(adapter = "httr2")
#   on.exit({
#     webmockr::disable(adapter = "httr2")
#     unloadNamespace("vcr")
#   })

#   stub_request("get", "https://hb.opencpu.org/get")
#   library("vcr")

#   # works when no cassette is loaded
#   expect_silent(x <- httr::GET("https://hb.opencpu.org/get"))
#   expect_is(x, "response")

#   # # works when empty cassette is loaded
#   vcr::vcr_configure(dir = tempdir())
#   vcr::insert_cassette("empty")
#   expect_silent(x <- httr::GET("https://hb.opencpu.org/get"))
#   vcr::eject_cassette("empty")
#   expect_is(x, "response")
# })

# # library(httr)
# # z <- GET("https://hb.opencpu.org/get")
# # httr_obj <- z$request
# # save(httr_obj, file = "tests/testthat/httr_obj.rda")

# context("Httr2Adapter: date slot")
# test_that("Httr2Adapter date slot works", {
#   skip_on_cran()
#   skip_if_not_installed("vcr")
#   library("vcr")

#   path <- file.path(tempdir(), "foobar")
#   vcr::vcr_configure(dir = path)
#   vcr::use_cassette("test-date", httr::GET("https://hb.opencpu.org/get"))
#   # list.files(path)
#   # readLines(file.path(path, "test-date.yml"))
#   vcr::insert_cassette("test-date")

#   x <- httr::GET("https://hb.opencpu.org/get")

#   # $date is of correct format
#   expect_output(print(x), "Date")
#   expect_is(x$date, "POSIXct")
#   expect_is(format(x$date, "%Y-%m-%d %H:%M"), "character")

#   # $headers$date is a different format
#   expect_is(x$headers$date, "character")
#   expect_error(format(x$headers$date, "%Y-%m-%d %H:%M"), "invalid 'trim'")

#   vcr::eject_cassette("test-date")

#   # cleanup
#   unlink(path, recursive = TRUE)
# })



# context("Httr2Adapter: works with real data")
# test_that("Httr2Adapter works", {
#   skip_on_cran()
#   skip_if_not_installed("vcr")

#   load("httr_obj.rda")
#   # load("tests/testthat/httr_obj.rda")
#   res <- Httr2Adapter$new()

#   # with vcr message
#   library("vcr")
#   expect_error(
#     res$handle_request(httr_obj),
#     "There is currently no cassette in use"
#   )

#   # with webmockr message
#   # unload vcr
#   unloadNamespace("vcr")
#   expect_error(
#     res$handle_request(httr_obj),
#     "Real HTTP connections are disabled.\nUnregistered request:\n  GET:  https://hb.opencpu.org/get"
#   )

#   invisible(stub_request("get", "https://hb.opencpu.org/get"))

#   aa <- res$handle_request(httr_obj)

#   expect_is(res, "Httr2Adapter")
#   expect_is(aa, "response")
#   expect_equal(aa$request$method, "GET")
#   expect_equal(aa$url, "https://hb.opencpu.org/get")

#   # no response headers
#   expect_equal(length(aa$headers), 0)
#   expect_equal(length(aa$all_headers), 1)


#   # with headers
#   # clear registry
#   stub_registry_clear()

#   # stub with headers
#   x <- stub_request("get", "https://hb.opencpu.org/get")
#   x <- to_return(x, headers = list("User-Agent" = "foo-bar"))

#   aa <- res$handle_request(httr_obj)

#   expect_is(res, "Httr2Adapter")
#   expect_is(aa, "response")
#   expect_equal(aa$request$method, "GET")
#   expect_equal(aa$url, "https://hb.opencpu.org/get")

#   # has headers and all_headers
#   expect_equal(length(aa$headers), 1)
#   expect_is(aa$headers, "list")
#   expect_named(aa$headers, "user-agent")
#   expect_equal(length(aa$all_headers), 1)
#   expect_is(aa$all_headers, "list")
#   expect_named(aa$all_headers, NULL)
#   expect_named(aa$all_headers[[1]], c("status", "version", "headers"))


#   # stub with redirect headers
#   my_url <- "https://doi.org/10.1007/978-3-642-40455-9_52-1"
#   x <- stub_request("get", my_url)
#   x <- to_return(x, status = 302, headers =
#     list(
#       status = 302,
#       location = "http://link.springer.com/10.1007/978-3-642-40455-9_52-1"
#     )
#   )

#   httr_obj$url <- my_url
#   res <- Httr2Adapter$new()
#   aa <- res$handle_request(httr_obj)

#   expect_equal(aa$request$method, "GET")
#   expect_equal(aa$url, my_url)
#   expect_equal(aa$status_code, 302)

#   # has headers and all_headers
#   expect_equal(length(aa$headers), 2)
#   expect_is(aa$headers, "list")
#   expect_equal(sort(names(aa$headers)), c("location", "status"))
#   expect_equal(length(aa$all_headers), 1)
#   expect_equal(length(aa$all_headers[[1]]), 3)
#   expect_is(aa$all_headers, "list")
#   expect_is(aa$all_headers[[1]], "list")
#   expect_named(aa$all_headers, NULL)
#   expect_equal(sort(names(aa$all_headers[[1]])),
#     c("headers", "status", "version"))
# })

# test_that("Httr2Adapter works with httr::authenticate", {
#   skip_on_cran()

#   unloadNamespace("vcr")
#   httr_mock()
#   # httr_mock(FALSE)
#   # webmockr_allow_net_connect()
#   stub_registry_clear()
#   # stub_registry()
#   # request_registry()
#   z <- stub_request("get", uri = "https://hb.opencpu.org/basic-auth/foo/bar") %>%
#       to_return(
#         body = list(foo = "bar"),
#         headers = list("Content-Type" = "application/json")
#       )
#   # x <- httr::GET("https://hb.opencpu.org/basic-auth/foo/bar", httr::authenticate("foo", "bar"))
#   # httr_obj_auth <- x$request
#   # save(httr_obj_auth, file = "tests/testthat/httr_obj_auth.rda", version = 2)
#   # load("tests/testthat/httr_obj_auth.rda")
#   # httr::content(x)

#   # mocked httr2 requests with auth work
#   # before the fixes in Httr2Adapter: a real request through webmockr would
#   #   not work with authenticate
#   x <- httr::GET("https://hb.opencpu.org/basic-auth/foo/bar", httr::authenticate("foo", "bar"))
#   expect_is(x, "response")
#   expect_equal(httr::content(x), list(foo = "bar"))
#   expect_equal(x$headers, structure(list(`content-type` = "application/json"),
#     class = c("insensitive", "list")))
#   expect_equal(x$status_code, 200)

#   # Httr2Adapter works on requests with auth
#   load("httr_obj_auth.rda")
#   zz <- Httr2Adapter$new()
#   z <- zz$handle_request(httr_obj_auth)
#   expect_is(z, "response")
#   expect_equal(httr::content(z), list(foo = "bar"))
#   expect_equal(z$headers, structure(list(`content-type` = "application/json"),
#     class = c("insensitive", "list")))
#   expect_equal(z$status_code, 200)
# })

test_that("httr2 works with webmockr_allow_net_connect", {
  skip_on_cran()

  httr2_mock()
  stub_registry_clear()
  z <- stub_request("get", uri = "https://hb.opencpu.org/get?stuff=things") %>%
    to_return(body = "yum=cheese")
  req <- request("https://hb.opencpu.org/get?stuff=things")
  x <- req_perform(req, mock = ~ mock_httr2(req))
  expect_true(resp_body_string(x) == "yum=cheese")

  # allow net connect - stub still exists though - so not a real request
  webmockr_allow_net_connect()
  req <- httr2::request("https://hb.opencpu.org/get?stuff=things")
  z <- req_perform(req, mock = ~ mock_httr2(req))
  expect_true(httr2::resp_body_string(z) == "yum=cheese")

  # allow net connect - stub now gone - so real request should happen
  stub_registry_clear()
  req <- httr2::request("https://hb.opencpu.org/get?stuff=things")
  w <- req_perform(req, mock = ~ mock_httr2(req))
  expect_false(httr2::resp_body_string(w) == "yum=cheese")

  # disable net connect - now real requests can't be made
  webmockr_disable_net_connect()
  expect_error(httr2::req_perform(req, mock = ~ mock_httr2(req)),
    "Real HTTP connections are disabled")
})

test_that("httr2 requests with bodies work", {
  skip_on_cran()

  httr2_mock()
  stub_registry_clear()
  z <- stub_request("post", uri = "https://hb.opencpu.org/post") %>%
    to_return(body = "asdffsdsdf")
  req <- request("https://hb.opencpu.org/post") %>% 
    req_body_json(list(stuff = "things"))
  x <- req_perform(req, mock = ~ mock_httr2(req))
  expect_true(httr2::resp_body_string(x) == "asdffsdsdf")

  # now with allow net connect
  stub_registry_clear()
  httr2_mock(FALSE)
  webmockr_allow_net_connect()
  req <- request("https://hb.opencpu.org/post") %>% 
    req_body_json(list(stuff = "things"))
  x <- req_perform(req)
  expect_identical(httr2::resp_body_json(x)$json, list(stuff = "things"))

  webmockr_disable_net_connect()
})

# test_that("httr2 requests with nested list bodies work", {
#   skip_on_cran()

#   httr_mock()
#   stub_registry_clear()
#   body = list(id = ' ', method = 'x', params = list(pwd = 'p', user = 'a'))
#   z <- stub_request("post", uri = "https://hb.opencpu.org/post") %>%
#     wi_th(body = body) %>%
#     to_return(body = "asdffsdsdf")
#   x <- httr::POST("https://hb.opencpu.org/post", body = body)
#   expect_true(httr::content(x, "text", encoding="UTF-8") == "asdffsdsdf")

#   # now with allow net connect
#   stub_registry_clear()
#   webmockr_allow_net_connect()
#   x <- httr::POST("https://hb.opencpu.org/post",
#     body = jsonlite::toJSON(body), httr::content_type_json())
#   expect_equal(
#     jsonlite::fromJSON(rawToChar(x$content))$json,
#     body)

#   webmockr_disable_net_connect()
# })

# test_that("httr2 requests with JSON-encoded bodies work", {
#   skip_on_cran()

#   on.exit(disable(adapter = "httr2"))
#   enable(adapter = "httr2")

#   stub_registry_clear()
#   body <- list(foo = "bar")
#   z <- stub_request("post", uri = "https://hb.opencpu.org/post") %>%
#     wi_th(body = jsonlite::toJSON(body, auto_unbox = TRUE))

#   # encoded body works
#   req <- request("https://hb.opencpu.org/post") %>% 
#     req_body_json(body)
#   res <- req_perform(req, mock = ~ mock_httr2(req))
#   expect_is(res, "response")

#   # encoded but modified body fails
#   req2 <- request("https://hb.opencpu.org/post") %>% req_body_json(list(foo = "bar1"))
#   expect_error(
#     req_perform(req2, mock = ~ mock_httr2(req)),
#     "Unregistered request"
#   )

#   # unencoded body fails
#   req3 <- request("https://hb.opencpu.org/post") %>% req_body_json(body)
#   expect_error(
#     req_perform(req3, mock = ~ mock_httr2(req)),
#     "Unregistered request"
#   )
# })
