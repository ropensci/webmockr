context("CurlAdapter")

aa <- CurlAdapter$new()

test_that("CurlAdapter bits are correct", {
  skip_on_cran()

  expect_is(CurlAdapter, "R6ClassGenerator")

  expect_is(aa, "CurlAdapter")
  expect_null(aa$build_curl_request) # pulled out of object, so should be NULL
  expect_null(aa$build_curl_response) # pulled out of object, so should be NULL
  expect_is(aa$disable, "function")
  expect_is(aa$enable, "function")
  expect_is(aa$handle_request, "function")
  expect_is(aa$remove_curl_stubs, "function")
  expect_is(aa$name, "character")

  expect_equal(aa$name, "curl_adapter")
})


test_that("CurlAdapter behaves correctly", {
  skip_on_cran()

  expect_message(aa$enable(), "CurlAdapter enabled!")
  expect_message(aa$disable(), "CurlAdapter disabled!")
})


test_that("build_curl_request/response fail well", {
  skip_on_cran()

  expect_error(build_curl_request(), "argument \"x\" is missing")
  expect_error(build_curl_response(), "argument \"resp\" is missing")
})


context("CurlAdapter - with real data")
test_that("CurlAdapter works", {
  skip_on_cran()
  # skip_if_not_installed('vcr') # FIXME: not needed until curl supported in vcr

  # load("curl_obj.rda")
  # curl_obj$url$handle <- curl::new_handle()
  res <- CurlAdapter$new()
  curl_obj <- list()
  curl_obj$url <- "https://httpbin.org/get?foo=bar"
  curl_obj$handle <- curl::new_handle()
  curl_obj$called <- "curl_fetch_memoryurl = \"https://httpbin.org/get?foo=bar\""
  curl_obj$method <- "GET"
  curl_obj$headers <- list(
    accept = "*/*",
    'accept-encoding' = "gzip, deflate",
    host = "localhost:9359"
    # 'user-agent' = "R (3.5.2 x86_64-apple-darwin15.6.0 x86_64 darwin15.6.0)"
  )

  # with vcr message
  # FIXME: not needed until curl supported in vcr
  # library(vcr)
  # expect_error(
  #   res$handle_request(curl_obj),
  #   "There is currently no cassette in use"
  # )

  # with webmockr message
  # unload vcr
  # unloadNamespace("vcr")
  expect_error(
    res$handle_request(curl_obj),
    "Real HTTP connections are disabled.\nUnregistered request"
  )

  invisible(stub_request("get", "https://httpbin.org/get?foo=bar"))

  aa <- res$handle_request(curl_obj)

  expect_is(res, "CurlAdapter")
  expect_is(aa, "list")
  expect_equal(aa$url, "https://httpbin.org/get?foo=bar")

  # no response headers
  expect_equal(length(aa$headers), 0)


  # with headers
  # clear registry
  stub_registry_clear()

  # stub with headers
  x <- stub_request("get", "https://httpbin.org/get?foo=bar")
  x <- to_return(x, headers = list('User-Agent' = 'foo-bar'))

  aa <- res$handle_request(curl_obj)

  expect_is(res, "CurlAdapter")
  expect_is(aa, "list")
  expect_equal(aa$url, "https://httpbin.org/get?foo=bar")
  expect_equal(aa$status_code, 200)
  expect_equal(aa$modified, NA)
  expect_equal(length(aa$times), 0)
  expect_equal(length(aa$content), 0)

  # has headers
  expect_gt(length(aa$headers), 1)
  expect_is(aa$headers, "raw")
  expect_is(rawToChar(aa$headers), "character")
  expect_match(rawToChar(aa$headers), "User-Agent")
  expect_match(rawToChar(aa$headers), "foo-bar")


  # stub with redirect headers
  my_url <- "https://doi.org/10.1007/978-3-642-40455-9_52-1"
  x <- stub_request("get", my_url)
  x <- to_return(x, status = 302, headers =
    list(
      status = 302, 
      location = "http://link.springer.com/10.1007/978-3-642-40455-9_52-1"
    )
  )

  curl_obj$url <- my_url
  curl_obj$called <- sprintf("curl_fetch_memoryurl = \"%s\"", my_url)
  res <- CurlAdapter$new()
  aa <- res$handle_request(curl_obj)

  expect_equal(aa$url, my_url)
  expect_equal(aa$status_code, 302)

  # has headers
  expect_gt(length(aa$headers), 1)
  expect_is(aa$headers, "raw")
  heads <- rawToChar(aa$headers)
  expect_match(heads, 'status')
  expect_match(heads, 'location')
})
