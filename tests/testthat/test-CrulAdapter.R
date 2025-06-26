aa <- CrulAdapter$new()

test_that("CrulAdapter bits are correct", {
  skip_on_cran()

  expect_s3_class(CrulAdapter, "R6ClassGenerator")

  expect_s3_class(aa, "CrulAdapter")
  expect_null(aa$build_crul_request) # pulled out of object, so should be NULL
  expect_null(aa$build_crul_response) # pulled out of object, so should be NULL
  expect_type(aa$disable, "closure")
  expect_type(aa$enable, "closure")
  expect_type(aa$handle_request, "closure")
  expect_type(aa$remove_stubs, "closure")
  expect_type(aa$name, "character")

  expect_equal(aa$name, "CrulAdapter")
})


test_that("CrulAdapter behaves correctly", {
  skip_on_cran()

  expect_message(aa$enable(), "CrulAdapter enabled!")
  expect_message(aa$disable(), "CrulAdapter disabled!")
})


test_that("build_crul_request/response fail well", {
  skip_on_cran()

  expect_error(build_crul_request(), "argument \"x\" is missing")
  expect_error(build_crul_response(), "argument \"resp\" is missing")
})

test_that("CrulAdapter: works when vcr is loaded but no cassette is inserted", {
  skip_on_cran()
  skip_if_not_installed("vcr")

  webmockr::enable(adapter = "crul", quiet = TRUE)
  on.exit({
    webmockr::disable(adapter = "crul", quiet = TRUE)
    unloadNamespace("vcr")
  })

  stub_request("get", hb("/get"))
  library("vcr")

  # works when no cassette is loaded
  cli <- crul::HttpClient$new(hb())

  expect_silent(x <- cli$get("get"))
  expect_s3_class(x, "HttpResponse")

  # works when empty cassette is loaded
  vcr::vcr_configure(dir = tempdir())
  vcr::insert_cassette("empty")
  expect_silent(x <- cli$get("get"))
  vcr::eject_cassette()
  expect_s3_class(x, "HttpResponse")
})

test_that("CrulAdapter works", {
  skip_on_cran()
  skip_if_not_installed("vcr")

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
    "Real HTTP connections are disabled"
  )

  invisible(stub_request("get", "http://localhost:9000/get"))

  aa <- res$handle_request(crul_obj)

  expect_s3_class(res, "CrulAdapter")
  expect_s3_class(aa, "HttpResponse")
  expect_equal(aa$method, "get")
  expect_equal(aa$url, "http://localhost:9000/get")

  # no response headers
  expect_equal(length(aa$response_headers), 0)
  expect_equal(length(aa$response_headers_all), 0)

  # with headers
  # clear registry
  stub_registry_clear()

  # stub with headers
  x <- stub_request("get", "http://localhost:9000/get")
  x <- to_return(x, headers = list("User-Agent" = "foo-bar"))

  aa <- res$handle_request(crul_obj)

  expect_s3_class(res, "CrulAdapter")
  expect_s3_class(aa, "HttpResponse")
  expect_equal(aa$method, "get")
  expect_equal(aa$url, "http://localhost:9000/get")

  # has response_headers and response_headers_all
  expect_equal(length(aa$response_headers), 1)
  expect_type(aa$response_headers, "list")
  expect_named(aa$response_headers, "user-agent")
  expect_equal(length(aa$response_headers_all), 1)
  expect_type(aa$response_headers_all, "list")
  expect_named(aa$response_headers_all, NULL)
  expect_named(aa$response_headers_all[[1]], "user-agent")

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

  crul_obj$url$url <- my_url
  res <- CrulAdapter$new()
  aa <- res$handle_request(crul_obj)

  expect_equal(aa$method, "get")
  expect_equal(aa$url, my_url)
  expect_equal(aa$status_code, 302)

  # has response_headers and response_headers_all
  expect_equal(length(aa$response_headers), 2)
  expect_type(aa$response_headers, "list")
  expect_equal(sort(names(aa$response_headers)), c("location", "status"))
  expect_equal(length(aa$response_headers_all), 1)
  expect_equal(length(aa$response_headers_all[[1]]), 2)
  expect_type(aa$response_headers_all, "list")
  expect_type(aa$response_headers_all[[1]], "list")
  expect_named(aa$response_headers_all, NULL)
  expect_equal(
    sort(names(aa$response_headers_all[[1]])),
    c("location", "status")
  )

  ## FIXME: ideally can test multiple redirect headers, e.g. like this:
  # x <- stub_request("get", "https://doi.org/10.1007/978-3-642-40455-9_52-1")
  # x <- to_return(x, headers = list(
  #   list(
  #     status = 'HTTP/1.1 302 ',
  #     location = "http://link.springer.com/10.1007/978-3-642-40455-9_52-1"
  #   ),
  #   list(
  #     status = 'HTTP/1.1 301 Moved Permanently',
  #     location = "https://link.springer.com/10.1007/978-3-642-40455-9_52-1"
  #   ),
  #   list(
  #     status = 'HTTP/1.1 302 Found',
  #     location = "https://link.springer.com/referenceworkentry/10.1007%2F978-3-642-40455-9_52-1"
  #   ),
  #   list(
  #     status = 'HTTP/1.1 200 OK'
  #   )
  # ))
})

test_that("crul requests with JSON-encoded bodies work", {
  skip_on_cran()

  on.exit(disable(adapter = "crul", quiet = TRUE))
  enable(adapter = "crul", quiet = TRUE)

  body <- list(foo = "bar")
  url <- hb()

  cli <- crul::HttpClient$new(url)

  z <- stub_request("post", uri = file.path(url, "post")) %>%
    wi_th(body = jsonlite::toJSON(body, auto_unbox = TRUE))

  # encoded body works
  res <- cli$post("post", body = body, encode = "json")
  expect_s3_class(res, "HttpResponse")

  # encoded but modified body fails
  expect_error(
    cli$post("post", body = list(foo = "bar1"), encode = "json"),
    "Unregistered request"
  )

  # unencoded body should work because we serialize internally
  expect_s3_class(cli$post("post", body = body), "HttpResponse")
})
