context("stub_request and crul: get")

library(crul)
crul::mock()

# clear any stubs
stub_registry_clear()

test_that("stub_request works well: get requests", {
  skip_on_cran()

  # before any stubs made
  ## 0 stubs
  expect_equal(length(stub_registry()$request_stubs), 0)

  x <- crul::HttpClient$new(url = "https://httpbin.org")

  ms1 <- get_err_mssg(x$get('get', query = list(foo = "bar", a = 5)))
  expect_error(
    x$get('get', query = list(foo = "bar", a = 5)),
    re_escape(ms1)
  )

  ms2 <- get_err_mssg(x$get('get', query = list(foo = "bar", stuff = FALSE)))
  expect_error(
    x$get('get', query = list(foo = "bar", stuff = FALSE)),
    re_escape(ms2)
  )

  ms3 <- get_err_mssg(x$get('get', query = list(foo = "bar")))
  expect_error(
    x$get('get', query = list(foo = "bar")),
    re_escape(ms3)
  )

  # after a stub made
  stub_request("get", "https://httpbin.org/get?foo=bar&a=5") %>%
    wi_th(headers = list(
      'Accept-Encoding' = 'gzip, deflate',
      'Accept' = 'application/json, text/xml, application/xml, */*')
    )
  ## 1 stub
  expect_equal(length(stub_registry()$request_stubs), 1)

  # the matching request works
  z <- x$get('get', query = list(foo = "bar", a = 5))
  expect_is(z, "HttpResponse")
  expect_equal(z$url, "https://httpbin.org/get?foo=bar&a=5")

  # but the others still do not work cause they dont match the stub
  ms2 <- get_err_mssg(x$get('get', query = list(foo = "bar", stuff = FALSE)))
  expect_error(x$get('get', query = list(foo = "bar", stuff = FALSE)), re_escape(ms2))
  ms3 <- get_err_mssg(x$get('get', query = list(foo = "bar")))
  expect_error(x$get('get', query = list(foo = "bar")), re_escape(ms3))

  # a stub for the second request
  stub_request("get", "https://httpbin.org/get?foo=bar&stuff=FALSE") %>%
    wi_th(headers = list(
      'Accept-Encoding' = 'gzip, deflate',
      'Accept' = 'application/json, text/xml, application/xml, */*')
    )
  ## 2 stubs now
  expect_equal(length(stub_registry()$request_stubs), 2)

  # the other request now works
  w <- x$get('get', query = list(foo = "bar", stuff = FALSE))
  expect_is(w, "HttpResponse")
  expect_equal(w$url, "https://httpbin.org/get?foo=bar&stuff=FALSE")

  # but the others still do not work cause they dont match the stub
  ms4 <- get_err_mssg(x$get('get', query = list(foo = "bar")))
  expect_error(x$get('get', query = list(foo = "bar")), re_escape(ms4))
})

# clear any stubs again
stub_registry_clear()


context("stub_request and crul: post")
test_that("stub_request works well: post requests", {
  skip_on_cran()

  # before any stubs made
  ## 0 stubs
  expect_equal(length(stub_registry()$request_stubs), 0)

  x <- crul::HttpClient$new(url = "https://httpbin.org")

  ms1 <- get_err_mssg(x$post('post', body = list(foo = "bar", a = 5)))
  expect_error(
    x$post('post', body = list(foo = "bar", a = 5)),
    re_escape(ms1)
  )

  # after a stub made
  stub_request("post", "https://httpbin.org/post") %>%
    wi_th(headers = list(
      'Accept-Encoding' = 'gzip, deflate',
      'Accept' = 'application/json, text/xml, application/xml, */*'),
      body = list(foo = "bar", a = 5)
    )
  ## 1 stub
  expect_equal(length(stub_registry()$request_stubs), 1)

  # the matching request works
  z <- x$post('post', body = list(foo = "bar", a = 5))
  expect_is(z, "HttpResponse")
  expect_equal(z$url, "https://httpbin.org/post")

  # but the others still do not work cause they dont match the stub
  ms2 <- get_err_mssg(x$post('post', query = list(foo = "bar", stuff = FALSE)))
  expect_error(x$post('post', query = list(foo = "bar", stuff = FALSE)), re_escape(ms2))
  ms3 <- get_err_mssg(x$post('post', query = list(foo = "bar")))
  expect_error(x$post('post', query = list(foo = "bar")), re_escape(ms3))
})

