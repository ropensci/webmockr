context("request_registry")

test_that("request_registry: structure", {
  request_registry_clear()

  expect_is(request_registry, "function")
  expect_is(request_registry(), "RequestRegistry")

  enable()
  stub_request("get", "https://httpbin.org/get") %>%
    to_return(body = "success!", status = 200)
  invisible(
    crul::HttpClient$new(url = "https://httpbin.org")$get("get")
  )
  disable()

  x <- request_registry()
  expect_is(x, "RequestRegistry")
  expect_is(x$clone, "function")
  expect_is(x$print, "function")
  expect_is(x$register_request, "function")
  expect_null(x$request)
  expect_is(x$request_signatures, "HashCounter")
  expect_is(x$reset, "function")

  expect_is(x$request_signatures$hash, "list")
  expect_match(names(x$request_signatures$hash), "GET")
  expect_is(x$request_signatures$hash[[1]]$count, 'numeric')
})
