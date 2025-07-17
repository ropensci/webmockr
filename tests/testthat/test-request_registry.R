test_that("request_registry: structure", {
  skip_on_cran()
  request_registry_clear()

  expect_type(request_registry, "closure")
  expect_s3_class(request_registry(), "RequestRegistry")

  enable(quiet = TRUE)
  stub_request("get", hb("/get")) %>%
    to_return(body = "success!", status = 200)
  invisible(
    crul::HttpClient$new(url = hb())$get("get")
  )
  disable(quiet = TRUE)

  x <- request_registry()
  expect_s3_class(x, "RequestRegistry")
  expect_type(x$clone, "closure")
  expect_type(x$print, "closure")
  expect_type(x$register_request, "closure")
  expect_null(x$request)
  expect_s3_class(x$request_signatures, "HashCounter")
  expect_type(x$reset, "closure")

  expect_type(x$request_signatures$hash, "list")
  expect_match(names(x$request_signatures$hash), "GET")
  expect_type(x$request_signatures$hash[[1]]$count, "double")
})
