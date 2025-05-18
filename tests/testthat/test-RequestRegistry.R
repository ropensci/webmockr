test_that("RequestRegistry: structure", {
  expect_s3_class(RequestRegistry, "R6ClassGenerator")

  aa <- RequestRegistry$new()

  expect_s3_class(aa, "RequestRegistry")
  expect_type(aa$clone, "closure")
  expect_type(aa$print, "closure")
  expect_type(aa$register_request, "closure")
  expect_null(aa$request)
  expect_s3_class(aa$request_signatures, "HashCounter")
  expect_type(aa$reset, "closure")
})

test_that("RequestRegistry: behaves as expected", {
  aa <- RequestRegistry$new()
  aa$reset()

  expect_length(aa$request_signatures$hash, 0)

  z1 <- RequestSignature$new(
    method = "post",
    uri = "https://www.wikipedia.org/"
  )

  aa$register_request(request = z1)
  aa$register_request(request = z1)

  expect_length(aa$request_signatures$hash, 1)
  expect_equal(
    aa$request_signatures$hash[[z1$to_s()]]$count,
    2
  )

  expect_output(
    print(aa),
    "Registered Requests"
  )
  expect_output(
    print(aa),
    "POST:  https://www.wikipedia.org/ was made"
  )

  # reset the request registry
  aa$reset()
  expect_length(aa$request_signatures$hash, 0)
})

test_that("RequestRegistry fails well", {
  x <- RequestRegistry$new()

  expect_error(x$register_request(), '\"request\" is missing')
})
