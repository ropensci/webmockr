stub_registry()$remove_all_request_stubs()

test_that("no stubs exist before stub_request called", {
  expect_equal(length(stub_registry()$request_stubs), 0)
})

aa <- stub_request("get", hb("/get"))

test_that("stub_request bits are correct", {
  expect_s3_class(aa, "StubbedRequest")
  expect_null(aa$body)
  expect_null(aa$host)
  expect_null(aa$query)
  expect_null(aa$request_headers)
  expect_null(aa$response)
  expect_null(aa$response_headers)
  expect_null(aa$responses_sequences)

  expect_type(aa$method, "character")
  expect_equal(aa$method, "get")
  expect_type(aa$uri, "character")
  expect_equal(aa$uri, hb("/get"))

  expect_type(aa$print, "closure")
  expect_output(aa$print(), "<webmockr stub>")

  expect_type(aa$to_return, "closure")
  expect_error(aa$to_return(), "argument \"body\" is missing")

  expect_type(aa$to_s, "closure")
  expect_equal(aa$to_s(), sprintf("GET: %s", hb("/get")))

  expect_type(aa$with, "closure")
  expect_null(aa$with())

  expect_type(aa$uri_parts, "list")
})

test_that("stubs exist after stub_request called", {
  expect_equal(length(stub_registry()$request_stubs), 1)
})

test_that("stub_request fails well", {
  expect_error(stub_request(), "one of uri or uri_regex is required")
  expect_error(
    stub_request(method = "stuff", "adf"),
    "'arg' should be one of"
  )
})
