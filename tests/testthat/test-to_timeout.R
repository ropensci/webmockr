stub_registry()$remove_all_request_stubs()

test_that("no stubs exist before stub_request called", {
  expect_equal(length(stub_registry()$request_stubs), 0)
})

aa <- stub_request("get", hb("/get")) %>% to_timeout()

test_that("stub_request bits are correct", {
  expect_s3_class(aa, "StubbedRequest")
  expect_null(aa$body)
  expect_null(aa$host)
  expect_null(aa$response)
  expect_null(aa$query)
  expect_null(aa$request_headers)
  expect_null(aa$response_headers)
  expect_type(aa$responses_sequences, "list")

  expect_type(aa$method, "character")
  expect_equal(aa$method, "get")
  expect_type(aa$uri, "character")
  expect_equal(aa$uri, hb("/get"))

  # to_timeout expected stuff
  expect_true(aa$responses_sequences[[1]]$timeout)
})

test_that("stubs exist after stub_request called", {
  expect_equal(length(stub_registry()$request_stubs), 1)
})

test_that("stub_request fails well", {
  expect_error(to_timeout(), "argument \".data\" is missing")
  expect_error(to_timeout(5), "must be of class StubbedRequest")
})

# cleanup
stub_registry_clear()
