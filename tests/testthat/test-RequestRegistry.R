test_that("RequestRegistry: structure", {
  expect_s3_class(RequestRegistry, "R6ClassGenerator")

  aa <- RequestRegistry$new()

  expect_s3_class(aa, "RequestRegistry")
  expect_type(aa$clone, "closure")
  expect_type(aa$print, "closure")
  expect_type(aa$register_request, "closure")
  expect_type(aa$times_executed, "closure")
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
  expect_output(
    print(aa),
    "2 times"
  )

  # reset the request registry
  aa$reset()
  expect_length(aa$request_signatures$hash, 0)
})

test_that("RequestRegistry fails well", {
  x <- RequestRegistry$new()

  expect_error(x$register_request(), '\"request\" is missing')
  expect_error(x$times_executed(), '`request_pattern` is required')
  expect_error(
    x$times_executed("not a request pattern"),
    "`request_pattern` must be of class 'RequestPattern'"
  )
})

test_that("RequestRegistry: times_executed works correctly", {
  x <- RequestRegistry$new()

  # Create request signatures
  get_sig <- RequestSignature$new(
    method = "get",
    uri = "http://example.org/get"
  )
  post_sig <- RequestSignature$new(
    method = "post",
    uri = "http://example.org/post"
  )
  complex_sig <- RequestSignature$new(
    method = "get",
    uri = "http://example.org/complex",
    options = list(
      headers = list(`User-Agent` = "test-agent", Accept = "application/json")
    )
  )

  # Register them different numbers of times
  x$register_request(get_sig)
  x$register_request(get_sig)
  x$register_request(post_sig)
  x$register_request(complex_sig)
  x$register_request(complex_sig)
  x$register_request(complex_sig)

  # Create matching patterns
  get_pattern <- RequestPattern$new(
    method = "get",
    uri = "http://example.org/get"
  )
  post_pattern <- RequestPattern$new(
    method = "post",
    uri = "http://example.org/post"
  )
  complex_pattern <- RequestPattern$new(
    method = "get",
    uri = "http://example.org/complex",
    headers = list(`User-Agent` = "test-agent")
  )
  non_matching_pattern <- RequestPattern$new(
    method = "get",
    uri = "http://example.org/nonexistent"
  )

  # Test times_executed returns correct counts
  expect_equal(x$times_executed(get_pattern), 2)
  expect_equal(x$times_executed(post_pattern), 1)
  expect_equal(x$times_executed(complex_pattern), 3)
  expect_equal(x$times_executed(non_matching_pattern), 0)
})

test_that("RequestRegistry: print method works with empty registry", {
  x <- RequestRegistry$new()
  expect_output(print(x), "<webmockr request registry>")
  expect_output(print(x), "Registered Requests")
})

test_that("RequestRegistry: initialization and cloning", {
  x <- RequestRegistry$new()

  # Register a request
  sig <- RequestSignature$new(method = "get", uri = "http://example.org")
  x$register_request(sig)

  # Clone and verify independent instances
  y <- x$clone()
  expect_s3_class(y, "RequestRegistry")

  # Add to original, clone should remain unchanged
  x$register_request(sig)
  expect_equal(
    x$times_executed(RequestPattern$new(
      method = "get",
      uri = "http://example.org"
    )),
    2
  )
  expect_equal(
    y$times_executed(RequestPattern$new(
      method = "get",
      uri = "http://example.org"
    )),
    2
  )
})

test_that("RequestRegistry: supports complex request signatures", {
  x <- RequestRegistry$new()

  # Create a complex request signature with headers, query params, and body
  complex_sig <- RequestSignature$new(
    method = "post",
    uri = "http://example.org/api?version=1",
    options = list(
      headers = list(
        `Content-Type` = "application/json",
        Authorization = "Bearer token"
      ),
      body = '{"key":"value"}'
    )
  )

  x$register_request(complex_sig)

  # Test matching with various patterns
  full_pattern <- RequestPattern$new(
    method = "post",
    uri = "http://example.org/api?version=1",
    headers = list(`Content-Type` = "application/json"),
    body = '{"key":"value"}'
  )

  partial_pattern <- RequestPattern$new(
    method = "post",
    uri = "http://example.org/api"
  )

  wrong_method_pattern <- RequestPattern$new(
    method = "get",
    uri = "http://example.org/api?version=1"
  )

  expect_equal(x$times_executed(full_pattern), 1)
  expect_equal(x$times_executed(partial_pattern), 0)
  expect_equal(x$times_executed(wrong_method_pattern), 0)
})
