#' Tests for HashCounter class

test_that("HashCounter: structure", {
  expect_s3_class(HashCounter, "R6ClassGenerator")

  x <- HashCounter$new()
  expect_s3_class(x, "HashCounter")

  expect_type(x$clone, "closure")
  expect_type(x$get, "closure")
  expect_type(x$put, "closure")

  expect_type(x$hash, "list")
  expect_length(x$hash, 0) # New instance has empty hash
})

test_that("HashCounter: initialization", {
  # Test initial state
  x <- HashCounter$new()
  expect_identical(x$hash, list())

  # Test that clone creates a proper duplicate
  y <- x$clone()
  expect_s3_class(y, "HashCounter")
  expect_identical(y$hash, x$hash)

  # Ensure they are independent objects
  a <- RequestSignature$new(method = "get", uri = hb("/get"))
  x$put(a)
  expect_length(x$hash, 1)
  expect_length(y$hash, 0)
})

test_that("HashCounter: works as expected with basic operations", {
  x <- HashCounter$new()

  a <- RequestSignature$new(method = "get", uri = hb("/get"))
  b <- RequestSignature$new(method = "post", uri = "https://www.wikipedia.org/")

  # First put
  x$put(a)
  expect_length(x$hash, 1)
  expect_equal(x$hash[[a$to_s()]]$count, 1)
  expect_equal(x$get(a), 1)

  # Second put of same request - counter should increment
  x$put(a)
  expect_length(x$hash, 1)
  expect_equal(x$hash[[a$to_s()]]$count, 2)
  expect_equal(x$get(a), 2)

  # Put different request
  x$put(b)
  expect_length(x$hash, 2)
  expect_equal(x$hash[[b$to_s()]]$count, 1)
  expect_equal(x$get(b), 1)

  # Multiple puts of second request
  x$put(b)
  x$put(b)
  expect_length(x$hash, 2)
  expect_equal(x$hash[[b$to_s()]]$count, 3)
  expect_equal(x$get(b), 3)

  # Original request count should still be 2
  expect_equal(x$get(a), 2)
})

test_that("HashCounter: get returns 0 for non-existing requests", {
  x <- HashCounter$new()
  a <- RequestSignature$new(method = "get", uri = hb("/get"))
  b <- RequestSignature$new(method = "post", uri = "https://www.wikipedia.org/")

  # Put one request
  x$put(a)
  expect_equal(x$get(a), 1)

  # Get count for non-existing request
  expect_equal(x$get(b), 0)
})

test_that("HashCounter: stores unique requests by signature", {
  x <- HashCounter$new()

  # These have different signatures
  a1 <- RequestSignature$new(method = "get", uri = hb("/get"))
  a2 <- RequestSignature$new(method = "get", uri = hb("/get?foo=bar"))
  a3 <- RequestSignature$new(method = "post", uri = hb("/get"))

  # Put all requests
  x$put(a1)
  x$put(a2)
  x$put(a3)

  # Each should have its own entry
  expect_length(x$hash, 3)
  expect_equal(x$get(a1), 1)
  expect_equal(x$get(a2), 1)
  expect_equal(x$get(a3), 1)

  # Getting one more time shouldn't change anything
  expect_equal(x$get(a1), 1)
  expect_length(x$hash, 3)
})

test_that("HashCounter: internal hash structure is correct", {
  x <- HashCounter$new()
  a <- RequestSignature$new(method = "get", uri = hb("/get"))

  x$put(a)

  # Check the internal hash structure
  key <- a$to_s()
  expect_true(key %in% names(x$hash))
  expect_type(x$hash[[key]], "list")
  expect_named(x$hash[[key]], c("key", "sig", "count"))
  expect_equal(x$hash[[key]]$key, key)
  expect_s3_class(x$hash[[key]]$sig, "RequestSignature")
  expect_equal(x$hash[[key]]$count, 1)
})

test_that("HashCounter fails well", {
  x <- HashCounter$new()

  # Missing arguments
  expect_error(x$get(), '\"req_sig\" is missing')
  expect_error(x$put(), '\"req_sig\" is missing')

  # Wrong argument types
  expect_error(
    x$get("not a request signature"),
    "must be of class RequestSignature"
  )
  expect_error(x$put(list()), "must be of class RequestSignature")
  expect_error(x$put(42), "must be of class RequestSignature")
})
