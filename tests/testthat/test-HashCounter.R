context("HashCounter")

test_that("HashCounter: structure", {
  expect_is(HashCounter, "R6ClassGenerator")

  x <- HashCounter$new()
  expect_is(x, "HashCounter")

  expect_is(x$clone, "function")
  expect_is(x$get, "function")
  expect_is(x$put, "function")

  expect_is(x$hash, "list")
})

test_that("HashCounter: works as expected", {
  x <- HashCounter$new()

  a <- RequestSignature$new(method = "get", uri = "https:/httpbin.org/get")
  b <- RequestSignature$new(method = "post", uri = "https://www.wikipedia.org/")

  x$put(a)
  expect_length(x$hash, 1)
  expect_equal(x$hash[[a$to_s()]]$count, 1)

  x$put(a)
  expect_length(x$hash, 1)
  expect_equal(x$hash[[a$to_s()]]$count, 2)

  x$put(b)
  expect_length(x$hash, 2)
  expect_equal(x$hash[[b$to_s()]]$count, 1)

  x$put(b)
  x$put(b)
  expect_length(x$hash, 2)
  expect_equal(x$hash[[b$to_s()]]$count, 3)
})

test_that("HashCounter fails well", {
  x <- HashCounter$new()

  expect_error(x$get(), '\"req_sig\" is missing')
  expect_error(x$put(), '\"req_sig\" is missing')
})
