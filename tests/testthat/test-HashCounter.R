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

  x$put("foo bar")
  expect_length(x$hash, 1)
  expect_equal(x$hash$`foo bar`, 1)

  x$put("foo bar")
  expect_length(x$hash, 1)
  expect_equal(x$hash$`foo bar`, 2)

  x$put("hello world")
  expect_length(x$hash, 2)
  expect_equal(x$hash$`hello world`, 1)

  x$put("hello world")
  x$put("hello world")
  expect_length(x$hash, 2)
  expect_equal(x$hash$`hello world`, 3)
})

test_that("HashCounter fails well", {
  x <- HashCounter$new()

  expect_error(x$get(), "'key' required")
  expect_error(x$put(), "'key' required")
})
