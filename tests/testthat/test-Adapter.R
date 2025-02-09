context("Adapter class")

test_that("Adapter class can't be instantiated", {
  expect_is(Adapter, "R6ClassGenerator")
  expect_error(
    Adapter$new(),
    "Adapter parent class should not be called directly"
  )
})

test_that("Adapter initialize method errors as expected", {
  adap <- R6::R6Class("CrulAdapter",
    inherit = Adapter,
    public = list(
      client = NULL
    )
  )
  expect_error(adap$new(), "should not be called directly")
})
