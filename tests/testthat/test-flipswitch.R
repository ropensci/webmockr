context("flipswitch (enable/disable)")

test_that("flipswitch in default state", {
  expect_is(webmockr_lightswitch, "environment")
  expect_is(webmockr_lightswitch$crul, "logical")
  expect_false(webmockr_lightswitch$crul)
})

test_that("flipswitch - turn on with 'enable'", {
  aa <- enable()

  expect_is(aa, "logical")
  expect_equal(length(aa), 1)

  expect_true(webmockr_lightswitch$crul)
})

test_that("flipswitch - turn off with 'disable'", {
  aa <- disable()

  expect_false(aa)

  expect_false(webmockr_lightswitch$crul)
})

test_that("enable and disable fail well", {
  expect_error(enable(a = 5), "unused argument")
  expect_error(disable(a = 5), "unused argument")
})
