context("flipswitch (enable/disable)")

test_that("flipswitch in default state", {
  expect_is(webmockr_lightswitch, "environment")
  expect_is(webmockr_lightswitch$crul, "logical")
  expect_false(webmockr_lightswitch$crul)
})

test_that("flipswitch - turn on with 'enable'", {
  aa <- enable()

  expect_is(aa, "logical")
  expect_equal(length(aa), 2)

  expect_true(webmockr_lightswitch$crul)
  skip_if_not_installed("httr")
  expect_true(webmockr_lightswitch$httr)
})

test_that("flipswitch - turn off with 'disable'", {
  aa <- disable()

  # all are FALSE
  expect_true(!all(aa))

  expect_false(webmockr_lightswitch$crul)
  skip_if_not_installed("httr")
  expect_false(webmockr_lightswitch$httr)
})

test_that("enable and disable fail well", {
  expect_error(enable(wasp = 5), "unused argument")
  expect_error(disable(bee = 5), "unused argument")
})
