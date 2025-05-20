test_that("flipswitch in default state", {
  expect_type(webmockr_lightswitch, "environment")
  expect_type(webmockr_lightswitch$httr, "logical")
})

test_that("flipswitch - turn on with 'enable'", {
  skip_if_not_installed("httr")
  skip_if_not_installed("httr2")

  aa <- enable()

  expect_type(aa, "logical")
  expect_equal(length(aa), 3)
  expect_true(all(aa))

  expect_true(webmockr_lightswitch$crul)
  skip_if_not_installed("httr")
  expect_true(webmockr_lightswitch$httr)
  skip_if_not_installed("httr2")
  expect_true(webmockr_lightswitch$httr2)
})

test_that("flipswitch - turn on with 'enable' - one pkg", {
  # disable all
  disable()

  # enable one pkg
  aa <- enable("crul")

  expect_type(aa, "logical")
  expect_equal(length(aa), 1)
  expect_true(aa)

  expect_true(webmockr_lightswitch$crul)
  skip_if_not_installed("httr")
  expect_false(webmockr_lightswitch$httr)
  skip_if_not_installed("httr2")
  expect_false(webmockr_lightswitch$httr2)
})

test_that("flipswitch - turn off with 'disable'", {
  aa <- disable()

  # all are FALSE
  expect_true(!all(aa))

  expect_false(webmockr_lightswitch$crul)
  skip_if_not_installed("httr")
  expect_false(webmockr_lightswitch$httr)
  skip_if_not_installed("httr2")
  expect_false(webmockr_lightswitch$httr2)
})

test_that("enable and disable fail well", {
  expect_error(enable(wasp = 5), "unused argument")
  expect_error(disable(bee = 5), "unused argument")

  expect_error(
    enable(adapter = "stuff"),
    "adapter must be one of"
  )
  expect_error(
    disable(adapter = "stuff"),
    "adapter must be one of"
  )

  # FIXME: not sure how to test when pkg not installed
  #   inside of test suite
})

test_that("enabled works", {
  # disable all
  disable()

  expect_false(enabled())
  expect_false(enabled("crul"))
  expect_false(enabled("httr"))
  expect_false(enabled("httr2"))

  expect_error(enabled("foobar"), "'adapter' must be in the set")
})
