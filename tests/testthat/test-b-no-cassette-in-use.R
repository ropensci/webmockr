test_that("no cassette in use behaves as expected", {
  skip_if_not_installed("vcr")
  library("vcr")
  library("httr")
  dir <- tempdir()
  invisible(vcr_configure(dir = dir))

  enable()

  # when no cassette in use, we get expected vcr error
  expect_error(
    GET(url = hb(), path = "get"),
    "Failed to find matching request"
  )

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "turtle.yml"))

  # reset configuration
  vcr_configure_reset()

  # unload vcr
  unloadNamespace("vcr")
})
