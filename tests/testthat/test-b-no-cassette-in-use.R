test_that("no cassette in use behaves as expected", {
  skip_if_not_installed("vcr")
  library("vcr")
  dir <- tempdir()
  invisible(vcr_configure(dir = dir))

  crul::mock()
  x <- crul::HttpClient$new(url = hb())

  # when no cassette in use, we get expected vcr error
  expect_error(
    x$get("get"),
    "There is currently no cassette in use"
  )

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "turtle.yml"))

  # reset configuration
  vcr_configure_reset()

  # unload vcr
  unloadNamespace("vcr")
})
