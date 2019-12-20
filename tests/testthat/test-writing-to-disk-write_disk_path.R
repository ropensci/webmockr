context("write_disk_path behavior")

test_that("with crul", {
  skip_on_cran()
  skip_if_not_installed("vcr")
  library("vcr")
  dir <- tempdir()
  invisible(vcr_configure(dir = dir))

  library(crul)
  f <- tempfile(fileext = ".json")

  webmockr_net_connect_allowed()

  # path not set
  expect_error(
    use_cassette("write_disk_path_not_set_crul_error", {
      out <- HttpClient$new("https://httpbin.org/get")$get(disk = f)
    }),
    "write_disk_path must be given"
  )

  # now set path
  wdp <- file.path(dir, "files")
  invisible(vcr_configure(dir = dir, write_disk_path = wdp))
  expect_error(
    use_cassette("write_disk_path_not_set_crul_noerror", {
      out <- HttpClient$new("https://httpbin.org/get")$get(disk = f)
    }),
    NA
  )

  # cleanup
  unlink(f)
  unlink(wdp, TRUE)
  unlink(file.path(dir, "write_disk_path_not_set_crul_error.yml"))
  unlink(file.path(dir, "write_disk_path_not_set_crul_noerror.yml"))
  webmockr_disable_net_connect()
  unloadNamespace("vcr")
})
