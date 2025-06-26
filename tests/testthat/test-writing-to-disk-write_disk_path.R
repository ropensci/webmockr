test_that("with crul", {
  skip_on_cran()
  skip_if_not_installed("vcr")
  library("vcr")
  dir <- tempdir()
  invisible(vcr_configure(dir = dir))

  suppressPackageStartupMessages(library("crul", warn.conflicts = FALSE))
  f <- tempfile(fileext = ".json")

  sm(webmockr_net_connect_allowed())

  # path not set
  expect_error(
    suppressWarnings(use_cassette("write_disk_path_not_set_crul_error", {
      out <- HttpClient$new(hb("/get"))$get(disk = f)
    })),
    "write_disk_path must be given"
  )

  # now set path
  wdp <- file.path(dir, "files")
  invisible(vcr_configure(dir = dir, write_disk_path = wdp))
  expect_error(
    use_cassette("write_disk_path_not_set_crul_noerror", {
      out <- HttpClient$new(hb("/get"))$get(disk = f)
    }),
    NA
  )

  # cleanup
  unlink(f)
  unlink(wdp, TRUE)
  unlink(file.path(dir, "write_disk_path_not_set_crul_error.yml"))
  unlink(file.path(dir, "write_disk_path_not_set_crul_noerror.yml"))
  suppressMessages(webmockr_disable_net_connect())
  unloadNamespace("vcr")
})

test_that("if relative path set its not expanded to full path anymore", {
  skip_on_cran()
  skip_if_not_installed("vcr")
  library("vcr")
  dir <- tempdir()
  f <- "stuff.json"
  wdp <- "../files"
  invisible(vcr_configure(dir = dir, write_disk_path = wdp))

  og <- getwd()
  setwd(dir)
  on.exit(setwd(og))

  expect_error(
    use_cassette("write_disk_path_is_relative", {
      out <- HttpClient$new(hb("/get?foo=foo"))$get(disk = f)
    }),
    NA
  )
  txt <- readLines(file.path(dir, "write_disk_path_is_relative.yml"))
  expect_true(any(grepl("../files/stuff.json", txt)))

  # cleanup
  # unlink("files", recursive = TRUE)
  unlink("stuff.json")
  suppressMessages(webmockr_disable_net_connect())
  unloadNamespace("vcr")
})

# httr
test_that("with httr", {
  skip_on_cran()
  skip_if_not_installed("vcr")
  library("vcr")
  enable(quiet = TRUE)
  dir <- tempdir()
  invisible(vcr_configure(dir = dir))

  library(httr)
  f <- tempfile(fileext = ".json")

  sm(webmockr_net_connect_allowed())

  # path not set
  # FIXME for vcr v2 - should no longer error
  expect_error(
    suppressWarnings(
      use_cassette("write_disk_path_not_set_crul_error", {
        out <- GET(hb("/get"), write_disk(f))
      })
    ),
    "write_disk_path must be given"
  )

  # now set path
  f <- tempfile(fileext = ".json")
  wdp <- file.path(dir, "files")
  invisible(vcr_configure(dir = dir, write_disk_path = wdp))
  expect_error(
    use_cassette("write_disk_path_not_set_crul_noerror", {
      out <- GET(hb("/get"), write_disk(f))
    }),
    NA
  )

  # cleanup
  unlink(f)
  unlink(wdp, TRUE)
  unlink(file.path(dir, "write_disk_path_not_set_crul_error.yml"))
  unlink(file.path(dir, "write_disk_path_not_set_crul_noerror.yml"))
  suppressMessages(webmockr_disable_net_connect())
  unloadNamespace("vcr")
})

test_that("if relative path set its not expanded to full path anymore: httr", {
  skip_on_cran()
  skip_if_not_installed("vcr")
  library("vcr")
  dir <- tempdir()
  f <- "stuff.json"
  wdp <- "../files"
  invisible(vcr_configure(dir = dir, write_disk_path = wdp))

  og <- getwd()
  setwd(dir)
  on.exit(setwd(og))

  expect_error(
    use_cassette("write_disk_path_is_relative", {
      out <- GET(hb("/get?foo=foo"), write_disk(f))
    }),
    NA
  )
  txt <- readLines(file.path(dir, "write_disk_path_is_relative.yml"))
  expect_true(any(grepl("../files/stuff.json", txt)))

  # cleanup
  # unlink("files", recursive = TRUE)
  unlink("stuff.json")
  suppressMessages(webmockr_disable_net_connect())
  unloadNamespace("vcr")
})
