context("pluck_body")

test_that("pluck_body: crul", {
  # prep objects
  # con <- crul::HttpClient$new("https://httpbin.org")

  # upload_list <- list(y = crul::upload(system.file("CITATION")))
  # b <- con$post("post", body = upload_list)
  # crul_body_upload_list <- b$request
  # crul_body_upload_list$url$handle <- NULL
  # save(crul_body_upload_list,
  #   file = "tests/testthat/crul_body_upload_list.rda", version = 2)

  # upload_no_list <- crul::upload(system.file("CITATION"))
  # d <- con$post("post", body = upload_no_list)
  # crul_body_upload_no_list <- d$request
  # crul_body_upload_no_list$url$handle <- NULL
  # save(crul_body_upload_no_list,
  #   file = "tests/testthat/crul_body_upload_no_list.rda", version = 2)

  # upload in a list
  load("crul_body_upload_list.rda")
  expect_is(pluck_body(crul_body_upload_list), "list")

  # upload not in a list
  load("crul_body_upload_no_list.rda")
  expect_is(pluck_body(crul_body_upload_no_list), "character")
  expect_match(pluck_body(crul_body_upload_no_list), "file size")
})

test_that("pluck_body: httr", {
  # prep objects
  # upload_list <- list(y = httr::upload_file(system.file("CITATION")))
  # b <- httr::POST("https://httpbin.org/post", body = upload_list)
  # httr_body_upload_list <- b$request
  # save(httr_body_upload_list,
  #   file = "tests/testthat/httr_body_upload_list.rda", version = 2)

  # upload_no_list <- httr::upload_file(system.file("CITATION"))
  # d <- httr::POST("https://httpbin.org/post", body = upload_no_list)
  # httr_body_upload_no_list <- d$request
  # save(httr_body_upload_no_list,
  #   file = "tests/testthat/httr_body_upload_no_list.rda", version = 2)

  # upload in a list
  load("httr_body_upload_list.rda")
  expect_is(pluck_body(httr_body_upload_list), "list")

  # upload not in a list
  load("httr_body_upload_no_list.rda")
  expect_is(pluck_body(httr_body_upload_no_list), "character")
  expect_match(pluck_body(httr_body_upload_no_list), "file size")
})


test_that("pluck_body fails well", {
  expect_error(pluck_body(5), "not a valid")
  expect_error(pluck_body(mtcars), "not a valid")
  expect_error(pluck_body(FALSE), "not a valid")
  expect_error(
    pluck_body(list(url = "adf", method = 3, options = 5)),
    "not a valid"
  )
})
