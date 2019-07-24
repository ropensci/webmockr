context("mock writing to disk")

enable()

test_that("Write to a file before mocked request: crul", {
  skip_on_cran()

  library(crul)
  ## make a temp file
  f <- tempfile(fileext = ".json")
  ## write something to the file
  cat("{\"hello\":\"world\"}\n", file = f)
  expect_is(readLines(f), "character")
  expect_match(readLines(f), "world")
  ## make the stub
  stub_request("get", "https://httpbin.org/get") %>% 
    to_return(body = file(f))
  ## make a request
  out <- HttpClient$new("https://httpbin.org/get")$get(disk = f)
  expect_is(out$content, "character")
  expect_equal(attr(out$content, "type"), "file")
  expect_is(readLines(out$content), "character")
  expect_match(readLines(out$content), "hello")
  
  # cleanup
  unlink(f)
  stub_registry_clear()
})

test_that("Write to a file before mocked request: httr", {
  skip_on_cran()

  library(httr)
  ## make a temp file
  f <- tempfile(fileext = ".json")
  ## write something to the file
  cat("{\"hello\":\"world\"}\n", file = f)
  expect_is(readLines(f), "character")
  expect_match(readLines(f), "world")
  ## make the stub
  stub_request("get", "https://httpbin.org/get") %>% 
    to_return(body = file(f), 
     headers = list('content-type' = "application/json"))
  ## make a request
  ## with httr, you must set overwrite=TRUE or you'll get an errror
  out <- GET("https://httpbin.org/get", write_disk(f, overwrite=TRUE))
  content(out)
  expect_is(out$content, "path")
  expect_equal(attr(out$content, "class"), "path")
  expect_is(readLines(out$content), "character")
  expect_match(readLines(out$content), "hello")
  
  # cleanup
  unlink(f)
  stub_registry_clear()
})

test_that("Use mock_file to have webmockr handle file and contents: crul", {
  skip_on_cran()

  library(crul)
  ## make a temp file
  f <- tempfile(fileext = ".json")
  ## make the stub
  stub_request("get", "https://httpbin.org/get") %>% 
    to_return(body = mock_file(f, "{\"hello\":\"mars\"}\n"))
  ## make a request
  out <- crul::HttpClient$new("https://httpbin.org/get")$get(disk = f)
  out$content
  expect_is(out$content, "character")
  expect_match(out$content, "json")
  expect_is(readLines(out$content), "character")
  expect_true(any(grepl("hello", readLines(out$content))))
  
  # cleanup
  unlink(f)
  stub_registry_clear()
})

test_that("Use mock_file to have webmockr handle file and contents: httr", {
  skip_on_cran()

  library(httr)
  ## make a temp file
  f <- tempfile(fileext = ".json")
  ## make the stub
  stub_request("get", "https://httpbin.org/get") %>% 
    to_return(
      body = mock_file(path = f, payload = "{\"foo\": \"bar\"}"),
      headers = list('content-type' = "application/json")
    )
  ## make a request
  out <- GET("https://httpbin.org/get", write_disk(f))
  ## view stubbed file content
  expect_is(out$content, "path")
  expect_match(out$content, "json")
  expect_is(readLines(out$content), "character")
  expect_true(any(grepl("foo", readLines(out$content))))
  
  # cleanup
  unlink(f)
  stub_registry_clear()
})
