test_that("request_registry_filter returns all requests when no filters", {
  enable()
  stub_request("any", uri_regex = ".+")

  # Make some test requests
  library(httr)
  GET("http://example.com")
  POST("https://httpbin.org/post", body = list(name = "test"))

  result <- request_registry_filter()

  expect_type(result, "list")
  expect_gte(length(result), 2)

  disable()
  request_registry_clear()
})

test_that("request_registry_filter filters by method correctly", {
  enable()
  stub_request("any", uri_regex = ".+")

  library(httr)
  GET("http://example.com")
  GET("http://stuff.com")
  POST("https://httpbin.org/post", body = list(name = "test"))
  PUT("https://httpbin.org/put", body = list(id = 1))

  # Filter by GET
  get_results <- request_registry_filter(method = "get")
  expect_true(all(vapply(
    get_results,
    function(x) tolower(x$method) == "get",
    logical(1)
  )))

  # Filter by POST
  post_results <- request_registry_filter(method = "post")
  expect_true(all(vapply(
    post_results,
    function(x) {
      tolower(x$method) == "post"
    },
    logical(1)
  )))

  # Filter by PUT
  put_results <- request_registry_filter(method = "put")
  expect_true(all(vapply(
    put_results,
    function(x) tolower(x$method) == "put",
    logical(1)
  )))

  disable()
  request_registry_clear()
})

test_that("request_registry_filter filters by URL correctly", {
  enable()
  stub_request("any", uri_regex = ".+")

  library(httr)
  GET("http://example.com")
  GET("https://httpbin.org/get")
  POST("https://httpbin.org/post", body = list(name = "test"))

  # Filter by specific URL
  example_results <- request_registry_filter(url = "http://example.com")
  expect_true(all(vapply(
    example_results,
    function(x) {
      grepl("example.com", x$uri)
    },
    logical(1)
  )))

  # Filter by different URL
  httpbin_results <- request_registry_filter(url = "https://httpbin.org/get")
  expect_true(all(vapply(
    httpbin_results,
    function(x) {
      grepl("httpbin.org/get", x$uri)
    },
    logical(1)
  )))

  disable()
  request_registry_clear()
})

test_that("request_registry_filter filters by body correctly", {
  enable()
  stub_request("any", uri_regex = ".+")

  library(httr)
  POST("https://httpbin.org/post", body = list(fruit = "apple"))
  POST("https://httpbin.org/post", body = list(fruit = "banana"))
  POST("https://httpbin.org/post", body = list(cheese = "swiss"))

  # Filter by specific body content
  apple_results <- request_registry_filter(body = list(fruit = "apple"))
  expect_gte(length(apple_results), 1)

  cheese_results <- request_registry_filter(body = list(cheese = "swiss"))
  expect_gte(length(cheese_results), 1)

  # Filter by non-existent body should return empty
  empty_results <- request_registry_filter(body = list(vegetable = "carrot"))
  expect_length(empty_results, 0)

  disable()
  request_registry_clear()
})

test_that("request_registry_filter filters by headers correctly", {
  enable()
  stub_request("any", uri_regex = ".+")

  library(httr)
  GET("https://httpbin.org/get")
  GET("https://httpbin.org/get", add_headers(Accept = "application/json"))
  GET("https://httpbin.org/get", add_headers(`Content-Type` = "text/plain"))

  # Filter by specific header
  json_results <- request_registry_filter(
    headers = list(Accept = "application/json")
  )
  expect_gte(length(json_results), 1)

  disable()
  request_registry_clear()
})

test_that("request_registry_filter combines multiple filters correctly", {
  enable()
  stub_request("any", uri_regex = ".+")

  library(httr)
  GET("http://example.com")
  POST("https://httpbin.org/post", body = list(name = "test"))
  POST("http://example.com", body = list(name = "test"))

  # Combine method and URL filters
  combined_results <- request_registry_filter(
    method = "post",
    url = "http://example.com"
  )

  # Should only return POST requests to example.com
  expect_true(all(vapply(
    combined_results,
    function(x) {
      tolower(x$method) == "post" && grepl("example.com", x$uri)
    },
    logical(1)
  )))

  disable()
  request_registry_clear()
})

test_that("request_registry_filter validates input parameters", {
  # Test that invalid headers parameter throws error
  expect_error(
    request_registry_filter(headers = list("invalid")),
    "'headers' must be a named list"
  )

  # Valid named list should not error
  expect_no_error(
    request_registry_filter(headers = list(Accept = "application/json"))
  )
})

test_that("request_registry_filter returns correct structure", {
  enable()
  stub_request("any", uri_regex = ".+")

  library(httr)
  GET("http://example.com")

  result <- request_registry_filter()

  expect_type(result, "list")
  expect_gte(length(result), 1)

  # Check structure of first result
  first_result <- result[[1]]
  expect_true("key" %in% names(first_result))
  expect_true("count" %in% names(first_result))
  expect_true("method" %in% names(first_result))
  expect_true("uri" %in% names(first_result))
  expect_true("request" %in% names(first_result))

  disable()
  request_registry_clear()
})

test_that("convert_registry_to_list works correctly", {
  enable()
  stub_request("any", uri_regex = ".+")

  library(httr)
  GET("http://example.com")

  result <- convert_registry_to_list()

  expect_type(result, "list")
  expect_gte(length(result), 1)

  # Check that 'sig' was renamed to 'request'
  first_result <- result[[1]]
  expect_true("request" %in% names(first_result))
  expect_false("sig" %in% names(first_result))

  disable()
  request_registry_clear()
})

test_that("request_registry_filter handles empty registry", {
  # Clear registry to ensure it's empty
  request_registry_clear()

  result <- request_registry_filter()
  expect_type(result, "list")
  expect_length(result, 0)
})
