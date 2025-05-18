aa <- StubRegistry$new()

test_that("StubRegistry: bits are correct prior to having data", {
  expect_s3_class(StubRegistry, "R6ClassGenerator")

  expect_s3_class(aa, "StubRegistry")

  expect_type(aa$request_stubs, "list")
  expect_equal(length(aa$request_stubs), 0)

  expect_null(aa$stub)

  expect_type(aa$find_stubbed_request, "closure")
  expect_type(aa$is_registered, "closure")
  expect_type(aa$print, "closure")
  expect_type(aa$register_stub, "closure")
  expect_type(aa$remove_all_request_stubs, "closure")
  expect_type(aa$remove_request_stub, "closure")
  expect_type(aa$request_stub_for, "closure")
  # expect_type(aa$response_for_request, "closure")
})


test_that("StubRegistry: bits are correct after having data", {
  stub1 <- StubbedRequest$new(method = "get", uri = "http://api.crossref.org")
  stub1$with(headers = list("User-Agent" = "R"))
  stub1$to_return(status = 200, body = "foobar", headers = list())

  stub2 <- StubbedRequest$new(method = "get", uri = hb())

  aa <- StubRegistry$new()
  expect_type(aa$register_stub(stub = stub1), "list")
  expect_type(aa$register_stub(stub = stub2), "list")

  expect_s3_class(aa, "StubRegistry")

  # request stubs now length 2
  expect_type(aa$request_stubs, "list")
  expect_equal(length(aa$request_stubs), 2)

  expect_null(aa$stub)

  # find_stubbed_request
  req1 <- RequestSignature$new(
    method = "get",
    uri = "http://api.crossref.org",
    options = list(
      headers = list("User-Agent" = "R")
    )
  )

  res <- aa$find_stubbed_request(req = req1)
  expect_type(res, "list")
  expect_s3_class(res[[1]], "StubbedRequest")
  expect_equal(res[[1]]$uri, "http://api.crossref.org")

  # is_registered
  expect_true(aa$is_registered(x = req1))

  # request_stub_for
  matches <- aa$request_stub_for(request_signature = req1)
  expect_type(matches, "logical")
  expect_equal(matches, c(TRUE, FALSE))

  # response_for_request
  ## FIXME - internal function not made yet
  # expect_error(aa$response_for_request(request_signature = req1),
  #              "could not find function")

  # remove_request_stub
  res <- aa$remove_request_stub(stub = stub1)
  expect_type(res, "list")
  expect_equal(length(res), 1)

  # remove_all_request_stubs
  ## add another first
  aa$register_stub(stub = stub1)
  res <- aa$remove_all_request_stubs()
  expect_type(res, "list")
  expect_equal(length(res), 0)
})

test_that("StubRegistry fails well", {
  # fill ins ome data first
  stub1 <- StubbedRequest$new(method = "get", uri = "api.crossref.org")
  aa <- StubRegistry$new()
  aa$register_stub(stub = stub1)

  expect_error(aa$find_stubbed_request(), "argument \"req\" is missing")
  expect_error(aa$is_registered(), "argument \"x\" is missing")
  expect_error(aa$register_stub(), "argument \"stub\" is missing")
  expect_error(aa$remove_request_stub(), "argument \"stub\" is missing")
  expect_error(
    aa$request_stub_for(),
    "argument \"request_signature\" is missing"
  )
})
