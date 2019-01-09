context("remove_request_stub")

# clear stubs before starting
stub_registry_clear()

test_that("remove_request_stub", {
  # no stubs at beginning
  expect_equal(length(stub_registry()$request_stubs), 0)

  # make a stub
  x <- stub_request("get", "https://httpbin.org/get")

  # no there's a stub
  expect_equal(length(stub_registry()$request_stubs), 1)

  # remove the stub
  w <- remove_request_stub(x)
  expect_is(w, "list")
  expect_equal(length(w), 0)
  
  # no there's no stubs
  expect_equal(length(stub_registry()$request_stubs), 0)
})
