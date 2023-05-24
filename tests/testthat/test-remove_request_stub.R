context("remove_request_stub")
# clear stubs before starting
stub_registry_clear()

test_that("remove_request_stub", {
  # no stubs at beginning
  expect_equal(length(stub_registry()$request_stubs), 0)

  # make a stub
  x <- stub_request("get", hb("/get"))

  # no there's a stub
  expect_equal(length(stub_registry()$request_stubs), 1)

  # remove the stub
  w <- remove_request_stub(x)
  expect_is(w, "list")
  expect_equal(length(w), 0)
  
  # no there's no stubs
  expect_equal(length(stub_registry()$request_stubs), 0)
})

test_that("remove_request_stub: removes the stub upon an error", {
  # no stubs at beginning
  stub_registry_clear()
  expect_equal(length(stub_registry()$request_stubs), 0)

  expect_error(
    stub_request("post", uri = hb("/post")) %>%
      to_return(body = 5)
  )
  expect_equal(length(stub_registry()$request_stubs), 0)  
  stub_registry_clear()
})

request_registry_clear()
