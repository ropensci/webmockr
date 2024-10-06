context("onload")

test_that("onload: http_lib_adapter_registry", {
  expect_is(http_lib_adapter_registry, "HttpLibAdapaterRegistry")
  expect_is(http_lib_adapter_registry, "R6")
  expect_equal(sort(ls(envir=http_lib_adapter_registry)),
    c('adapters', 'clone', 'print', 'register'))
  expect_is(http_lib_adapter_registry$adapters, "list")
  expect_is(http_lib_adapter_registry$adapters[[1]],
    "CrulAdapter")
  expect_is(http_lib_adapter_registry$adapters[[2]],
    "HttrAdapter")
  expect_is(http_lib_adapter_registry$adapters[[3]],
    "Httr2Adapter")
  expect_is(http_lib_adapter_registry$clone, "function")
  expect_is(http_lib_adapter_registry$print, "function")
  expect_is(http_lib_adapter_registry$register, "function")
})
