test_that("onload: http_lib_adapter_registry", {
  expect_s3_class(http_lib_adapter_registry, "HttpLibAdapaterRegistry")
  expect_s3_class(http_lib_adapter_registry, "R6")
  expect_equal(
    sort(ls(envir = http_lib_adapter_registry)),
    c("adapters", "clone", "print", "register")
  )
  expect_type(http_lib_adapter_registry$adapters, "list")
  expect_s3_class(
    http_lib_adapter_registry$adapters[[1]],
    "CrulAdapter"
  )
  expect_s3_class(
    http_lib_adapter_registry$adapters[[2]],
    "HttrAdapter"
  )
  expect_s3_class(
    http_lib_adapter_registry$adapters[[3]],
    "Httr2Adapter"
  )
  expect_type(http_lib_adapter_registry$clone, "closure")
  expect_type(http_lib_adapter_registry$print, "closure")
  expect_type(http_lib_adapter_registry$register, "closure")
})
