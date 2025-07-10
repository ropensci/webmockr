test_that("normalize_uri", {
  # prunes trailing slash
  expect_type(normalize_uri("example.com/"), "character")
  expect_match(normalize_uri("example.com/"), "example.com")

  # prunes ports 80 and 443
  expect_match(normalize_uri("example.com:80"), "example.com")
  expect_match(normalize_uri("example.com:443"), "example.com")

  # escapes special characters
  expect_match(
    normalize_uri("example.com/foo/bar"),
    "example.com/foo%2Fbar"
  )
  expect_match(
    normalize_uri("example.com/foo+bar"),
    "example.com/foo%2Bbar"
  )
  expect_match(
    normalize_uri("example.com/foo*bar"),
    "example.com/foo%2Abar"
  )
})


test_that("net_connect_explicit_allowed", {
  aa <- net_connect_explicit_allowed(
    allowed = "example.com",
    uri = "http://example.com"
  )

  expect_type(aa, "logical")
  expect_equal(length(aa), 1)

  # works with lists
  expect_true(
    net_connect_explicit_allowed(
      list("example.com", "foobar.org"),
      "example.com"
    )
  )
  expect_false(
    net_connect_explicit_allowed(
      list("example.com", "foobar.org"),
      "stuff.io"
    )
  )

  # no uri passed, returns FALSE
  expect_false(net_connect_explicit_allowed("google.com"))

  # empty character string uri passed, returns FALSE
  expect_false(net_connect_explicit_allowed("google.com", ""))

  # no allowed passed, errors
  expect_error(
    net_connect_explicit_allowed(),
    "argument \"allowed\" is missing"
  )
})

test_that("webmockr_net_connect_allowed", {
  # works with character strings
  expect_false(webmockr_net_connect_allowed("example.com"))
  expect_false(webmockr_net_connect_allowed("http://example.com"))
  expect_false(webmockr_net_connect_allowed("https://example.com"))

  # no uri passed, returns FALSE
  expect_false(webmockr_net_connect_allowed())

  # nonense passed, returns FALSE
  expect_false(webmockr_net_connect_allowed(""))
  expect_false(webmockr_net_connect_allowed("asdfadfafsd"))

  # errors when of wrong class
  expect_error(
    sm(webmockr_net_connect_allowed(mtcars)),
    "class character or list"
  )
})

test_that("webmockr_disable_net_connect", {
  # nothing passed
  expect_null(sm(webmockr_disable_net_connect()))
  expect_message(webmockr_disable_net_connect(), "net connect disabled")

  # single uri passed
  expect_message(
    webmockr_disable_net_connect("google.com"),
    "net connect disabled"
  )
  expect_type(sm(webmockr_disable_net_connect("google.com")), "character")
  expect_equal(sm(webmockr_disable_net_connect("google.com")), "google.com")

  # many uri's passed
  expect_message(
    webmockr_disable_net_connect(c("google.com", "nytimes.com")),
    "net connect disabled"
  )
  expect_type(
    sm(webmockr_disable_net_connect(c("google.com", "nytimes.com"))),
    "character"
  )
  expect_equal(
    sm(webmockr_disable_net_connect(c("google.com", "nytimes.com"))),
    c("google.com", "nytimes.com")
  )

  # errors when of wrong class
  expect_error(
    webmockr_disable_net_connect(5),
    "class character"
  )
  expect_error(
    webmockr_disable_net_connect(mtcars),
    "class character"
  )
})

test_that("webmockr_allow_net_connect", {
  # first call, sets to TRUE, and returns message

  # nothing passed
  expect_message(z <- webmockr_allow_net_connect(), "net connect allowed")
  expect_true(z)

  # check if net collect allowed afterwards, should be TRUE
  expect_true(sm(webmockr_net_connect_allowed()))

  # errors when an argument passed
  expect_error(sm(webmockr_allow_net_connect(5)), "unused argument")
})

test_that("show_stubbing_instructions", {
  enable(quiet = TRUE)
  x <- crul::HttpClient$new("https://hb.cran.dev/get")

  # DO show stubbing instructions
  webmockr_configure(show_stubbing_instructions = TRUE)
  err_mssg <- as.character(tryCatch(x$get(), error = function(e) e))
  expect_true(grepl("snippet", err_mssg, perl = TRUE))

  # DO NOT show stubbing instructions
  webmockr_configure(show_stubbing_instructions = FALSE)
  err_mssg <- as.character(tryCatch(x$get(), error = function(e) e))
  expect_false(grepl("^((?!snippet).)*$", err_mssg, perl = TRUE))

  # reset to default
  webmockr_configure(show_stubbing_instructions = TRUE)
  disable(quiet = TRUE)
})

test_that("webmockr_configuration", {
  expect_s3_class(webmockr_configuration(), "webmockr_config")
  expect_named(
    webmockr_configuration(),
    c(
      "show_stubbing_instructions",
      "show_body_diff",
      "allow",
      "allow_net_connect",
      "allow_localhost"
    )
  )

  # errors when an argument passed
  expect_error(webmockr_configuration(5), "unused argument")
})

test_that("webmockr_configure_reset", {
  # webmockr_configure_reset does the same thing as webmockr_configure
  expect_identical(webmockr_configure(), webmockr_configure_reset())

  # errors when an argument passed
  expect_error(webmockr_configure_reset(5), "unused argument")
})

test_that("webmockr_disable", {
  expect_error(webmockr_disable(), "disable", class = "error")
})
test_that("webmockr_enable", {
  expect_error(webmockr_enable(), "enable", class = "error")
})


test_that("hdl_lst works", {
  expect_equal(hdl_lst(NULL), "")
  expect_equal(hdl_lst(character(0)), "")

  expect_equal(hdl_lst(raw(0)), "")
  expect_equal(hdl_lst(raw(5)), "raw bytes, length: 5")

  expect_error(hdl_lst(), "argument \"x\" is missing")

  expect_equal(hdl_lst(list(foo = "bar")), "foo=bar")
  expect_equal(hdl_lst(list(foo = "5")), "foo=5")
  expect_equal(hdl_lst(list(foo = "5", bar = "a")), "foo=5, bar=a")

  expect_equal(hdl_lst(1.5), 1.5)
})


test_that("hdl_lst2 works", {
  expect_equal(hdl_lst2(NULL), "")
  expect_equal(hdl_lst2(character(0)), "")

  expect_equal(hdl_lst2(raw(5)), "")
  expect_equal(hdl_lst2(charToRaw("hello")), "hello")

  expect_error(hdl_lst2(), "argument \"x\" is missing")

  expect_equal(hdl_lst2(list(foo = "bar")), "foo=\"bar\"")
  expect_equal(hdl_lst2(list(foo = 5)), "foo=5")
  expect_equal(hdl_lst2(list(foo = 5, bar = "a")), "foo=5, bar=\"a\"")
  expect_equal(
    hdl_lst2(list(foo = "bar", stuff = FALSE)),
    "foo=\"bar\", stuff=FALSE"
  )

  expect_equal(hdl_lst2(1.5), 1.5)
})

test_that("query_mapper", {
  expect_type(query_mapper, "closure")
  expect_null(query_mapper(NULL))
  expect_equal(query_mapper(5), 5)
  expect_equal(query_mapper("aaa"), "aaa")
  expect_equal(query_mapper(mtcars), mtcars)
})
