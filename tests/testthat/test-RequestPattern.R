test_that("RequestPattern: structure is correct", {
  expect_s3_class(RequestPattern, "R6ClassGenerator")

  aa <- RequestPattern$new(method = "get", uri = hb("/get"))

  expect_s3_class(aa, "RequestPattern")
  expect_null(aa$body_pattern)
  expect_null(aa$headers_pattern)
  expect_type(aa$clone, "closure")
  expect_type(aa$initialize, "closure")
  expect_type(aa$matches, "closure")
  expect_s3_class(aa$method_pattern, "MethodPattern")
  expect_type(aa$to_s, "closure")
  expect_s3_class(aa$uri_pattern, "UriPattern")
})

test_that("RequestPattern: behaves as expected", {
  aa <- RequestPattern$new(method = "get", uri = hb("/get"))
  rs1 <- RequestSignature$new(method = "get", uri = hb("/get"))
  rs2 <- RequestSignature$new(method = "post", uri = hb("/get"))
  rs3 <- RequestSignature$new(
    method = "get",
    uri = "https:/hb.cran.dev",
    options = list(headers = list(`User-Agent` = "foobar", stuff = "things"))
  )

  expect_true(aa$matches(rs1))
  expect_false(aa$matches(rs2))
  expect_false(aa$matches(rs3))

  expect_type(aa$to_s(), "character")
  expect_match(aa$to_s(), "GET")
  expect_match(aa$to_s(), "hb.cran.dev/get")
})

test_that("RequestPattern: uri_regex", {
  x <- RequestPattern$new(method = "get", uri_regex = ".+ossref.org")
  expect_s3_class(x$uri_pattern, "UriPattern")
  expect_equal(x$uri_pattern$to_s(), "https?://.+ossref.org")
  expect_equal(x$to_s(), "GET https?://.+ossref.org")
})

test_that("RequestPattern fails well", {
  expect_error(RequestPattern$new(), "one of uri or uri_regex is required")
  x <- RequestPattern$new(method = "get", uri = hb("/get"))
  expect_error(x$matches(), "argument \"request_signature\" is missing")
  expect_error(
    x$matches("adfadf"),
    "must be of class RequestSignature"
  )
})


# BODY PATTERNS: plain text bodies and related
test_that("should match if request body and body pattern are the same", {
  aa <- RequestPattern$new(method = "get", uri = hb("/get"), body = "abc")
  rs1 <- RequestSignature$new(
    method = "get",
    uri = hb("/get"),
    options = list(body = "abc")
  )
  expect_true(aa$matches(rs1))
})

test_that("should match if request body and body pattern are the same with multline text", {
  multiline_text <- "hello\nworld"
  bb <- RequestPattern$new(
    method = "get",
    uri = hb("/get"),
    body = multiline_text
  )
  rs2 <- RequestSignature$new(
    method = "get",
    uri = hb("/get"),
    options = list(body = multiline_text)
  )
  expect_true(bb$matches(rs2))
})

# FIXME: regex in bodies not supported yet
test_that("regex", {})

test_that("should match if pattern is missing body but is in signature", {
  cc <- RequestPattern$new(method = "get", uri = hb("/get"))
  rs3 <- RequestSignature$new(
    method = "get",
    uri = hb("/get"),
    options = list(body = "abc")
  )
  expect_true(cc$matches(rs3))
})

test_that("should not match if pattern has body specified as NA but request body is not empty", {
  dd <- RequestPattern$new(method = "get", uri = hb("/get"), body = NA)
  rs4 <- RequestSignature$new(
    method = "get",
    uri = hb("/get"),
    options = list(body = "abc")
  )
  expect_false(dd$matches(rs4))
})

test_that("should not match if pattern has body specified as empty string but request body is not empty", {
  ee <- RequestPattern$new(method = "get", uri = hb("/get"), body = "")
  rs5 <- RequestSignature$new(
    method = "get",
    uri = hb("/get"),
    options = list(body = "abc")
  )
  expect_false(ee$matches(rs5))
})

test_that("should not match if pattern has body specified but request has no body", {
  ff <- RequestPattern$new(method = "get", uri = hb("/get"), body = "abc")
  rs6 <- RequestSignature$new(method = "get", uri = hb("/get"))
  expect_false(ff$matches(rs6))
})


test_that("should match when pattern body is json or list", {
  body_list <- list(
    a = "1",
    b = "five",
    c = list(
      d = list("e", "f")
    )
  )

  # These should both be TRUE
  pattern_as_list <- RequestPattern$new(
    method = "get",
    uri = hb("/get"),
    body = body_list
  )
  rs7 <- RequestSignature$new(
    method = "get",
    uri = hb("/get"),
    options = list(
      headers = list(`Content-Type` = "application/json"),
      body = jsonlite::toJSON(body_list, auto_unbox = TRUE)
    )
  )
  expect_true(pattern_as_list$matches(rs7))

  pattern_as_json <- RequestPattern$new(
    method = "get",
    uri = hb("/get"),
    body = jsonlite::toJSON(body_list, auto_unbox = TRUE)
  )
  expect_true(pattern_as_json$matches(rs7))
})

test_that("should match when pattern body is a list and body is various content types", {
  pattern <- RequestPattern$new(
    method = "get",
    uri = hb("/get"),
    body = list(data = list(a = "1", b = "five"))
  )
  rs_xml <- RequestSignature$new(
    method = "get",
    uri = hb("/get"),
    options = list(
      headers = list(`Content-Type` = "application/xml"),
      body = '<data a="1" b="five" />'
    )
  )
  expect_true(pattern$matches(rs_xml))

  xml_employees_text <- '
    <company>
      <employees company="MacroSoft" division="Sales">
        <employee empno="7369" ename="SMITH" job="CLERK" hiredate="17-DEC-1980"/>
        <employee empno="7499" ename="ALLEN" job="SALESMAN" hiredate="20-FEB-1981"/>
      </employees>
      <employees company="MacroSoft" division="Research">
        <employee empno="7698" ename="BLAKE" job="MANAGER" hiredate="01-MAY-1981"/>
        <employee empno="7782" ename="CLARK" job="MANAGER" hiredate="09-JUN-1981"/>
      </employees>
    </company>'

  xml_employees_list <- list(
    company = list(
      employees = list(
        company = "MacroSoft",
        division = "Sales",
        employee = list(
          empno = "7369",
          ename = "SMITH",
          job = "CLERK",
          hiredate = "17-DEC-1980"
        ),
        employee = list(
          empno = "7499",
          ename = "ALLEN",
          job = "SALESMAN",
          hiredate = "20-FEB-1981"
        )
      ),
      employees = list(
        company = "MacroSoft",
        division = "Research",
        employee = list(
          empno = "7698",
          ename = "BLAKE",
          job = "MANAGER",
          hiredate = "01-MAY-1981"
        ),
        employee = list(
          empno = "7782",
          ename = "CLARK",
          job = "MANAGER",
          hiredate = "09-JUN-1981"
        )
      )
    )
  )

  pattern2 <- RequestPattern$new(
    method = "get",
    uri = hb("/get"),
    body = xml_employees_list
  )
  rs_xml2 <- RequestSignature$new(
    method = "get",
    uri = hb("/get"),
    options = list(
      headers = list(`Content-Type` = "application/xml"),
      body = xml_employees_text
    )
  )
  expect_true(pattern2$matches(rs_xml2))
})

test_that("should warn when xml parsing fails and fall back to the xml string", {
  pattern <- RequestPattern$new(
    method = "get",
    uri = hb("/get"),
    body = '<data a="1" b="five" />'
  )
  rs_xml_parse_fail <- RequestSignature$new(
    method = "get",
    uri = hb("/get"),
    options = list(
      headers = list(`Content-Type` = "application/xml"),
      body = '<data a="1" b="five" '
    )
  )
  expect_false(pattern$matches(rs_xml_parse_fail))
  # expect_warning(pattern$matches(rs_xml_parse_fail)) # FIXME: should throw warning
})

test_that("should work with basic_auth", {
  pattern <- RequestPattern$new(
    method = "get",
    uri = hb("/get"),
    basic_auth = c("user", "pass")
  )

  expect_equal(
    pattern$headers_pattern$to_s(),
    "authorization=\"Basic dXNlcjpwYXNz\""
  )

  rs_basic_auth <- RequestSignature$new(
    method = "get",
    uri = hb("/get"),
    options = list(headers = prep_auth("user:pass"))
  )
  expect_true(pattern$matches(rs_basic_auth))
})


test_that("MethodPattern: structure is correct", {
  expect_s3_class(MethodPattern, "R6ClassGenerator")

  aa <- MethodPattern$new(pattern = "get")

  expect_s3_class(aa, "MethodPattern")
  expect_type(aa$pattern, "character")
  expect_equal(aa$pattern, "get")
  expect_true(aa$matches(method = "get"))
  expect_false(aa$matches(method = "post"))

  expect_error(
    expect_type(aa$matches(), "closure"),
    "argument \"method\" is missing"
  )
})


test_that("HeadersPattern: structure is correct", {
  expect_s3_class(HeadersPattern, "R6ClassGenerator")

  aa <- HeadersPattern$new(pattern = list(a = 5))

  expect_s3_class(aa, "HeadersPattern")
  expect_type(aa$pattern, "list")
  expect_named(aa$pattern, "a")
  expect_true(aa$matches(headers = list(a = 5)))
  expect_false(aa$matches(headers = list(a = 6)))
  expect_false(aa$matches(list()))

  # with pattern empty
  bb <- HeadersPattern$new(pattern = list())
  expect_true(bb$matches(list()))

  expect_error(
    expect_type(aa$matches(), "closure"),
    "argument \"headers\" is missing"
  )

  expect_equal(aa$to_s(), "a=5")
})


test_that("BodyPattern: structure is correct", {
  expect_s3_class(BodyPattern, "R6ClassGenerator")

  bb <- RequestSignature$new(
    method = "get",
    uri = hb("/get"),
    options = list(
      body = list(foo = "bar", a = 5)
    )
  )

  aa <- BodyPattern$new(pattern = list(foo = "bar"))
  expect_s3_class(aa, "BodyPattern")
  expect_type(aa$pattern, "list")
  expect_named(aa$pattern, "foo")
  expect_false(aa$matches(bb$body))

  aaa <- BodyPattern$new(pattern = list(foo = "bar", a = 5))
  expect_true(aaa$matches(bb$body))

  aaaa <- BodyPattern$new(pattern = list(foo = "bar", a = 5, b = "asdad"))
  expect_false(aaaa$matches(bb$body))

  # with pattern empty
  empties <- BodyPattern$new(pattern = list())
  expect_true(empties$matches(list()))

  # with pattern and body
  strings <- BodyPattern$new(pattern = "some string")
  expect_true(strings$matches("some string"))
  # expect_false(strings$matches("some string"))

  # error behavior
  expect_error(
    aa$matches(),
    "argument \"body\" is missing"
  )

  expect_equal(aa$to_s(), list(foo = "bar"))
})

test_that("BodyPattern: converts json/character to list internally", {
  skip_if_not_installed("httr")
  suppressPackageStartupMessages(library("httr", warn.conflicts = FALSE))

  enable(quiet = TRUE)

  # via https://github.com/ropensci/webmockr/issues/139
  # and https://github.com/mdneuzerling/lambdr/issues/40
  response_body <- as.character(jsonlite::toJSON(
    list(parity = "odd"),
    auto_unbox = TRUE
  ))

  stub_request("post", "http://pink.tv/pajamas") |>
    wi_th(body = response_body) |>
    to_return(status = 200)

  res <- POST(url = "http://pink.tv/pajamas", body = response_body)
  expect_s3_class(res, "response")
  expect_equal(status_code(res), 200)

  disable(quiet = TRUE)
})


test_that("UriPattern: structure is correct", {
  expect_s3_class(UriPattern, "R6ClassGenerator")

  aa <- UriPattern$new(pattern = "http://foobar.com")

  expect_s3_class(aa, "UriPattern")
  expect_type(aa$pattern, "character")
  expect_false(aa$regex)
  expect_match(aa$pattern, "foobar")
  # matches w/o slash
  expect_true(aa$matches("http://foobar.com"))
  # and matches w/ slash
  expect_true(aa$matches("http://foobar.com/"))

  # fails well
  expect_error(
    expect_type(aa$matches(), "closure"),
    "argument \"uri\" is missing"
  )

  # regex usage
  z <- UriPattern$new(regex_pattern = ".+ample\\..")

  expect_s3_class(z, "UriPattern")
  expect_type(z$pattern, "character")
  expect_true(z$regex)
  expect_true(z$matches("http://sample.org"))
  expect_true(z$matches("http://example.com"))
  expect_false(z$matches("http://tramples.net"))

  # add query params usage
  z <- UriPattern$new(pattern = "http://foobar.com")
  expect_equal(z$pattern, "http://foobar.com")
  z$add_query_params(list(pizza = "cheese", cheese = "cheddar"))
  expect_equal(z$pattern, "http://foobar.com?pizza=cheese&cheese=cheddar")
  ## query params in uri only
  z <- UriPattern$new(pattern = "http://foobar.com?pizza=cheese&cheese=cheddar")
  expect_equal(z$pattern, "http://foobar.com?pizza=cheese&cheese=cheddar")
  ## before running add_query_params(), query_params_matches() of UriPattern won't match
  expect_false(z$matches("http://foobar.com?pizza=cheese&cheese=cheddar"))
  z$add_query_params()
  ## after unning add_query_params(), we should match
  expect_true(z$matches("http://foobar.com?pizza=cheese&cheese=cheddar"))

  # matches urls without scheme
  # - does match with "http"
  # - does not match with "https"
  z <- UriPattern$new(pattern = "foobar.com")
  expect_equal(z$pattern, "http://foobar.com")
  expect_true(z$matches("http://foobar.com"))
  expect_false(z$matches("https://foobar.com"))

  # regex with query parameters
  z <- UriPattern$new(regex_pattern = "https://x.com/.+/order\\?fruit=apple")

  expect_s3_class(z, "UriPattern")
  expect_type(z$pattern, "character")
  expect_true(z$regex)
  expect_true(z$matches("https://x.com/a/order?fruit=apple"))
  expect_true(z$matches("https://x.com/b/order?fruit=apple"))
  expect_false(z$matches("https://x.com/a?fruit=apple"))
})
