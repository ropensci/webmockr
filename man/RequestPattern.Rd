% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RequestPattern.R
\name{RequestPattern}
\alias{RequestPattern}
\title{RequestPattern class}
\description{
class handling all request matchers
}
\examples{
\dontrun{
(x <- RequestPattern$new(method = "get", uri = "httpbin.org/get"))
x$body_pattern
x$headers_pattern
x$method_pattern
x$uri_pattern
x$to_s()

# make a request signature
rs <- RequestSignature$new(method = "get", uri = "http://httpbin.org/get")

# check if it matches
x$matches(rs)

# regex uri
(x <- RequestPattern$new(method = "get", uri_regex = ".+ossref.org"))
x$uri_pattern
x$uri_pattern$to_s()
x$to_s()

# uri with query parameters
(x <- RequestPattern$new(
  method = "get", uri = "https://httpbin.org/get",
  query = list(foo = "bar")
))
x$to_s()
## query params included in url, not separately
(x <- RequestPattern$new(
  method = "get", uri = "https://httpbin.org/get?stuff=things"
))
x$to_s()
x$query_params

# just headers (via setting method=any & uri_regex=.+)
headers <- list(
  "User-Agent" = "Apple",
  "Accept-Encoding" = "gzip, deflate",
  "Accept" = "application/json, text/xml, application/xml, */*"
)
x <- RequestPattern$new(
  method = "any",
  uri_regex = ".+",
  headers = headers
)
x$to_s()
rs <- RequestSignature$new(
  method = "any", uri = "http://foo.bar",
  options = list(headers = headers)
)
rs
x$matches(rs)

# body
x <- RequestPattern$new(
  method = "post", uri = "httpbin.org/post",
  body = list(y = crul::upload(system.file("CITATION")))
)
x$to_s()
rs <- RequestSignature$new(
  method = "post", uri = "http://httpbin.org/post",
  options = list(
    body = list(y = crul::upload(system.file("CITATION")))
  )
)
rs
x$matches(rs)

# basic auth
x <- RequestPattern$new(
  method = "post",
  uri = "httpbin.org/post",
  basic_auth = c("user", "pass")
)
x
x$headers_pattern$to_s()
x$to_s()
rs <- RequestSignature$new(
  method = "post", uri = "http://httpbin.org/post",
  options = list(headers = prep_auth("user:pass"))
)
rs
x$matches(rs) # TRUE
rs <- RequestSignature$new(
  method = "post", uri = "http://httpbin.org/post",
  options = list(headers = prep_auth("user:longpassword"))
)
x$matches(rs) # FALSE
}
}
\seealso{
pattern classes for HTTP method \link{MethodPattern}, headers
\link{HeadersPattern}, body \link{BodyPattern}, and URI/URL \link{UriPattern}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{method_pattern}}{xxx}

\item{\code{uri_pattern}}{xxx}

\item{\code{body_pattern}}{xxx}

\item{\code{headers_pattern}}{xxx}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-RequestPattern-new}{\code{RequestPattern$new()}}
\item \href{#method-RequestPattern-matches}{\code{RequestPattern$matches()}}
\item \href{#method-RequestPattern-to_s}{\code{RequestPattern$to_s()}}
\item \href{#method-RequestPattern-clone}{\code{RequestPattern$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RequestPattern-new"></a>}}
\if{latex}{\out{\hypertarget{method-RequestPattern-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{RequestPattern} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RequestPattern$new(
  method,
  uri = NULL,
  uri_regex = NULL,
  query = NULL,
  body = NULL,
  headers = NULL,
  basic_auth = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{method}}{the HTTP method (any, head, options, get, post, put,
patch, trace, or delete). "any" matches any HTTP method. required.}

\item{\code{uri}}{(character) request URI. required or uri_regex}

\item{\code{uri_regex}}{(character) request URI as regex. required or uri}

\item{\code{query}}{(list) query parameters, optional}

\item{\code{body}}{(list) body request, optional}

\item{\code{headers}}{(list) headers, optional}

\item{\code{basic_auth}}{(list) vector of length 2 (username, password),
optional}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{RequestPattern} object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RequestPattern-matches"></a>}}
\if{latex}{\out{\hypertarget{method-RequestPattern-matches}{}}}
\subsection{Method \code{matches()}}{
does a request signature match the selected matchers?
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RequestPattern$matches(request_signature)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{request_signature}}{a \link{RequestSignature} object}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a boolean
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RequestPattern-to_s"></a>}}
\if{latex}{\out{\hypertarget{method-RequestPattern-to_s}{}}}
\subsection{Method \code{to_s()}}{
Print pattern for easy human consumption
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RequestPattern$to_s()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
a string
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RequestPattern-clone"></a>}}
\if{latex}{\out{\hypertarget{method-RequestPattern-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RequestPattern$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
