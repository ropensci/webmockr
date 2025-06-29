% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Response.R
\name{Response}
\alias{Response}
\title{Response}
\description{
custom webmockr http response class
}
\examples{
(x <- Response$new())

x$set_url("https://httpbin.org/get")
x

x$set_request_headers(list("Content-Type" = "application/json"))
x
x$request_headers

x$set_response_headers(list("Host" = "httpbin.org"))
x
x$response_headers

x$set_status(404)
x
x$get_status()

x$set_body("hello world")
x
x$get_body()
# raw body
x$set_body(charToRaw("hello world"))
x
x$get_body()

x$set_exception("exception")
x
x$get_exception()
}
\keyword{internal}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{url}}{(character) a url}

\item{\code{body}}{(various) list, character, etc}

\item{\code{content}}{(various) response content/body}

\item{\code{request_headers}}{(list) a named list}

\item{\code{response_headers}}{(list) a named list}

\item{\code{options}}{(character) list}

\item{\code{status_code}}{(integer) an http status code}

\item{\code{exception}}{(character) an exception message}

\item{\code{should_timeout}}{(logical) should the response timeout?}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Response-new}{\code{Response$new()}}
\item \href{#method-Response-print}{\code{Response$print()}}
\item \href{#method-Response-set_url}{\code{Response$set_url()}}
\item \href{#method-Response-get_url}{\code{Response$get_url()}}
\item \href{#method-Response-set_request_headers}{\code{Response$set_request_headers()}}
\item \href{#method-Response-get_request_headers}{\code{Response$get_request_headers()}}
\item \href{#method-Response-set_response_headers}{\code{Response$set_response_headers()}}
\item \href{#method-Response-get_respone_headers}{\code{Response$get_respone_headers()}}
\item \href{#method-Response-set_body}{\code{Response$set_body()}}
\item \href{#method-Response-get_body}{\code{Response$get_body()}}
\item \href{#method-Response-set_status}{\code{Response$set_status()}}
\item \href{#method-Response-get_status}{\code{Response$get_status()}}
\item \href{#method-Response-set_exception}{\code{Response$set_exception()}}
\item \href{#method-Response-get_exception}{\code{Response$get_exception()}}
\item \href{#method-Response-clone}{\code{Response$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Response-new"></a>}}
\if{latex}{\out{\hypertarget{method-Response-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{Response} object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Response$new(options = list())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{options}}{(list) a list of options}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{Response} object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Response-print"></a>}}
\if{latex}{\out{\hypertarget{method-Response-print}{}}}
\subsection{Method \code{print()}}{
print method for the \code{Response} class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Response$print(x, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{self}

\item{\code{...}}{ignored}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Response-set_url"></a>}}
\if{latex}{\out{\hypertarget{method-Response-set_url}{}}}
\subsection{Method \code{set_url()}}{
set the url for the response
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Response$set_url(url)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{url}}{(character) a url}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
nothing returned; sets url
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Response-get_url"></a>}}
\if{latex}{\out{\hypertarget{method-Response-get_url}{}}}
\subsection{Method \code{get_url()}}{
get the url for the response
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Response$get_url()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
(character) a url
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Response-set_request_headers"></a>}}
\if{latex}{\out{\hypertarget{method-Response-set_request_headers}{}}}
\subsection{Method \code{set_request_headers()}}{
set the request headers for the response
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Response$set_request_headers(headers, capitalize = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{headers}}{(list) named list}

\item{\code{capitalize}}{(logical) whether to capitalize first letters of
each header; default: \code{TRUE}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
nothing returned; sets request headers on the response
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Response-get_request_headers"></a>}}
\if{latex}{\out{\hypertarget{method-Response-get_request_headers}{}}}
\subsection{Method \code{get_request_headers()}}{
get the request headers for the response
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Response$get_request_headers()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
(list) request headers, a named list
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Response-set_response_headers"></a>}}
\if{latex}{\out{\hypertarget{method-Response-set_response_headers}{}}}
\subsection{Method \code{set_response_headers()}}{
set the response headers for the response
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Response$set_response_headers(headers, capitalize = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{headers}}{(list) named list}

\item{\code{capitalize}}{(logical) whether to capitalize first letters of
each header; default: \code{TRUE}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
nothing returned; sets response headers on the response
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Response-get_respone_headers"></a>}}
\if{latex}{\out{\hypertarget{method-Response-get_respone_headers}{}}}
\subsection{Method \code{get_respone_headers()}}{
get the response headers for the response
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Response$get_respone_headers()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
(list) response headers, a named list
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Response-set_body"></a>}}
\if{latex}{\out{\hypertarget{method-Response-set_body}{}}}
\subsection{Method \code{set_body()}}{
set the body of the response
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Response$set_body(body, disk = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{body}}{(various types)}

\item{\code{disk}}{(logical) whether its on disk; default: \code{FALSE}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
nothing returned; sets body on the response
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Response-get_body"></a>}}
\if{latex}{\out{\hypertarget{method-Response-get_body}{}}}
\subsection{Method \code{get_body()}}{
get the body of the response
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Response$get_body()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
various
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Response-set_status"></a>}}
\if{latex}{\out{\hypertarget{method-Response-set_status}{}}}
\subsection{Method \code{set_status()}}{
set the http status of the response
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Response$set_status(status)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{status}}{(integer) the http status}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
nothing returned; sets the http status of the response
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Response-get_status"></a>}}
\if{latex}{\out{\hypertarget{method-Response-get_status}{}}}
\subsection{Method \code{get_status()}}{
get the http status of the response
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Response$get_status()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
(integer) the http status
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Response-set_exception"></a>}}
\if{latex}{\out{\hypertarget{method-Response-set_exception}{}}}
\subsection{Method \code{set_exception()}}{
set an exception
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Response$set_exception(exception)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{exception}}{(character) an exception string}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
nothing returned; sets an exception
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Response-get_exception"></a>}}
\if{latex}{\out{\hypertarget{method-Response-get_exception}{}}}
\subsection{Method \code{get_exception()}}{
get the exception, if set
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Response$get_exception()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
(character) an exception
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Response-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Response-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Response$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
