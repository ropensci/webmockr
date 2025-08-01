% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RequestRegistry.R
\name{RequestRegistry}
\alias{RequestRegistry}
\title{RequestRegistry}
\description{
keeps track of HTTP requests
}
\seealso{
\code{\link[=stub_registry]{stub_registry()}} and \link{StubRegistry}

Other request-registry: 
\code{\link{HashCounter}},
\code{\link{request_registry}()}
}
\concept{request-registry}
\keyword{internal}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{request_signatures}}{a HashCounter object}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-RequestRegistry-print}{\code{RequestRegistry$print()}}
\item \href{#method-RequestRegistry-reset}{\code{RequestRegistry$reset()}}
\item \href{#method-RequestRegistry-register_request}{\code{RequestRegistry$register_request()}}
\item \href{#method-RequestRegistry-times_executed}{\code{RequestRegistry$times_executed()}}
\item \href{#method-RequestRegistry-clone}{\code{RequestRegistry$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RequestRegistry-print"></a>}}
\if{latex}{\out{\hypertarget{method-RequestRegistry-print}{}}}
\subsection{Method \code{print()}}{
print method for the \code{RequestRegistry} class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RequestRegistry$print(x, ...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-RequestRegistry-reset"></a>}}
\if{latex}{\out{\hypertarget{method-RequestRegistry-reset}{}}}
\subsection{Method \code{reset()}}{
Reset the registry to no registered requests
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RequestRegistry$reset()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
nothing returned; resets registry to no requests
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RequestRegistry-register_request"></a>}}
\if{latex}{\out{\hypertarget{method-RequestRegistry-register_request}{}}}
\subsection{Method \code{register_request()}}{
Register a request
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RequestRegistry$register_request(request)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{request}}{a character string of the request, serialized from
a \code{RequestSignature$new(...)$to_s()}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
nothing returned; registers the request
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RequestRegistry-times_executed"></a>}}
\if{latex}{\out{\hypertarget{method-RequestRegistry-times_executed}{}}}
\subsection{Method \code{times_executed()}}{
How many times has a request been made
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RequestRegistry$times_executed(request_pattern)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{request_pattern}}{an object of class \code{RequestPattern}}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
if no match is found for the request pattern, 0 is returned
}

\subsection{Returns}{
integer, the number of times the request has been made
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RequestRegistry-clone"></a>}}
\if{latex}{\out{\hypertarget{method-RequestRegistry-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RequestRegistry$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
