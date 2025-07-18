% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/webmockr-package.R
\docType{package}
\name{webmockr-package}
\alias{webmockr}
\alias{webmockr-package}
\title{webmockr: Stubbing and Setting Expectations on 'HTTP' Requests}
\description{
Stubbing and setting expectations on 'HTTP' requests. Includes tools for stubbing 'HTTP' requests, including expected request conditions and response conditions. Match on 'HTTP' method, query parameters, request body, headers and more. Can be used for unit tests or outside of a testing context.
}
\section{Features}{

\itemize{
\item Stubbing HTTP requests at low http client lib level
\item Setting and verifying expectations on HTTP requests
\item Matching requests based on method, URI, headers and body
\item Supports multiple HTTP libraries, including \pkg{crul},
\pkg{httr}, and \pkg{httr2}
\item Supports async http request mocking with \pkg{crul} only
}
}

\examples{
library(webmockr)
stub_request("get", "https://httpbin.org/get")
stub_request("post", "https://httpbin.org/post")
stub_registry()

}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/ropensci/webmockr}
  \item \url{https://books.ropensci.org/http-testing/}
  \item \url{https://docs.ropensci.org/webmockr/}
  \item Report bugs at \url{https://github.com/ropensci/webmockr/issues}
}

}
\author{
\strong{Maintainer}: Scott Chamberlain \email{myrmecocystus+r@gmail.com} (\href{https://orcid.org/0000-0003-1444-9135}{ORCID})

Other contributors:
\itemize{
  \item Aaron Wolen (\href{https://orcid.org/0000-0003-2542-2202}{ORCID}) [contributor]
  \item rOpenSci (019jywm96) [funder]
}

}
\keyword{internal}
