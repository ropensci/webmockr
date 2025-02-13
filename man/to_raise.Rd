% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_raise.R
\name{to_raise}
\alias{to_raise}
\title{Set raise error condition}
\usage{
to_raise(.data, ...)
}
\arguments{
\item{.data}{input. Anything that can be coerced to a \code{StubbedRequest}
class object}

\item{...}{One or more HTTP exceptions from the \pkg{fauxpas} package. Run
\code{grep("HTTP*", getNamespaceExports("fauxpas"), value = TRUE)} for a list of
possible exceptions}
}
\value{
an object of class \code{StubbedRequest}, with print method describing
the stub
}
\description{
Set raise error condition
}
\details{
The behavior in the future will be:

When multiple exceptions are passed, the first is used on the first
mock, the second on the second mock, and so on. Subsequent mocks use the
last exception

But for now, only the first exception is used until we get that fixed
}
\note{
see examples in \code{\link[=stub_request]{stub_request()}}
}
\section{Raise vs. Return}{

\code{to_raise()} always raises a stop condition, while \code{to_return(status=xyz)}
only sets the status code on the returned HTTP response object. So if you
want to raise a stop condition then \code{to_raise()} is what you want. But if
you don't want to raise a stop condition use \code{to_return()}. Use cases for
each vary. For example, in a unit test you may have a test expecting a 503
error; in this case \code{to_raise()} makes sense. In another case, if a unit
test expects to test some aspect of an HTTP response object that httr,
httr2, or crul typically returns, then you'll want \code{to_return()}.
}

