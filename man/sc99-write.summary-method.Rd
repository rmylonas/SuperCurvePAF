% $Id: sc99-write.summary-method.Rd 836 2014-06-27 16:42:44Z proebuck $

\name{write.summary-method}
\alias{write.summary}
\alias{write.summary-method}
\alias{write.summary,ANY-method}
\docType{methods}
\title{Method "write.summary"}
\description{
  \code{write.summary} is a generic function used like a \code{summary} method
  that writes to disk, saving summary information from the object in an
  external format. The method invokes particular \code{\link{methods}} which
  depend on the \code{\link{class}} of the first argument.
}
\usage{
\S4method{write.summary}{ANY}(object, \dots)
}
\arguments{
  \item{object}{an object for which saving summary information externally is
    desired}
  \item{\dots}{additional arguments affecting the summary information
    produced}
}
\note{
  Exactly what is written to disk by \code{write.summary} depends on the
  class of its argument. See the documentation of the particular methods
  for details of what is written by that method.
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\keyword{methods}

