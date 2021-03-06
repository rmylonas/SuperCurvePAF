% $Id: sc22-DS5RPPAPreFitQC-class.Rd 912 2014-07-15 20:33:13Z proebuck $

\name{DS5RPPAPreFitQC-class}
\alias{DS5RPPAPreFitQC-class}
\alias{qcprob,DS5RPPAPreFitQC-method}
\alias{summary,DS5RPPAPreFitQC-method}
\docType{class}
\title{Class "DS5RPPAPreFitQC"}
\description{
  The DS5RPPAPreFitQC class represents the inputs necessary to determine the
  quality control rating of a reverse-phase protein array slide with 5 dilution
  series.
}
\usage{
\S4method{qcprob}{DS5RPPAPreFitQC}(object, \dots)
\S4method{summary}{DS5RPPAPreFitQC}(object, \dots)
}
\arguments{
  \item{object}{object of class \code{DS5RPPAPreFitQC}}
  \item{\dots}{extra arguments for generic routines}
}
\details{
  The prediction model used multiple training datasets from the RPPA Core
  Facility by fitting a logistic regression model using an expert rating of
  a slide's quality (good, fair, or poor) as the response variable and a
  host of metrics about the raw positive control data as predicting variables.
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link[methods]{new}, the preferred method is to use the \code{RPPAPreFitQC}
  factory generator function.
}
\section{Slots}{
  \describe{
    \item{\code{slopediff}:}{numeric scalar specifying the difference from
      perfect slope}
    \item{\code{cvs}:}{numeric vector containing the coefficient of variance
      for each positive control dilution series}
    \item{\code{slopes}:}{numeric vector containing the slopes for each
      positive control dilution series}
    \item{\code{skews}:}{numeric vector containing the skews for each
      sample dilution series}
    \item{\code{drdiffs}:}{numeric vector containing the difference in dynamic
      range of each positive control dilution series}
    \item{\code{percentgood}:}{numeric scalar specifying percentage of "good"
      sample spots on the slide}
    \item{\code{adjusted}:}{logical scalar specifying if adjusted measures were
      used}
  }
}
\section{Extends}{
  Class \code{\linkS4class{RPPAPreFitQC}}, directly.
}
\section{Methods}{
  \describe{
    \item{qcprob}{
      \code{signature(object = "DS5RPPAPreFitQC")}:\cr
      Calculates the probability of good slide, returned as numeric scalar.}
    \item{summary}{
      \code{signature(object = "DS5RPPAPreFitQC")}:\cr
      Prints a summary of the underlying data frame.}
  }
}
\references{
  Ju Z, Liu W, Roebuck PL, Siwak DR, Zhang N, Lu Y, Davies MA,
  Akbani R, Weinstein JN, Mills GB, Coombes KR\cr
  \emph{Development of a Robust Classifier for Quality Control of
  Reverse Phase Protein Arrays.}\cr
  (submitted).
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\keyword{classes}
\keyword{methods}

