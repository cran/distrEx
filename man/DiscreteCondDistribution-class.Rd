\name{DiscreteCondDistribution-class}
\docType{class}
\alias{DiscreteCondDistribution-class}

\title{Discrete conditional distribution}
\description{The class of discrete conditional univariate distributions.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("DiscreteCondDistribution", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{support}:}{Object of class \code{"function"}: conditional support.}
    \item{\code{cond}:}{Object of class \code{"Condition"}: condition }
    \item{\code{img}:}{Object of class \code{"rSpace"}: the image space. }
    \item{\code{param}:}{Object of class \code{"OptionalParameter"}: an 
      optional parameter.}
    \item{\code{r}:}{Object of class \code{"function"}: 
      generates random numbers. }
    \item{\code{d}:}{Object of class \code{"OptionalFunction"}: 
      optional conditional density function. }
    \item{\code{p}:}{Object of class \code{"OptionalFunction"}: 
      optional conditional cumulative distribution function. }
    \item{\code{q}:}{Object of class \code{"OptionalFunction"}:
      optional conditional quantile function. }
  }
}
\section{Extends}{
Class \code{"UnivariateCondDistribution"}, directly.\cr
Class \code{"Distribution"}, by class \code{"UnivariateCondDistribution"}.
}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{UnivariateCondDistribution-class}}}
\examples{new("DiscreteCondDistribution")}
\concept{conditional distribution}
\concept{discrete distribution}
\keyword{classes}
\keyword{distribution}
