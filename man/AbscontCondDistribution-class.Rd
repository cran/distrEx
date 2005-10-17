\name{AbscontCondDistribution-class}
\docType{class}
\alias{AbscontCondDistribution-class}

\title{Absolutely continuous conditional distribution}
\description{The class of absolutely continuous conditional univariate distributions.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("AbscontCondDistribution", ...)}.
}
\section{Slots}{
  \describe{
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
\seealso{\code{\link{UnivariateCondDistribution-class}}, \code{\link[distr]{Distribution-class}}}
\examples{new("AbscontCondDistribution")}
\keyword{distribution}
\keyword{classes}
