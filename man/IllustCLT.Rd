\name{illustrateCLT}
\alias{illustrateCLT}
\alias{illustrateCLT-methods}
\alias{illustrateCLT,AbscontDistribution-method}
\alias{illustrateCLT,DiscreteDistribution-method}

\title{Generic Function for Illustrating the CLT}
\description{Generic function for generating a sequence of plots of
the density and cdf of the consecutive standardized and centered sums of iid r.v. distributed
according to a prescribed discrete or absolutely continuous distribution compared to
the standard normal.}
\usage{illustrateCLT(Distr, ...)
\S4method{illustrateCLT}{AbscontDistribution}(Distr, len, sleep = 0)
\S4method{illustrateCLT}{DiscreteDistribution}(Distr, len, sleep = 0)
}
\arguments{
  \item{Distr}{object of class \code{"AbscontDistribution"} or class \code{"DiscreteDistribution"}: distribution of the summands}
  \item{len}{integer: up to which number of summands plots are generated}
  \item{sleep}{numeric: pause between subsequent plots }
  \item{\dots}{addtional arguments for methods not yet implemented }
}}
%\details{}
\value{void}

\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
distroptions("DefaultNrFFTGridPointsExponent" = 13)
illustrateCLT(Distr = Unif(), len = 20)
distroptions("DefaultNrFFTGridPointsExponent" = 12)
illustrateCLT(Distr = Pois(lambda = 2), len = 20)
distroptions("DefaultNrFFTGridPointsExponent" = 13)
illustrateCLT(Distr = Pois(lambda = 2)+Unif(), len = 20)
}
\concept{convex contamination}
\concept{gross error}
\keyword{distribution}
