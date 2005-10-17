\name{DiscreteDistribution}
\alias{DiscreteDistribution}

\title{Generating function for DiscreteDistribution-class}
\description{
  Generates an object of class \code{"DiscreteDistribution"}.
}
\synopsis{DiscreteDistribution(supp, prob)}
\usage{
  DiscreteDistribution(supp, prob)
  DiscreteDistribution(supp)
}
\arguments{
  \item{supp}{ numeric vector which forms the support 
    of the discrete distribution. }
  \item{prob}{ vector of probability weights for the 
    elements of \code{supp}.}
}
\details{
  If \code{prob} is missing, all elements in \code{supp}
  are equally weighted.
}
\value{Object of class \code{"DiscreteDistribution"}}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link[distr]{DiscreteDistribution-class}}}
\examples{
# Dirac-measure at 0
D1 <- DiscreteDistribution(supp = 0)
support(D1)

# simple discrete distribution
D2 <- DiscreteDistribution(supp = c(1:5), prob = c(0.1, 0.2, 0.3, 0.2, 0.2))
plot(D2)
}
\concept{discrete distribution}
\keyword{distribution}
