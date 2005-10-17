\name{EuclideanSpace}
\alias{EuclideanSpace}

\title{Generating function for EuclideanSpace-class}
\description{
  Generates an object of class \code{"EuclideanSpace"}.
}
\usage{
EuclideanSpace(dimension = 1)
}
\arguments{
  \item{dimension}{ positive integer: dimension of 
  the Euclidean space (default \code{=1})}
}
%\details{}
\value{Object of class \code{"EuclideanSpace"}}
%\references{}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link[distr]{EuclideanSpace-class}}}
\examples{EuclideanSpace(dimension = 3)

## The function is currently defined as
function(dimension = 1){ 
    new("EuclideanSpace", dimension = dimension)
}
}
\concept{Euclidean space}
\keyword{}
