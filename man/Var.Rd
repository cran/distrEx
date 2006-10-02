\name{var}
\alias{var}
\alias{var-methods}
\alias{var,ANY-method}
\alias{var,UnivariateDistribution-method}
\alias{var,Beta-method}
\alias{var,Binom-method}
\alias{var,Cauchy-method}
\alias{var,Chisq-method}
\alias{var,Dirac-method}
\alias{var,DExp-method}
\alias{var,Exp-method}
\alias{var,Fd-method}
\alias{var,Gammad-method}
\alias{var,Geom-method}
\alias{var,Hyper-method}
\alias{var,Logis-method}
\alias{var,Lnorm-method}
\alias{var,Nbinom-method}
\alias{var,Norm-method}
\alias{var,Pois-method}
\alias{var,Unif-method}
\alias{var,Weibull-method}
\alias{var,Td-method}
\alias{sd}
\alias{sd-methods}
\alias{sd,UnivariateDistribution-method}
\alias{median}
\alias{median,ANY-method}
\alias{median-methods}
\alias{median,UnivariateDistribution-method}
\alias{IQR}
\alias{IQR-methods}
\alias{IQR,ANY-method}
\alias{IQR,UnivariateDistribution-method}
\alias{mad}
\alias{mad,ANY-method}
\alias{mad-methods}
\alias{mad,UnivariateDistribution-method}

\title{Generic Function for the Computation of Functionals}
\description{
  Generic function for the computation of functionals on distributions.
}
\usage{
IQR(x, ...)

\S4method{IQR}{UnivariateDistribution}(x)

median(x, ...)

\S4method{median}{UnivariateDistribution}(x)

mad(x, ...)

\S4method{mad}{UnivariateDistribution}(x)

sd(x, ...)

\S4method{sd}{UnivariateDistribution}(x, fun, cond, withCond, useApply, ...)

var(x, ...)

\S4method{var}{UnivariateDistribution}(x, fun, cond, withCond, useApply, ...)
\S4method{var}{Binom}(x)
\S4method{var}{Beta}(x)
\S4method{var}{Cauchy}(x)
\S4method{var}{Chisq}(x)
\S4method{var}{Dirac}(x)
\S4method{var}{DExp}(x)
\S4method{var}{Exp}(x)
\S4method{var}{Fd}(x)
\S4method{var}{Gammad}(x)
\S4method{var}{Geom}(x)
\S4method{var}{Hyper}(x)
\S4method{var}{Logis}(x)
\S4method{var}{Lnorm}(x)
\S4method{var}{Nbinom}(x)
\S4method{var}{Norm}(x)
\S4method{var}{Pois}(x)
\S4method{var}{Td}(x)
\S4method{var}{Unif}(x)
\S4method{var}{Weibull}(x)
}
\arguments{
  \item{x}{ object of class \code{"UnivariateDistribution"}}
  \item{fun}{ if missing the (conditional) variance resp. standard deviation is computed
    else the (conditional) variance resp. standard deviation of \code{fun} is computed. }
  \item{cond}{ if not missing the conditional variance resp. standard deviation  
    given \code{cond} is computed. }
  \item{\dots}{ additional arguments to \code{fun} or \code{E}}
  \item{useApply}{ logical: should \code{sapply}, respectively \code{apply} 
    be used to evaluate \code{fund}.}
  \item{withCond}{ logical: is \code{cond} in the argument list of \code{fun}. }
  }
\value{
  The value of the corresponding functional at the distribution in the argument is computed.
}
\section{Methods}{
\describe{
  \item{\code{var}, \code{signature(x = "Any")}:}{
    interface to the \pkg{stats}-function \code{var} --- see \code{\link[stats]{var}}.}
  \item{\code{var}, \code{signature(x = "UnivariateDistribution")}:}{ 
    variance of univariate distributions using corresponding \code{E()}-method.}

 \item{\code{sd}, \code{signature(x = "UnivariateDistribution")}:}{ 
    standard deviation of univariate distributions using corresponding \code{E()}-method.}

  \item{\code{IQR}, \code{signature(x = "Any")}:}{
    interface to the \pkg{stats}-function \code{IQR} --- see \code{\link[stats]{IQR}}.}
  \item{\code{IQR}, \code{signature(x = "UnivariateDistribution")}:}{ 
    interquartile range of univariate distributions using corresponding \code{q()}-method.}

  \item{\code{median}, \code{signature(x = "Any")}:}{
    interface to the \pkg{stats}-function \code{median} --- see \code{\link[stats]{median}}.}
  \item{\code{median}, \code{signature(x = "UnivariateDistribution")}:}{ 
    median of univariate distributions using corresponding \code{q()}-method. }

   \item{\code{mad}, \code{signature(x = "Any")}:}{
    interface to the \pkg{stats}-function \code{mad} --- see \code{\link[stats]{mad}}.}
   \item{\code{mad}, \code{signature(x = "UnivariateDistribution")}:}{ 
    mad of univariate distributions using corresponding \code{q()}-method applied to \code{abs(x-median(x))}.}

\item{\code{var}, \code{signature(x = "Beta")}:}{ 
    for noncentrality 0 exact evaluation using explicit expressions.}  
  \item{\code{var}, \code{signature(x = "Binom")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Cauchy")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Chisq")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Dirac")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "DExp")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Exp")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Fd")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Gammad")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Geom")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Hyper")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Logis")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Lnorm")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Nbinom")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Norm")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Pois")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Td")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Unif")}:}{ 
    exact evaluation using explicit expressions.}
  \item{\code{var}, \code{signature(x = "Weibull")}:}{ 
    exact evaluation using explicit expressions.}
}}
%\references{ ~put references to the literature/web site here ~ }
\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-bayreuth.de}}
%\note{ ~~further notes~~ }
\examples{
# Variance of Exp(1) distribution
var(Exp())

#median(Exp())
IQR(Exp())
mad(Exp())

# Variance of N(1,4)^2
var(Norm(mean=1, sd=2), fun = function(x){x^2})
var(Norm(mean=1, sd=2), fun = function(x){x^2}, useApply = FALSE)

## sd -- may equivalently be replaced by var
sd(Pois()) ## uses explicit terms
sd(as(Pois(),"DiscreteDistribution")) ## uses sums
sd(as(Pois(),"UnivariateDistribution")) ## uses simulations
sd(Norm(mean=2), fun = function(x){2*x^2}) ## uses simulations
#
mad(sin(exp(Norm()+2*Pois()))) ## weird
}
\seealso{\code{\link{distrExIntegrate}}, \code{\link{m1df}}, \code{\link{m2df}},
         \code{\link[distr]{Distribution-class}},\cr
 \code{\link[stats]{sd}}, \code{\link[stats]{var}}, \code{\link[stats]{IQR}},\cr
 \code{\link[stats]{median}}, \code{\link[stats]{mad}},  \code{\link[distr]{sd}}}
\concept{functional}
\concept{var}
\concept{sd}
\concept{IQR}
\concept{mad}
\concept{median}
\keyword{methods}
\keyword{distribution}
