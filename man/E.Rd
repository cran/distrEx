\name{E}
\alias{E}
\alias{E-methods}
\alias{E,UnivariateDistribution,missing,missing-method}
\alias{E,AbscontDistribution,missing,missing-method}
\alias{E,DiscreteDistribution,missing,missing-method}
\alias{E,MultivariateDistribution,missing,missing-method}
\alias{E,DiscreteMVDistribution,missing,missing-method}
\alias{E,UnivariateDistribution,function,missing-method}
\alias{E,AbscontDistribution,function,missing-method}
\alias{E,DiscreteDistribution,function,missing-method}
\alias{E,MultivariateDistribution,function,missing-method}
\alias{E,DiscreteMVDistribution,function,missing-method}
\alias{E,UnivariateCondDistribution,missing,numeric-method}
\alias{E,AbscontCondDistribution,missing,numeric-method}
\alias{E,DiscreteCondDistribution,missing,numeric-method}
\alias{E,UnivariateCondDistribution,function,numeric-method}
\alias{E,AbscontCondDistribution,function,numeric-method}
\alias{E,DiscreteCondDistribution,function,numeric-method}

\title{Generic Function for the Computation of (Conditional) Expectations}
\description{
  Generic function for the computation of (conditional) expectations.
}
\usage{
E(object, fun, cond, ...)

\S4method{E}{UnivariateDistribution,function,missing}(object, fun, cond, useApply = TRUE, ...)

\S4method{E}{AbscontDistribution,function,missing}(object, fun, cond, useApply = TRUE, ...)

\S4method{E}{DiscreteDistribution,function,missing}(object, fun, cond, useApply = TRUE, ...)

\S4method{E}{MultivariateDistribution,function,missing}(object, fun, cond, useApply = TRUE, ...)

\S4method{E}{DiscreteMVDistribution,function,missing}(object, fun, cond, useApply = TRUE, ...)

\S4method{E}{AbscontCondDistribution,missing,numeric}(object, cond, useApply = TRUE)

\S4method{E}{DiscreteCondDistribution,missing,numeric}(object, cond, useApply = TRUE)

\S4method{E}{UnivariateCondDistribution,function,numeric}(object, fun, cond, withCond = FALSE, useApply = TRUE, ...)

\S4method{E}{AbscontCondDistribution,function,numeric}(object, fun, cond, withCond = FALSE, useApply = TRUE, ...)

\S4method{E}{DiscreteCondDistribution,function,numeric}(object, fun, cond, withCond = FALSE, useApply = TRUE, ...)
}
\arguments{
  \item{object}{ object of class \code{"Distribution"}}
  \item{fun}{ if missing the (conditional) expectation is computed
    else the (conditional) expection of \code{fun} is computed. }
  \item{cond}{ if not missing the conditional expectation 
    given \code{cond} is computed. }
  \item{\dots}{ additional arguments to \code{fun} }
  \item{useApply}{ logical: should \code{sapply}, respectively \code{apply} 
    be used to evaluate \code{fund}. }
  \item{withCond}{ logical: is \code{cond} in the argument list of \code{fun}. }
}
\details{The precision of the computations can be controlled via 
  certain global options; cf. \code{\link{distrExOptions}}. }
\value{
  The (conditional) expectation is computed.
}
\section{Methods}{
\describe{
  \item{object = "UnivariateDistribution", fun = "missing", cond = "missing":}{ 
    expectation of univariate distributions using crude Monte-Carlo integration. }

  \item{object = "AbscontDistribution", fun = "missing", cond = "missing":}{ 
    expectation of absolutely continuous univariate distributions
    using \code{distrExIntegrate}. }

  \item{object = "DiscreteDistribution", fun = "missing", cond = "missing":}{  
    expectation of discrete univariate distributions using \code{support}
    and \code{sum}.}

  \item{object = "MultivariateDistribution", fun = "missing", cond = "missing":}{
    expectation of multivariate distributions using crude Monte-Carlo integration. }

  \item{object = "DiscreteMVDistribution", fun = "missing", cond = "missing":}{
    expectation of discrete multivariate distributions. The computation is based
    on \code{support} and \code{sum}.}

  \item{object = "UnivariateDistribution", fun = "function", cond = "missing":}{ 
    expectation of \code{fun} under univariate distributions using 
    crude Monte-Carlo integration. }

  \item{object = "AbscontDistribution", fun = "function", cond = "missing":}{ 
    expectation of \code{fun} under absolutely continuous 
    univariate distributions using \code{distrExIntegrate}. }

  \item{object = "DiscreteDistribution", fun = "function", cond = "missing":}{  
    expectation of \code{fun} under discrete univariate 
    distributions using \code{support} and \code{sum}. }

  \item{object = "MultivariateDistribution", fun = "function", cond = "missing":}{
    expectation of multivariate distributions using crude Monte-Carlo integration. }

  \item{object = "DiscreteMVDistribution", fun = "function", cond = "missing":}{
    expectation of \code{fun} under discrete multivariate 
    distributions. The computation is based on \code{support} and \code{sum}. }  

  \item{object = "UnivariateCondDistribution", fun = "missing", cond = "numeric":}{
    conditional expectation for univariate conditional distributions given \code{cond}. 
    The integral is computed using crude Monte-Carlo integration. }

  \item{object = "AbscontCondDistribution", fun = "missing", cond = "numeric":}{
    conditional expectation for absolutely continuous, univariate 
    conditional distributions given \code{cond}. The computation
    is based on \code{distrExIntegrate}. }

  \item{object = "DiscreteCondDistribution", fun = "missing", cond = "numeric":}{ 
    conditional expectation for discrete, univariate conditional 
    distributions given \code{cond}. The computation is based 
    on \code{support} and \code{sum}. }

  \item{object = "UnivariateCondDistribution", fun = "function", cond = "numeric":}{
    conditional expectation of \code{fun} under univariate conditional distributions 
    given \code{cond}. The integral is computed using crude Monte-Carlo integration. }

  \item{object = "AbscontCondDistribution", fun = "function", cond = "numeric":}{
    conditional expectation of \code{fun} under absolutely continuous, 
    univariate conditional distributions given \code{cond}. The
    computation is based on \code{distrExIntegrate}. }

  \item{object = "DiscreteCondDistribution", fun = "function", cond = "numeric":}{ 
    conditional expectation of \code{fun} under discrete, univariate 
    conditional distributions given \code{cond}. The computation is
    based on \code{support} and \code{sum}. }
}}
%\references{ ~put references to the literature/web site here ~ }
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{ ~~further notes~~ }
\seealso{\code{\link{distrExIntegrate}}, \code{\link{m1df}}, \code{\link{m2df}},
    \code{\link[distr]{Distribution-class}}}
\examples{
# mean of Exp(1) distribution
E(Exp())

# second moment of N(1,4)
E(Norm(mean=1, sd=2), fun = function(x){x^2})
E(Norm(mean=1, sd=2), fun = function(x){x^2}, useApply = FALSE)

# conditional distribution of a linear model
D1 <- LMCondDistribution(theta = 1) 
E(D1, cond = 1)
E(Norm(mean=1))
E(D1, function(x){x^2}, cond = 1)
E(Norm(mean=1), fun = function(x){x^2})
E(D1, function(x, cond){cond*x^2}, cond = 2, withCond = TRUE, useApply = FALSE)
E(Norm(mean=2), function(x){2*x^2})
}
\concept{expectation}
\concept{conditional expectation}
\concept{integration}
\keyword{methods}
\keyword{distribution}
