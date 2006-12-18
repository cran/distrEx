\name{distrEx-package}
\alias{distrEx-package}
\alias{distrEx}
\docType{package}
\title{
distrEx -- Extensions of package distr
}
\description{
\pkg{distrEx} proides some extensions of package \pkg{distr}:
\itemize{\item  extreme value distribution classes,
         \item expectations in the form 
         \itemize{\item \code{E(X)} for the expectation of a distribution object \code{X} 
                  \item \code{E(X,f)} for the expectation of \code{f(X)} where \code{X} is
                         some distribution object and \code{f} some function in \code{X} }
\item further functionals: var, sd, IQR, mad, median
\item truncated moments,
\item distances between distributions
     (Hellinger, Kolmogorov, total variation, "convex contamination")
\item lists of distributions,
\item conditional distributions in factorized form 
\item conditional expectations in factorized form
}}

\details{
\tabular{ll}{
Package: \tab distrEx\cr
Version: \tab 1.8\cr
Date: \tab 2006-12-18\cr
Depends: \tab R(>= 2.2.0), methods, distr(>= 1.8) , evd\cr
SaveImage: \tab no\cr
LazyLoad: \tab yes\cr
License: \tab GPL (version 2 or later)\cr
URL: \tab http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/\cr
}
}
\section{Classes}{
\preformatted{

Distribution Classes

"Distribution" (from distr)
|>"UnivariateDistribution" (from distr)
|>|>"AbscontDistribution" (from distr)
|>|>|>"Gumbel"
|>"MultivariateDistribution"
|>|>"DiscreteMVDistribution-class"
|>"UnivariateCondDistribution"
|>|>"AbscontCondDistribution"
|>|>|>"PrognCondDistribution"
|>|>"DiscreteCondDistribution"

"DistrList" 
|>"UnivarDistrList"


Condition Classes

"Condition"
|>"EuclCondition"
|>"PrognCondition"

Parameter Classes

"OptionalParameter" (from distr)
|>"Parameter" (from distr)
|>|>"LMParameter"
|>|>"GumbelParameter"
}
}
\section{Functions}{

\preformatted{

Integration:
GLIntegrate             Gauss-Legendre Quadrature
distrExIntegrate        Integration of One-Dimensional Functions

Options:
distrExOptions          Function to change the global variables of the
                        package 'distrEx'
Standardization:
make01                  Centering and Standardization of Univariate
                        Distributions

}}

\section{Generating Functions}{
\preformatted{

Distribution Classes
ConvexContamination     Generic Function for Generating Convex
                        Contaminations
DiscreteDistribution    Generating function for
                        DiscreteDistribution-class
DiscreteMVDistribution
                        Generating function for
                        DiscreteMVDistribution-class
Gumbel                  Generating function for Gumbel-class
LMCondDistribution      Generating function for the conditional
                        distribution of a linear regression model.
UnivarDistrList         Generating function for UnivarDistrList-class

Condition Classes
EuclCondition           Generating function for EuclCondition-class

Parameter Classes
LMParameter             Generating function for LMParameter-class

Space Classes
EuclideanSpace          Generating function for EuclideanSpace-class
Naturals                Generating function for Naturals-class
Reals                   Generating function for Reals-class
}}
\section{Methods}{
\preformatted{
Distances:
ContaminationSize       Generic Function for the Computation of the
                        Convex Contamination (Pseudo-)Distance of Two
                        Distributions
HellingerDist           Generic function for the computation of the
                        Hellinger distance of two distributions
KolmogorovDist          Generic function for the computation of the
                        Kolmogorov distance of two distributions
TotalVarDist            Generic function for the computation of the
                        total variation distance of two distributions

liesInSupport           Generic Function for Testing the Support of a
                        Distribution


Functionals:
E                       Generic Function for the Computation of
                        (Conditional) Expectations
var                     Generic Functions for the Computation of
                        Functionals
IQR                     Generic Functions for the Computation of
                        Functionals
sd                      Generic Functions for the Computation of
                        Functionals
mad                     Generic Functions for the Computation of
                        Functionals
median                  Generic Functions for the Computation of
                        Functionals

truncated Moments:
m1df                    Generic Function for the Computation of Clipped
                        First Moments
m2df                    Generic function for the computation of clipped
                        second moments

}
}

\section{Demos}{
Demos are available --- see \code{demo(package="distrEx")}.}

\section{Start-up-Banner}{
You may suppress the start-up banner/message completely by setting \code{options("StartupBanner"="off")}
somewhere before loading this package by \code{library} or \code{require} in your R-code / R-session.

If option \code{"StartupBanner"} is not defined (default) or setting    
\code{options("StartupBanner"=NULL)} or  \code{options("StartupBanner"="complete")} 
the complete start-up banner is displayed.

For any other value of option \code{"StartupBanner"} (i.e., not in \code{c(NULL,"off","complete")})
only the version information is displayed.

The same can be achieved by wrapping the \code{library} or \code{require}  call into
either \code{suppressStartupMessages()} or \code{onlytypeStartupMessages(.,atypes="version")}
}

\author{
Matthias Kohl \email{Matthias.Kohl@stamats.de} and \cr
Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr 

\emph{Maintainer:}  Matthias Kohl \email{Matthias.Kohl@stamats.de}
}
\references{
P. Ruckdeschel, M. Kohl, T. Stabla, F. Camphausen (2006):
S4 Classes for Distributions, {\em R News}, {\em 6}(2), 2-6. 
\url{http://CRAN.R-project.org/doc/Rnews/Rnews_2006-2.pdf}

%a more detailed manual for \pkg{distr}, \pkg{distrSim}, \pkg{distrTEst}, and \pkg{distrEx} may be downloaded from  
%\url{http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf}\cr

a vignette for packages \pkg{distr}, \pkg{distrSim}, \pkg{distrTEst}, and \pkg{distrEx} 
is included into the mere documentation package \pkg{distrDoc} and may be called by 
\code{require("distrDoc");vignette("distr")}

a homepage to this package is available under\cr
\url{http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distrEx.html}

M. Kohl (2005): \emph{Numerical Contributions to the Asymptotic Theory of Robustness.}
PhD Thesis. Bayreuth. Available as \url{http://www.stamats.de/ThesisMKohl.pdf}
}
\keyword{package}
\seealso{
\code{\link[distr]{distr}} 
}
