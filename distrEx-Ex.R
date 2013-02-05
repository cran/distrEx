pkgname <- "distrEx"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('distrEx')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("AbscontCondDistribution-class")
### * AbscontCondDistribution-class

flush(stderr()); flush(stdout())

### Name: AbscontCondDistribution-class
### Title: Absolutely continuous conditional distribution
### Aliases: AbscontCondDistribution-class
### Keywords: distribution

### ** Examples
new("AbscontCondDistribution")


cleanEx()
nameEx("AsymTotalVarDist")
### * AsymTotalVarDist

flush(stderr()); flush(stdout())

### Name: AsymTotalVarDist
### Title: Generic function for the computation of asymmetric total
###   variation distance of two distributions
### Aliases: AsymTotalVarDist AsymTotalVarDist-methods
###   AsymTotalVarDist,AbscontDistribution,AbscontDistribution-method
###   AsymTotalVarDist,AbscontDistribution,DiscreteDistribution-method
###   AsymTotalVarDist,DiscreteDistribution,DiscreteDistribution-method
###   AsymTotalVarDist,DiscreteDistribution,AbscontDistribution-method
###   AsymTotalVarDist,LatticeDistribution,DiscreteDistribution-method
###   AsymTotalVarDist,DiscreteDistribution,LatticeDistribution-method
###   AsymTotalVarDist,LatticeDistribution,LatticeDistribution-method
###   AsymTotalVarDist,numeric,DiscreteDistribution-method
###   AsymTotalVarDist,DiscreteDistribution,numeric-method
###   AsymTotalVarDist,numeric,AbscontDistribution-method
###   AsymTotalVarDist,AbscontDistribution,numeric-method
###   AsymTotalVarDist,AcDcLcDistribution,AcDcLcDistribution-method
### Keywords: distribution

### ** Examples

AsymTotalVarDist(Norm(), UnivarMixingDistribution(Norm(1,2),Norm(0.5,3),
                 mixCoeff=c(0.2,0.8)), rho=0.3)
AsymTotalVarDist(Norm(), Td(10), rho=0.3)
AsymTotalVarDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100), rho=0.3) # mutually singular
AsymTotalVarDist(Pois(10), Binom(size = 20), rho=0.3) 

x <- rnorm(100)
AsymTotalVarDist(Norm(), x, rho=0.3)
AsymTotalVarDist(x, Norm(), asis.smooth.discretize = "smooth", rho=0.3)

y <- (rbinom(50, size = 20, prob = 0.5)-10)/sqrt(5)
AsymTotalVarDist(y, Norm(), rho=0.3)
AsymTotalVarDist(y, Norm(), asis.smooth.discretize = "smooth", rho=0.3)

AsymTotalVarDist(rbinom(50, size = 20, prob = 0.5), Binom(size = 20, prob = 0.5), rho=0.3)



cleanEx()
nameEx("Condition-class")
### * Condition-class

flush(stderr()); flush(stdout())

### Name: Condition-class
### Title: Conditions
### Aliases: Condition-class name,Condition-method name<-,Condition-method
### Keywords: distribution

### ** Examples
new("Condition")


cleanEx()
nameEx("ContaminationSize")
### * ContaminationSize

flush(stderr()); flush(stdout())

### Name: ContaminationSize
### Title: Generic Function for the Computation of the Convex Contamination
###   (Pseudo-)Distance of Two Distributions
### Aliases: ContaminationSize ContaminationSize-methods
###   ContaminationSize,AbscontDistribution,AbscontDistribution-method
###   ContaminationSize,DiscreteDistribution,DiscreteDistribution-method
###   ContaminationSize,LatticeDistribution,DiscreteDistribution-method
###   ContaminationSize,DiscreteDistribution,LatticeDistribution-method
###   ContaminationSize,LatticeDistribution,LatticeDistribution-method
###   ContaminationSize,AcDcLcDistribution,AcDcLcDistribution-method
### Keywords: distribution

### ** Examples

ContaminationSize(Norm(), Norm(mean=0.1))
ContaminationSize(Pois(), Pois(1.5))



cleanEx()
nameEx("ConvexContamination")
### * ConvexContamination

flush(stderr()); flush(stdout())

### Name: ConvexContamination
### Title: Generic Function for Generating Convex Contaminations
### Aliases: ConvexContamination ConvexContamination-methods
###   ConvexContamination,UnivariateDistribution,UnivariateDistribution,numeric-method
###   ConvexContamination,AbscontDistribution,AbscontDistribution,numeric-method
###   ConvexContamination,AbscontDistribution,UnivariateDistribution,numeric-method
###   ConvexContamination,DiscreteDistribution,DiscreteDistribution,numeric-method
###   ConvexContamination,LatticeDistribution,DiscreteDistribution,numeric-method
###   ConvexContamination,DiscreteDistribution,LatticeDistribution,numeric-method
###   ConvexContamination,LatticeDistribution,LatticeDistribution,numeric-method
###   ConvexContamination,AcDcLcDistribution,AcDcLcDistribution,numeric-method
### Keywords: distribution methods

### ** Examples

# Convex combination of two normal distributions
C1 <- ConvexContamination(e1 = Norm(), e2 = Norm(mean = 5), size = 0.1)
plot(C1)



cleanEx()
nameEx("CvMDist")
### * CvMDist

flush(stderr()); flush(stdout())

### Name: CvMDist
### Title: Generic function for the computation of the Cramer - von Mises
###   distance of two distributions
### Aliases: CvMDist CvMDist-methods
###   CvMDist,UnivariateDistribution,UnivariateDistribution-method
###   CvMDist,numeric,UnivariateDistribution-method
### Keywords: distribution

### ** Examples

CvMDist(Norm(), UnivarMixingDistribution(Norm(1,2),Norm(0.5,3),
                 mixCoeff=c(0.2,0.8)))
CvMDist(Norm(), UnivarMixingDistribution(Norm(1,2),Norm(0.5,3),
                 mixCoeff=c(0.2,0.8)),mu=Norm())
CvMDist(Norm(), Td(10))
CvMDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100))
CvMDist(Pois(10), Binom(size = 20)) 
CvMDist(rnorm(100),Norm())
CvMDist((rbinom(50, size = 20, prob = 0.5)-10)/sqrt(5), Norm())
CvMDist(rbinom(50, size = 20, prob = 0.5), Binom(size = 20, prob = 0.5))
CvMDist(rbinom(50, size = 20, prob = 0.5), Binom(size = 20, prob = 0.5), mu = Pois())



cleanEx()
nameEx("DiscreteCondDistribution-class")
### * DiscreteCondDistribution-class

flush(stderr()); flush(stdout())

### Name: DiscreteCondDistribution-class
### Title: Discrete conditional distribution
### Aliases: DiscreteCondDistribution-class
### Keywords: distribution

### ** Examples
new("DiscreteCondDistribution")


cleanEx()
nameEx("DiscreteMVDistribution-class")
### * DiscreteMVDistribution-class

flush(stderr()); flush(stdout())

### Name: DiscreteMVDistribution-class
### Title: Discrete Multivariate Distributions
### Aliases: DiscreteMVDistribution-class
###   support,DiscreteMVDistribution-method
### Keywords: distribution

### ** Examples

(D1 <- new("MultivariateDistribution")) # Dirac measure in (0,0)
r(D1)(5)

(D2 <- DiscreteMVDistribution(supp = matrix(c(1:5, rep(3, 5)), ncol=2, byrow=TRUE)))
support(D2)
r(D2)(10)
d(D2)(support(D2))
p(D2)(lower = c(1,1), upper = c(3,3))
q(D2)
param(D2)
img(D2)

e1 <- E(D2) # expectation



cleanEx()
nameEx("DiscreteMVDistribution")
### * DiscreteMVDistribution

flush(stderr()); flush(stdout())

### Name: DiscreteMVDistribution
### Title: Generating function for DiscreteMVDistribution-class
### Aliases: DiscreteMVDistribution
### Keywords: distribution

### ** Examples

# Dirac-measure at (0,0,0)
D1 <- DiscreteMVDistribution(supp = c(0,0,0))
support(D1)

# simple discrete distribution
D2 <- DiscreteMVDistribution(supp = matrix(c(0,1,0,2,2,1,1,0), ncol=2), 
                prob = c(0.3, 0.2, 0.2, 0.3))
support(D2)
r(D2)(10)



cleanEx()
nameEx("E")
### * E

flush(stderr()); flush(stdout())

### Name: E
### Title: Generic Function for the Computation of (Conditional)
###   Expectations
### Aliases: E E-methods E,UnivariateDistribution,missing,missing-method
###   E,AbscontDistribution,missing,missing-method
###   E,DiscreteDistribution,missing,missing-method
###   E,LatticeDistribution,missing,missing-method
###   E,AffLinDistribution,missing,missing-method
###   E,AffLinAbscontDistribution,missing,missing-method
###   E,AffLinDiscreteDistribution,missing,missing-method
###   E,AffLinLatticeDistribution,missing,missing-method
###   E,MultivariateDistribution,missing,missing-method
###   E,DiscreteMVDistribution,missing,missing-method
###   E,UnivarLebDecDistribution,missing,missing-method
###   E,AffLinUnivarLebDecDistribution,missing,missing-method
###   E,UnivarMixingDistribution,missing,missing-method
###   E,UnivariateDistribution,function,missing-method
###   E,AbscontDistribution,function,missing-method
###   E,DiscreteDistribution,function,missing-method
###   E,LatticeDistribution,function,missing-method
###   E,MultivariateDistribution,function,missing-method
###   E,DiscreteMVDistribution,function,missing-method
###   E,UnivarLebDecDistribution,function,missing-method
###   E,UnivarMixingDistribution,function,missing-method
###   E,AcDcLcDistribution,ANY,ANY-method
###   E,CompoundDistribution,missing,missing-method
###   E,UnivariateCondDistribution,missing,numeric-method
###   E,AbscontCondDistribution,missing,numeric-method
###   E,DiscreteCondDistribution,missing,numeric-method
###   E,UnivarLebDecDistribution,missing,ANY-method
###   E,UnivarMixingDistribution,missing,ANY-method
###   E,UnivarLebDecDistribution,function,ANY-method
###   E,UnivariateCondDistribution,function,numeric-method
###   E,UnivarMixingDistribution,function,ANY-method
###   E,AbscontCondDistribution,function,numeric-method
###   E,DiscreteCondDistribution,function,numeric-method
###   E,Arcsine,missing,missing-method E,Beta,missing,missing-method
###   E,Binom,missing,missing-method E,Cauchy,missing,missing-method
###   E,Chisq,missing,missing-method E,Dirac,missing,missing-method
###   E,DExp,missing,missing-method E,Exp,missing,missing-method
###   E,Fd,missing,missing-method E,Gammad,missing,missing-method
###   E,Gammad,function,missing-method E,Geom,missing,missing-method
###   E,Gumbel,missing,missing-method E,GPareto,missing,missing-method
###   E,GPareto,function,missing-method E,GEV,missing,missing-method
###   E,GEV,function,missing-method E,Hyper,missing,missing-method
###   E,Logis,missing,missing-method E,Lnorm,missing,missing-method
###   E,Nbinom,missing,missing-method E,Norm,missing,missing-method
###   E,Pareto,missing,missing-method E,Pois,missing,missing-method
###   E,Td,missing,missing-method E,Unif,missing,missing-method
###   E,Weibull,missing,missing-method
### Keywords: methods distribution

### ** Examples

# mean of Exp(1) distribution
E <- Exp() 

E(E) ## uses explicit terms
E(as(E,"AbscontDistribution")) ## uses numerical integration
E(as(E,"UnivariateDistribution")) ## uses simulations
E(E, fun = function(x){2*x^2}) ## uses simulations

# the same operator for discrete distributions:
P <- Pois(lambda=2)

E(P) ## uses explicit terms
E(as(P,"DiscreteDistribution")) ## uses sums
E(as(P,"UnivariateDistribution")) ## uses simulations
E(P, fun = function(x){2*x^2}) ## uses simulations


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

E(as(Norm(mean=2),"AbscontDistribution"))
### somewhat less accurate:
E(as(Norm(mean=2),"AbscontDistribution"), 
     lowerTruncQuantil=1e-4,upperTruncQuantil=1e-4, IQR.fac= 4)
### even less accurate:
E(as(Norm(mean=2),"AbscontDistribution"), 
     lowerTruncQuantil=1e-2,upperTruncQuantil=1e-2, IQR.fac= 4)
### no good idea, but just as an example:
E(as(Norm(mean=2),"AbscontDistribution"), 
     lowerTruncQuantil=1e-2,upperTruncQuantil=1e-2, IQR.fac= .1)

### truncation of integration range; see also m1df...
E(Norm(mean=2), low=2,upp=4)

E(Cauchy())
E(Cauchy(),upp=3,low=-2)
# some Lebesgue decomposed distribution 
mymix <- UnivarLebDecDistribution(acPart = Norm(), discretePart = Binom(4,.4),
         acWeight = 0.4)
E(mymix)



cleanEx()
nameEx("EuclCondition-class")
### * EuclCondition-class

flush(stderr()); flush(stdout())

### Name: EuclCondition-class
### Title: Conditioning by an Euclidean space.
### Aliases: EuclCondition-class Range Range,EuclCondition-method
###   show,EuclCondition-method
### Keywords: distribution

### ** Examples

  new("EuclCondition")



cleanEx()
nameEx("EuclCondition")
### * EuclCondition

flush(stderr()); flush(stdout())

### Name: EuclCondition
### Title: Generating function for EuclCondition-class
### Aliases: EuclCondition
### Keywords: distribution

### ** Examples

EuclCondition(dimension = 3)

## The function is currently defined as
function(dimension){
    new("EuclCondition", Range = EuclideanSpace(dimension = dimension))
}



cleanEx()
nameEx("GLIntegrate")
### * GLIntegrate

flush(stderr()); flush(stdout())

### Name: GLIntegrate
### Title: Gauss-Legendre Quadrature
### Aliases: GLIntegrate
### Keywords: math utilities

### ** Examples

integrate(dnorm, -1.96, 1.96)
GLIntegrate(dnorm, -1.96, 1.96)



cleanEx()
nameEx("HellingerDist")
### * HellingerDist

flush(stderr()); flush(stdout())

### Name: HellingerDist
### Title: Generic function for the computation of the Hellinger distance
###   of two distributions
### Aliases: HellingerDist HellingerDist-methods
###   HellingerDist,AbscontDistribution,AbscontDistribution-method
###   HellingerDist,AbscontDistribution,DiscreteDistribution-method
###   HellingerDist,DiscreteDistribution,DiscreteDistribution-method
###   HellingerDist,DiscreteDistribution,AbscontDistribution-method
###   HellingerDist,LatticeDistribution,DiscreteDistribution-method
###   HellingerDist,DiscreteDistribution,LatticeDistribution-method
###   HellingerDist,LatticeDistribution,LatticeDistribution-method
###   HellingerDist,numeric,DiscreteDistribution-method
###   HellingerDist,DiscreteDistribution,numeric-method
###   HellingerDist,numeric,AbscontDistribution-method
###   HellingerDist,AbscontDistribution,numeric-method
###   HellingerDist,AcDcLcDistribution,AcDcLcDistribution-method
### Keywords: distribution

### ** Examples

HellingerDist(Norm(), UnivarMixingDistribution(Norm(1,2),Norm(0.5,3),
                 mixCoeff=c(0.2,0.8)))
HellingerDist(Norm(), Td(10))
HellingerDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100)) # mutually singular
HellingerDist(Pois(10), Binom(size = 20)) 

x <- rnorm(100)
HellingerDist(Norm(), x)
HellingerDist(x, Norm(), asis.smooth.discretize = "smooth")

y <- (rbinom(50, size = 20, prob = 0.5)-10)/sqrt(5)
HellingerDist(y, Norm())
HellingerDist(y, Norm(), asis.smooth.discretize = "smooth")

HellingerDist(rbinom(50, size = 20, prob = 0.5), Binom(size = 20, prob = 0.5))



cleanEx()
nameEx("KolmogorovDist")
### * KolmogorovDist

flush(stderr()); flush(stdout())

### Name: KolmogorovDist
### Title: Generic function for the computation of the Kolmogorov distance
###   of two distributions
### Aliases: KolmogorovDist KolmogorovDist-methods
###   KolmogorovDist,AbscontDistribution,AbscontDistribution-method
###   KolmogorovDist,AbscontDistribution,DiscreteDistribution-method
###   KolmogorovDist,DiscreteDistribution,DiscreteDistribution-method
###   KolmogorovDist,DiscreteDistribution,AbscontDistribution-method
###   KolmogorovDist,LatticeDistribution,DiscreteDistribution-method
###   KolmogorovDist,DiscreteDistribution,LatticeDistribution-method
###   KolmogorovDist,LatticeDistribution,LatticeDistribution-method
###   KolmogorovDist,numeric,UnivariateDistribution-method
###   KolmogorovDist,UnivariateDistribution,numeric-method
###   KolmogorovDist,AcDcLcDistribution,AcDcLcDistribution-method
### Keywords: distribution

### ** Examples

KolmogorovDist(Norm(), UnivarMixingDistribution(Norm(1,2),Norm(0.5,3),
                 mixCoeff=c(0.2,0.8)))
KolmogorovDist(Norm(), Td(10))
KolmogorovDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100))
KolmogorovDist(Pois(10), Binom(size = 20)) 
KolmogorovDist(Norm(), rnorm(100))
KolmogorovDist((rbinom(50, size = 20, prob = 0.5)-10)/sqrt(5), Norm())
KolmogorovDist(rbinom(50, size = 20, prob = 0.5), Binom(size = 20, prob = 0.5))



cleanEx()
nameEx("LMCondDistribution")
### * LMCondDistribution

flush(stderr()); flush(stdout())

### Name: LMCondDistribution
### Title: Generating function for the conditional distribution of a linear
###   regression model.
### Aliases: LMCondDistribution
### Keywords: distribution models

### ** Examples

# normal error distribution
(D1 <- LMCondDistribution(theta = 1)) # corresponds to Norm(cond, 1)
plot(D1)
r(D1)
d(D1)
p(D1)
q(D1)
param(D1)
cond(D1)

d(D1)(0, cond = 1)
d(Norm(mean=1))(0)

E(D1, cond = 1)
E(D1, function(x){x^2}, cond = 2)
E(Norm(mean=2), function(x){x^2})



cleanEx()
nameEx("LMParameter-class")
### * LMParameter-class

flush(stderr()); flush(stdout())

### Name: LMParameter-class
### Title: Parameter of a linear regression model
### Aliases: LMParameter-class show,LMParameter-method
### Keywords: distribution

### ** Examples

  new("LMParameter")



cleanEx()
nameEx("LMParameter")
### * LMParameter

flush(stderr()); flush(stdout())

### Name: LMParameter
### Title: Generating function for LMParameter-class
### Aliases: LMParameter
### Keywords: models

### ** Examples

LMParameter(theta = c(1,1), intercept = 2, scale = 0.5)

## The function is currently defined as
function(theta = 0, intercept = 0, scale = 1){
    new("LMParameter", theta = theta, intercept = intercept, scale = 1)
}



cleanEx()
nameEx("MultivariateDistribution-class")
### * MultivariateDistribution-class

flush(stderr()); flush(stdout())

### Name: MultivariateDistribution-class
### Title: Multivariate Distributions
### Aliases: MultivariateDistribution-class
###   show,MultivariateDistribution-method
###   plot,MultivariateDistribution-method
### Keywords: distribution

### ** Examples

# Dirac-measure in (0,0)
new("MultivariateDistribution")



cleanEx()
nameEx("OAsymTotalVarDist")
### * OAsymTotalVarDist

flush(stderr()); flush(stdout())

### Name: OAsymTotalVarDist
### Title: Generic function for the computation of (minimal) asymmetric
###   total variation distance of two distributions
### Aliases: OAsymTotalVarDist OAsymTotalVarDist-methods
###   OAsymTotalVarDist,AbscontDistribution,AbscontDistribution-method
###   OAsymTotalVarDist,AbscontDistribution,DiscreteDistribution-method
###   OAsymTotalVarDist,DiscreteDistribution,DiscreteDistribution-method
###   OAsymTotalVarDist,DiscreteDistribution,AbscontDistribution-method
###   OAsymTotalVarDist,LatticeDistribution,DiscreteDistribution-method
###   OAsymTotalVarDist,DiscreteDistribution,LatticeDistribution-method
###   OAsymTotalVarDist,LatticeDistribution,LatticeDistribution-method
###   OAsymTotalVarDist,numeric,DiscreteDistribution-method
###   OAsymTotalVarDist,DiscreteDistribution,numeric-method
###   OAsymTotalVarDist,numeric,AbscontDistribution-method
###   OAsymTotalVarDist,AbscontDistribution,numeric-method
###   OAsymTotalVarDist,AcDcLcDistribution,AcDcLcDistribution-method
### Keywords: distribution

### ** Examples

OAsymTotalVarDist(Norm(), UnivarMixingDistribution(Norm(1,2),Norm(0.5,3),
                 mixCoeff=c(0.2,0.8)))
OAsymTotalVarDist(Norm(), Td(10))
OAsymTotalVarDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100)) # mutually singular
OAsymTotalVarDist(Pois(10), Binom(size = 20)) 

x <- rnorm(100)
OAsymTotalVarDist(Norm(), x)
OAsymTotalVarDist(x, Norm(), asis.smooth.discretize = "smooth")

y <- (rbinom(50, size = 20, prob = 0.5)-10)/sqrt(5)
OAsymTotalVarDist(y, Norm())
OAsymTotalVarDist(y, Norm(), asis.smooth.discretize = "smooth")

OAsymTotalVarDist(rbinom(50, size = 20, prob = 0.5), Binom(size = 20, prob = 0.5))



cleanEx()
nameEx("PrognCondDistribution-class")
### * PrognCondDistribution-class

flush(stderr()); flush(stdout())

### Name: PrognCondDistribution-class
### Title: Posterior distribution in convolution
### Aliases: PrognCondDistribution-class
### Keywords: distribution

### ** Examples
PrognCondDistribution()


cleanEx()
nameEx("PrognCondDistribution")
### * PrognCondDistribution

flush(stderr()); flush(stdout())

### Name: PrognCondDistribution
### Title: Generating function for PrognCondDistribution-class
### Aliases: PrognCondDistribution
### Keywords: distribution

### ** Examples

PrognCondDistribution(Error = ConvexContamination(Norm(), Norm(4,1), size=0.1))



cleanEx()
nameEx("PrognCondition-class")
### * PrognCondition-class

flush(stderr()); flush(stdout())

### Name: PrognCondition-class
### Title: Conditions of class 'PrognCondition'
### Aliases: PrognCondition-class show,PrognCondition-method PrognCondition
### Keywords: distribution

### ** Examples
PrognCondition()


cleanEx()
nameEx("TotalVarDist")
### * TotalVarDist

flush(stderr()); flush(stdout())

### Name: TotalVarDist
### Title: Generic function for the computation of the total variation
###   distance of two distributions
### Aliases: TotalVarDist TotalVarDist-methods
###   TotalVarDist,AbscontDistribution,AbscontDistribution-method
###   TotalVarDist,AbscontDistribution,DiscreteDistribution-method
###   TotalVarDist,DiscreteDistribution,DiscreteDistribution-method
###   TotalVarDist,DiscreteDistribution,AbscontDistribution-method
###   TotalVarDist,LatticeDistribution,DiscreteDistribution-method
###   TotalVarDist,DiscreteDistribution,LatticeDistribution-method
###   TotalVarDist,LatticeDistribution,LatticeDistribution-method
###   TotalVarDist,numeric,DiscreteDistribution-method
###   TotalVarDist,DiscreteDistribution,numeric-method
###   TotalVarDist,numeric,AbscontDistribution-method
###   TotalVarDist,AbscontDistribution,numeric-method
###   TotalVarDist,AcDcLcDistribution,AcDcLcDistribution-method
### Keywords: distribution

### ** Examples

TotalVarDist(Norm(), UnivarMixingDistribution(Norm(1,2),Norm(0.5,3),
                 mixCoeff=c(0.2,0.8)))
TotalVarDist(Norm(), Td(10))
TotalVarDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100)) # mutually singular
TotalVarDist(Pois(10), Binom(size = 20)) 

x <- rnorm(100)
TotalVarDist(Norm(), x)
TotalVarDist(x, Norm(), asis.smooth.discretize = "smooth")

y <- (rbinom(50, size = 20, prob = 0.5)-10)/sqrt(5)
TotalVarDist(y, Norm())
TotalVarDist(y, Norm(), asis.smooth.discretize = "smooth")

TotalVarDist(rbinom(50, size = 20, prob = 0.5), Binom(size = 20, prob = 0.5))



cleanEx()
nameEx("UnivariateCondDistribution-class")
### * UnivariateCondDistribution-class

flush(stderr()); flush(stdout())

### Name: UnivariateCondDistribution-class
### Title: Univariate conditional distribution
### Aliases: UnivariateCondDistribution-class cond
###   cond,UnivariateCondDistribution-method
###   plot,UnivariateCondDistribution-method
###   show,UnivariateCondDistribution-method
### Keywords: distribution

### ** Examples
new("UnivariateCondDistribution")


cleanEx()
nameEx("Var")
### * Var

flush(stderr()); flush(stdout())

### Name: var
### Title: Generic Functions for the Computation of Functionals
### Aliases: var var-methods var,ANY-method
###   var,UnivariateDistribution-method var,AffLinDistribution-method
###   var,AffLinAbscontDistribution-method
###   var,AffLinDiscreteDistribution-method
###   var,AffLinLatticeDistribution-method var,CompoundDistribution-method
###   var,Arcsine-method var,Beta-method var,Binom-method var,Cauchy-method
###   var,Chisq-method var,Dirac-method var,DExp-method var,Exp-method
###   var,Fd-method var,Gammad-method var,Geom-method var,Hyper-method
###   var,Logis-method var,Lnorm-method var,Nbinom-method var,Norm-method
###   var,Pois-method var,Unif-method var,Weibull-method var,Td-method sd
###   sd-methods sd,UnivariateDistribution-method sd,Norm-method median
###   median,ANY-method median-methods median,UnivariateDistribution-method
###   median,UnivariateCondDistribution-method
###   median,AffLinDistribution-method
###   median,AffLinAbscontDistribution-method
###   median,AffLinDiscreteDistribution-method
###   median,AffLinLatticeDistribution-method median,Arcsine-method
###   median,Cauchy-method median,Dirac-method median,DExp-method
###   median,Exp-method median,Geom-method median,Logis-method
###   median,Lnorm-method median,Norm-method median,Unif-method IQR
###   IQR-methods IQR,ANY-method IQR,UnivariateDistribution-method
###   IQR,UnivariateCondDistribution-method IQR,AffLinDistribution-method
###   IQR,AffLinAbscontDistribution-method
###   IQR,AffLinDiscreteDistribution-method
###   IQR,AffLinLatticeDistribution-method IQR,DiscreteDistribution-method
###   IQR,Arcsine-method IQR,Cauchy-method IQR,Dirac-method IQR,DExp-method
###   IQR,Exp-method IQR,Geom-method IQR,Logis-method IQR,Norm-method
###   IQR,Unif-method mad mad,ANY-method mad-methods
###   mad,UnivariateDistribution-method mad,AffLinDistribution-method
###   mad,AffLinAbscontDistribution-method
###   mad,AffLinDiscreteDistribution-method
###   mad,AffLinLatticeDistribution-method mad,Cauchy-method
###   mad,Dirac-method mad,DExp-method mad,Exp-method mad,Geom-method
###   mad,Logis-method mad,Norm-method mad,Unif-method mad,Arcsine-method
###   skewness skewness-methods skewness,ANY-method
###   skewness,UnivariateDistribution-method
###   skewness,AffLinDistribution-method
###   skewness,AffLinAbscontDistribution-method
###   skewness,AffLinDiscreteDistribution-method
###   skewness,AffLinLatticeDistribution-method skewness,Arcsine-method
###   skewness,Beta-method skewness,Binom-method skewness,Cauchy-method
###   skewness,Chisq-method skewness,Dirac-method skewness,DExp-method
###   skewness,Exp-method skewness,Fd-method skewness,Gammad-method
###   skewness,Geom-method skewness,Hyper-method skewness,Logis-method
###   skewness,Lnorm-method skewness,Nbinom-method skewness,Norm-method
###   skewness,Pois-method skewness,Unif-method skewness,Weibull-method
###   skewness,Td-method kurtosis kurtosis-methods kurtosis,ANY-method
###   kurtosis,UnivariateDistribution-method
###   kurtosis,AffLinDistribution-method
###   kurtosis,AffLinAbscontDistribution-method
###   kurtosis,AffLinDiscreteDistribution-method
###   kurtosis,AffLinLatticeDistribution-method kurtosis,Arcsine-method
###   kurtosis,Beta-method kurtosis,Binom-method kurtosis,Cauchy-method
###   kurtosis,Chisq-method kurtosis,Dirac-method kurtosis,DExp-method
###   kurtosis,Exp-method kurtosis,Fd-method kurtosis,Gammad-method
###   kurtosis,Geom-method kurtosis,Hyper-method kurtosis,Logis-method
###   kurtosis,Lnorm-method kurtosis,Nbinom-method kurtosis,Norm-method
###   kurtosis,Pois-method kurtosis,Unif-method kurtosis,Weibull-method
###   kurtosis,Td-method
### Keywords: methods distribution

### ** Examples

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



cleanEx()
nameEx("distrExIntegrate")
### * distrExIntegrate

flush(stderr()); flush(stdout())

### Name: distrExIntegrate
### Title: Integration of One-Dimensional Functions
### Aliases: distrExIntegrate
### Keywords: math utilities

### ** Examples

fkt <- function(x){x*dchisq(x+1, df = 1)}
integrate(fkt, lower = -1, upper = 3)
GLIntegrate(fkt, lower = -1, upper = 3)
try(integrate(fkt, lower = -1, upper = 5))
distrExIntegrate(fkt, lower = -1, upper = 5)



cleanEx()
nameEx("distrExMASK")
### * distrExMASK

flush(stderr()); flush(stdout())

### Name: distrExMASK
### Title: Masking of/by other functions in package "distrEx"
### Aliases: distrExMASK MASKING
### Keywords: programming distribution documentation

### ** Examples

distrExMASK()



cleanEx()
nameEx("distrExMOVED")
### * distrExMOVED

flush(stderr()); flush(stdout())

### Name: distrExMOVED
### Title: Moved functionality from package "distrEx"
### Aliases: distrExMOVED MOVEDING
### Keywords: programming distribution documentation

### ** Examples

distrExMOVED()



cleanEx()
nameEx("distrExOptions")
### * distrExOptions

flush(stderr()); flush(stdout())

### Name: distrExOptions
### Title: Function to change the global variables of the package 'distrEx'
### Aliases: distrExOptions distrExoptions getdistrExOption MCIterations
###   GLIntegrateTruncQuantile GLIntegrateOrder ElowerTruncQuantile
###   EupperTruncQuantile ErelativeTolerance m1dfLowerTruncQuantile
###   m1dfRelativeTolerance m2dfLowerTruncQuantile m2dfRelativeTolerance
###   nDiscretize hSmooth IQR.fac
### Keywords: misc distribution

### ** Examples

distrExOptions()
distrExOptions("ElowerTruncQuantile")
distrExOptions("ElowerTruncQuantile" = 1e-6)
# or
distrExOptions(ElowerTruncQuantile = 1e-6)
getdistrExOption("ElowerTruncQuantile")



cleanEx()
nameEx("liesInSupport")
### * liesInSupport

flush(stderr()); flush(stdout())

### Name: liesInSupport
### Title: Generic Function for Testing the Support of a Distribution
### Aliases: liesInSupport,DiscreteMVDistribution,numeric-method
###   liesInSupport,DiscreteMVDistribution,matrix-method
### Keywords: distribution utilities methods

### ** Examples

M <- matrix(rpois(30, lambda = 10), ncol = 3)
D1 <- DiscreteMVDistribution(M)
M1 <- rbind(r(D1)(10), matrix(rpois(30, lam = 10), ncol = 3))
liesInSupport(D1, M1)



cleanEx()
nameEx("m1df")
### * m1df

flush(stderr()); flush(stdout())

### Name: m1df
### Title: Generic Function for the Computation of Clipped First Moments
### Aliases: m1df m1df-methods m1df,UnivariateDistribution-method
###   m1df,AbscontDistribution-method m1df,LatticeDistribution-method
###   m1df,AffLinDistribution-method m1df,Binom-method m1df,Pois-method
###   m1df,Norm-method m1df,Exp-method m1df,Chisq-method
### Keywords: distribution methods

### ** Examples

# standard normal distribution
N1 <- Norm()
m1df(N1, 0)

# Poisson distribution
P1 <- Pois(lambda=2)
m1df(P1, 3)
m1df(P1, 3, fun = function(x)sin(x))

# absolutely continuous distribution
D1 <- Norm() + Exp() # convolution
m1df(D1, 2)
m1df(D1, Inf)
E(D1)



cleanEx()
nameEx("m2df")
### * m2df

flush(stderr()); flush(stdout())

### Name: m2df
### Title: Generic function for the computation of clipped second moments
### Aliases: m2df m2df-methods m2df,UnivariateDistribution-method
###   m2df,AbscontDistribution-method m2df,LatticeDistribution-method
###   m2df,AffLinDistribution-method m2df,Binom-method m2df,Pois-method
###   m2df,Norm-method m2df,Exp-method m2df,Chisq-method
### Keywords: methods distribution

### ** Examples

# standard normal distribution
N1 <- Norm()
m2df(N1, 0)

# Poisson distribution
P1 <- Pois(lambda=2)
m2df(P1, 3)
m2df(P1, 3, fun = function(x)sin(x))

# absolutely continuous distribution
D1 <- Norm() + Exp() # convolution
m2df(D1, 2)
m2df(D1, Inf)
E(D1, function(x){x^2})



cleanEx()
nameEx("make01")
### * make01

flush(stderr()); flush(stdout())

### Name: make01
### Title: Centering and Standardization of Univariate Distributions
### Aliases: make01
### Keywords: distribution

### ** Examples

X <- sin(exp(2*log(abs( Norm())))) ## something weird
X01 <- make01(X)
print(X01)
plot(X01)
sd(X01); E(X01)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
