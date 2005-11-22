###############################################################################
## Method: HellingerDist
## Hellinger distance of two distributions
###############################################################################
setMethod("HellingerDist", signature(e1 = "AbscontDistribution", 
                                     e2 = "AbscontDistribution"),
    function(e1, e2){
        lower1 <- ifelse(is.nan(q(e1)(0)), q(e1)(distr::TruncQuantile), q(e1)(0))
        upper1 <- ifelse(is.nan(q(e1)(1)), q(e1)(1-distr::TruncQuantile), q(e1)(1))
        lower2 <- ifelse(is.nan(q(e2)(0)), q(e2)(distr::TruncQuantile), q(e2)(0))
        upper2 <- ifelse(is.nan(q(e2)(1)), q(e2)(1-distr::TruncQuantile), q(e2)(1))
        lower <- min(lower1, lower2)
        upper <- max(upper1, upper2)

        integrand <- function(x, dfun1, dfun2){ 0.5*(sqrt(dfun1(x))-sqrt(dfun2(x)))^2 }
        res <- distrExIntegrate(integrand, lower = lower, upper = upper, 
                    dfun1 = d(e1), dfun2 = d(e2), rel.tol=.Machine$double.eps^0.3)

        return(list(e1 = e1, e2 = e2, Hellinger.distance = res))
    })
setMethod("HellingerDist", signature(e1 = "DiscreteDistribution", 
                                     e2 = "DiscreteDistribution"),
    function(e1, e2){
        supp <- union(support(e1), support(e2))
        res <- 0.5*sum((sqrt(d(e1)(supp))-sqrt(d(e2)(supp)))^2)

        return(list(e1 = e1, e2 = e2, Hellinger.distance = res))
    })
setMethod("HellingerDist", signature(e1 = "DiscreteDistribution", 
                                     e2 = "AbscontDistribution"),
    function(e1, e2){ 
        return(list(e1 = e1, e2 = e2, Hellinger.distance = 1))
    })
setMethod("HellingerDist", signature(e1 = "AbscontDistribution", 
                                     e2 = "DiscreteDistribution"),
    function(e1, e2){ 
        return(list(e1 = e1, e2 = e2, Hellinger.distance = 1))
    })
