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

        integrand <- function(x, e1, e2){ 0.5*(sqrt(d(e1)(x))-sqrt(d(e2)(x)))^2 }
        res <- distrExIntegrate(integrand, lower = lower, upper = upper, e1 = e1, e2 = e2, 
                        rel.tol=.Machine$double.eps^0.3)
        names(res) <- "Hellinger distance"

        return(res)
    })
setMethod("HellingerDist", signature(e1 = "DiscreteDistribution", 
                                     e2 = "DiscreteDistribution"),
    function(e1, e2){
        supp <- union(support(e1), support(e2))
        res <- 0.5*sum((sqrt(d(e1)(supp))-sqrt(d(e2)(supp)))^2)
        names(res) <- "Hellinger distance"

        return(res)
    })
setMethod("HellingerDist", signature(e1 = "DiscreteDistribution", 
                                     e2 = "AbscontDistribution"),
    function(e1, e2){ 
        res <- 1
        names(res) <- "Hellinger distance"

        return(res) 
    })
setMethod("HellingerDist", signature(e1 = "AbscontDistribution", 
                                     e2 = "DiscreteDistribution"),
    function(e1, e2){ 
        res <- 1
        names(res) <- "Hellinger distance"

        return(res) 
    })
