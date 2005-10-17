###############################################################################
## Method: TotalVarDist
## total variation distance of two distributions
###############################################################################
setMethod("TotalVarDist", signature(e1 = "AbscontDistribution", 
                                    e2 = "AbscontDistribution"),
    function(e1, e2){
        lower1 <- ifelse(is.finite(q(e1)(0)), q(e1)(0), q(e1)(distr::TruncQuantile))
        lower2 <- ifelse(is.finite(q(e2)(0)), q(e2)(0), q(e2)(distr::TruncQuantile))
        upper1 <- ifelse(is.finite(q(e1)(1)), q(e1)(1), q(e1)(1 - distr::TruncQuantile))
        upper2 <- ifelse(is.finite(q(e2)(1)), q(e2)(1), q(e2)(1 - distr::TruncQuantile))
        lower <- min(lower1, lower2)
        upper <- max(upper1, upper2)

        integrand <- function(x, e1, e2){ 0.5*abs(d(e1)(x)-d(e2)(x)) }
        res <- distrExIntegrate(integrand, lower = lower, upper = upper, e1 = e1, e2 = e2, 
                        rel.tol=.Machine$double.eps^0.3)
        names(res) <- "total variation distance"

        return(res)
    })
setMethod("TotalVarDist", signature(e1 = "DiscreteDistribution",
                                    e2 = "DiscreteDistribution"),
    function(e1, e2){
        supp <- union(support(e1), support(e2))
        res <- 0.5*sum(abs(d(e1)(supp)-d(e2)(supp)))
        names(res) <- "total variation distance"

        return(res)
    })
setMethod("TotalVarDist", signature(e1 = "DiscreteDistribution",
                                    e2 = "AbscontDistribution"),
    function(e1, e2){ 
        res <- 1
        names(res) <- "total variation distance"

        return(res) 
    })
setMethod("TotalVarDist", signature(e1 = "AbscontDistribution",
                                    e2 = "DiscreteDistribution"),
    function(e1, e2){ 
        res <- 1
        names(res) <- "total variation distance"

        return(res) 
    })
