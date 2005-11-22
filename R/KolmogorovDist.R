###############################################################################
## Method: KolmogorovDist
## Kolmogorov distance of two distributions
###############################################################################
setMethod("KolmogorovDist", signature(e1 = "AbscontDistribution",
                                      e2 = "AbscontDistribution"),
    function(e1, e2){
        lower1 <- ifelse(is.finite(q(e1)(0)), q(e1)(0), q(e1)(distr::TruncQuantile))
        lower2 <- ifelse(is.finite(q(e2)(0)), q(e2)(0), q(e2)(distr::TruncQuantile))
        upper1 <- ifelse(is.finite(q(e1)(1)), q(e1)(1), q(e1)(1 - distr::TruncQuantile))
        upper2 <- ifelse(is.finite(q(e2)(1)), q(e2)(1), q(e2)(1 - distr::TruncQuantile))
        lower <- min(lower1, lower2)
        upper <- max(upper1, upper2)

        x1 <- union(r(e1)(1e5), r(e2)(1e5))
        x2 <- seq(from=lower, to=upper, length=1e5)
        x <- union(x1, x2)
        
        res <- max(abs(p(e1)(x)-p(e2)(x)))

        return(list(e1 = e1, e2 = e2, Kolmogorov.distance = res))
    })
setMethod("KolmogorovDist", signature(e1 = "DiscreteDistribution",
                                      e2 = "DiscreteDistribution"),
    function(e1, e2){
        supp <- union(support(e1), support(e2))
        res <- max(abs(p(e1)(supp)-p(e2)(supp)))

        return(list(e1 = e1, e2 = e2, Kolmogorov.distance = res))
    })
setMethod("KolmogorovDist", signature(e1 = "DiscreteDistribution",
                                      e2 = "AbscontDistribution"),
    function(e1, e2){
        lower <- ifelse(is.finite(q(e2)(0)), q(e2)(0), q(e2)(distr::TruncQuantile))
        upper <- ifelse(is.finite(q(e2)(1)), q(e2)(1), q(e2)(1 - distr::TruncQuantile))
    
        x1 <- union(support(e1), r(e2)(1e5))
        x2 <- seq(from=lower, to=upper, length=1e5)
        x <- union(x1, x2)
        res <- max(abs(p(e1)(x)-p(e2)(x)))

        return(list(e1 = e1, e2 = e2, Kolmogorov.distance = res))
    })
setMethod("KolmogorovDist", signature(e1 = "AbscontDistribution",
                                      e2 = "DiscreteDistribution"),
    function(e1, e2){
        KolmogorovDist(e2, e1)
    })
