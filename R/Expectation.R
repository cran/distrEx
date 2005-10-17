## Integration of functions
setMethod("E", signature(object = "UnivariateDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun){
        return(mean(r(object)(MCIterations)))
    })
setMethod("E", signature(object = "AbscontDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun){
        integrand <- function(x, object){ x * d(object)(x) }
        return(distrExIntegrate(f = integrand, lower = q(object)(ElowerTruncQuantile),
                    upper = q(object)(1-EupperTruncQuantile), 
                    rel.tol = ErelativeTolerance, distr = object, object = object))
    })
setMethod("E", signature(object = "DiscreteDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun){
        supp = support(object)
        return(sum(supp * d(object)(supp)))
    })
setMethod("E", signature(object = "MultivariateDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun){
        return(colMeans(r(object)(MCIterations)))
    })
setMethod("E", signature(object = "DiscreteMVDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun){
        supp <- support(object)
        integrand <- function(x, object){ x * d(object)(t(x)) }
        erg <- apply(supp, 1, integrand, object = object)
        if(is.vector(erg))
            return(sum(erg))
        else
            return(rowSums(erg))
    })
setMethod("E", signature(object = "UnivariateDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, ...){
        return(mean(sapply(r(object)(MCIterations), fun, ...)))
    })
setMethod("E", signature(object = "AbscontDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, ...){
        integrand <- function(x, object, fun, ...){ sapply(x, fun, ...) * d(object)(x) }
        return(distrExIntegrate(f = integrand, lower = q(object)(ElowerTruncQuantile), 
                upper = q(object)(1-EupperTruncQuantile), 
                rel.tol = ErelativeTolerance, distr = object, fun = fun, object = object, ...))
    })
setMethod("E", signature(object = "DiscreteDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, ...){
        supp <- support(object)
        integrand <- function(x, object, fun, ...){
            sapply(x, fun, ...) * d(object)(x)
        }
        return(sum(integrand(x = supp, object = object, fun = fun, ...)))
    })
setMethod("E", signature(object = "MultivariateDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, ...){
        x <- r(object)(MCIterations)
        erg <- apply(x, 1, fun, ...)
        if(is.vector(erg))
            return(mean(erg))
        else{
            res <- fun(x[1,], ...)
            res[] <- rowMeans(erg)
            return(res)
        }
    })
setMethod("E", signature(object = "DiscreteMVDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, ...){
        supp <- support(object)
        integrand <- function(x, fun, object, ...){ fun(x, ...) * d(object)(t(x)) }
        erg <- apply(supp, 1, integrand, fun = fun, object = object, ...)
        if(is.vector(erg))
            return(sum(erg))
        else{
            res <- fun(supp[1,], ...)
            res[] <- rowSums(erg)
            return(res)
        }
    })
## Conditional expectation of functions
setMethod("E", signature(object = "UnivariateCondDistribution", 
                         fun = "missing", 
                         cond = "numeric"),
    function(object, cond){
        return(mean(r(object)(MCIterations, cond)))
    })
setMethod("E", signature(object = "AbscontCondDistribution", 
                         fun = "missing", 
                         cond = "numeric"),
    function(object, cond){
        fct <- function(x, object, cond){ x * d(object)(x, cond) }
        integrand <- function(x, object, cond){ 
            return(sapply(x, fct, object = object, cond = cond))
        }
        return(distrExIntegrate(integrand, lower = q(object)(ElowerTruncQuantile, cond), 
                upper = q(object)(1-EupperTruncQuantile, cond), 
                rel.tol = ErelativeTolerance, distr = object, object = object, cond = cond))
    })
setMethod("E", signature(object = "DiscreteCondDistribution", 
                         fun = "missing",
                         cond = "numeric"),
    function(object, cond){
        supp <- support(object)(cond)
        fct <- function(x, object, cond){ x * d(object)(x, cond) }
        return(sum(sapply(x, fct, object = object, cond = cond)))
    })
setMethod("E", signature(object = "UnivariateCondDistribution",
                         fun = "function", 
                         cond = "numeric"),
    function(object, fun, cond, withCond = FALSE, ...){
        if(withCond)
            res <- mean(sapply(r(object)(MCIterations, cond), fun, cond, ...))
        else
            res <- mean(sapply(r(object)(MCIterations, cond), fun, ...))

        return(res)
    })
setMethod("E", signature(object = "AbscontCondDistribution", 
                         fun = "function", 
                         cond = "numeric"),
    function(object, fun, cond, withCond = FALSE, ...){
        if(withCond)
            integrand <- function(x, object, fun, cond, ...){ sapply(x, fun, cond, ...) * d(object)(x, cond) }        
        else
            integrand <- function(x, object, fun, cond, ...){ sapply(x, fun, ...) * d(object)(x, cond) }
        
        return(distrExIntegrate(integrand, lower = q(object)(ElowerTruncQuantile, cond), 
                upper = q(object)(1-EupperTruncQuantile, cond), 
                rel.tol = ErelativeTolerance, distr = object, 
                object = object, fun = fun, cond = cond, ...))
    })
setMethod("E", signature(object = "DiscreteCondDistribution", 
                         fun = "function",
                         cond = "numeric"),
    function(object, fun, cond, withCond = FALSE, ...){
        supp <- support(object)(cond)
        if(withCond){
            fct <- function(x, object, fun, cond, ...){ sapply(x, fun, cond, ...) * d(object)(x, cond) }
        }else{
            fct <- function(x, object, fun, cond, ...){ sapply(x, fun, ...) * d(object)(x, cond) }
        }
        return(sum(fct(x = supp, object = object, fun = fun, cond = cond, ...)))
    })
