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
        integrand <- function(x, dfun){ x * dfun(x) }
        return(distrExIntegrate(f = integrand, lower = q(object)(ElowerTruncQuantile),
                    upper = q(object)(1-EupperTruncQuantile), 
                    rel.tol = ErelativeTolerance, distr = object, dfun = d(object)))
    })
setMethod("E", signature(object = "DiscreteDistribution", 
                         fun = "missing", 
                         cond = "missing"),
    function(object, fun){
        supp <- support(object)
        dfun <- d(object)
        return(sum(supp * dfun(supp)))
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
        integrand <- function(x, dfun){ x * dfun(t(x)) }
        erg <- apply(supp, 1, integrand, dfun = d(object))
        if(is.vector(erg))
            return(sum(erg))
        else
            return(rowSums(erg))
    })
setMethod("E", signature(object = "UnivariateDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, useApply = TRUE, ...){
        if(useApply)        
            return(mean(sapply(r(object)(MCIterations), fun, ...)))
        else
            return(mean(fun(r(object)(MCIterations), ...)))
    })
setMethod("E", signature(object = "AbscontDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, useApply = TRUE, ...){
        if(useApply){
            integrand <- function(x, dfun, fun, ...){ 
                sapply(x, fun, ...) * dfun(x) 
            }
        }else{
            integrand <- function(x, dfun, fun, ...){ 
                fun(x, ...) * dfun(x) 
            }
        }
        return(distrExIntegrate(f = integrand, 
                    lower = q(object)(ElowerTruncQuantile), 
                    upper = q(object)(1-EupperTruncQuantile), 
                    rel.tol = ErelativeTolerance, distr = object, 
                    fun = fun, dfun = d(object), ...))
    })
setMethod("E", signature(object = "DiscreteDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, useApply = TRUE, ...){
        supp <- support(object)
        if(useApply){
            integrand <- function(x, dfun, fun, ...){
                sapply(x, fun, ...) * dfun(x)
            }
        }else{
            integrand <- function(x, dfun, fun, ...){
                fun(x, ...) * dfun(x)
            }
        }
        return(sum(integrand(x = supp, dfun = d(object), fun = fun, ...)))
    })
setMethod("E", signature(object = "MultivariateDistribution", 
                         fun = "function", 
                         cond = "missing"),
    function(object, fun, useApply = TRUE, ...){
        x <- r(object)(MCIterations)
        if(useApply)
            erg <- apply(x, 1, fun, ...)
        else
            erg <- t(fun(x, ...))
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
    function(object, fun, useApply = TRUE, ...){
        supp <- support(object)
        if(useApply){
            integrand <- function(x, fun, dfun, ...){ fun(x, ...) * dfun(t(x)) }
            erg <- apply(supp, 1, integrand, fun = fun, dfun = d(object), ...)
        }else{
            erg <- t(fun(supp, ...) * d(object)(supp))
        }
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
    function(object, cond, useApply = TRUE){
        fct <- function(x, dfun, cond){ x * dfun(x, cond) }
        if(useApply){
            integrand <- function(x, dfun, cond){ 
                return(sapply(x, fct, dfun = dfun, cond = cond))
            }
        }else{
            integrand <- fct
        }
        return(distrExIntegrate(integrand, lower = q(object)(ElowerTruncQuantile, cond), 
                upper = q(object)(1-EupperTruncQuantile, cond), 
                rel.tol = ErelativeTolerance, distr = object, dfun = d(object), cond = cond))
    })
setMethod("E", signature(object = "DiscreteCondDistribution", 
                         fun = "missing",
                         cond = "numeric"),
    function(object, cond, useApply = TRUE){
        supp <- support(object)(cond)
        fct <- function(x, dfun, cond){ x * dfun(x, cond) }
        if(useApply)
            return(sum(sapply(supp, fct, dfun = d(object), cond = cond)))
        else
            return(sum(fct(x = supp, dfun = d(object), cond = cond)))            
    })
setMethod("E", signature(object = "UnivariateCondDistribution",
                         fun = "function", 
                         cond = "numeric"),
    function(object, fun, cond, withCond = FALSE, useApply = TRUE, ...){
        if(withCond){
            if(useApply)
                res <- mean(sapply(r(object)(MCIterations, cond), fun, cond, ...))
            else
                res <- mean(fun(r(object)(MCIterations, cond), ...))
        }else{
            if(useApply)
                res <- mean(sapply(r(object)(MCIterations, cond), fun, ...))
            else
                res <- mean(fun(r(object)(MCIterations, cond), cond, ...))                
        }

        return(res)
    })
setMethod("E", signature(object = "AbscontCondDistribution", 
                         fun = "function", 
                         cond = "numeric"),
    function(object, fun, cond, withCond = FALSE, useApply = TRUE, ...){
        if(withCond)
            if(useApply){
                integrand <- function(x, dfun, fun, cond, ...){ 
                    sapply(x, fun, cond, ...) * dfun(x, cond) 
                }        
            }else{
                integrand <- function(x, dfun, fun, cond, ...){ 
                    fun(x, cond, ...) * dfun(x, cond) 
                }        
            }
        else
            if(useApply){
                integrand <- function(x, dfun, fun, cond, ...){ 
                    sapply(x, fun, ...) * dfun(x, cond) 
                }
            }else{
                integrand <- function(x, dfun, fun, cond, ...){ 
                    fun(x, ...) * dfun(x, cond) 
                }
            }
        
        return(distrExIntegrate(integrand, lower = q(object)(ElowerTruncQuantile, cond), 
                upper = q(object)(1-EupperTruncQuantile, cond), 
                rel.tol = ErelativeTolerance, distr = object, 
                dfun = d(object), fun = fun, cond = cond, ...))
    })
setMethod("E", signature(object = "DiscreteCondDistribution", 
                         fun = "function",
                         cond = "numeric"),
    function(object, fun, cond, withCond = FALSE, useApply = TRUE, ...){
        supp <- support(object)(cond)
        if(withCond){
            if(useApply){
                fct <- function(x, dfun, fun, cond, ...){ 
                    sapply(x, fun, cond, ...) * dfun(x, cond) 
                }
            }else{
                fct <- function(x, dfun, fun, cond, ...){ 
                    fun(x, cond, ...) * dfun(x, cond) 
                }
            }
        }else{
            if(useApply){
                fct <- function(x, dfun, fun, cond, ...){ 
                    sapply(x, fun, ...) * dfun(x, cond) 
                }
            }else{
                fct <- function(x, dfun, fun, cond, ...){ 
                    fun(x, ...) * dfun(x, cond) 
                }
            }
        }
        return(sum(fct(x = supp, dfun = d(object), fun = fun, cond = cond, ...)))
    })
