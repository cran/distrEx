###############################################################################
## Class: PrognCondition
## condition in case of the simple prognose model 
## y = x + u
## i.e., conditioning by realizations of y
###############################################################################
setClass("PrognCondition", 
            representation(range = "EuclideanSpace"), 
            prototype(name = "condition in case of a linear regression model",
                      range = new("EuclideanSpace")),
            contains = "Condition")
PrognCondition <- function(range = EuclideanSpace()){
    new("PrognCondition", range = range)
}

setMethod("show", "PrognCondition",
    function(object){
        cat(gettextf("name:\t%s\n", object@name))
        cat("range:\t%s with dimension %s\n", object@range@name, object@range@dimension)
    })

## generating function
PrognCondDistribution <- function(Regr = Norm(), Error = Norm()){
    TruncQuantile <- getdistrOption("TruncQuantile")  
    if(!is(Error, "AbscontDistribution"))
        stop("Error has to be of type 'AbscontDistribution'")
    if(!is(Regr, "AbscontDistribution"))
        stop("Regr has to be of type 'AbscontDistribution'")
    param <- NULL
    cond <- PrognCondition(range = new("Reals"))
    rfun <- function(n, cond){ r <- rfun; cond - r(n)}
    body(rfun) <- substitute({ r <- rfun; cond - r(n)}, list(rfun = r(Error)))
                        
    dfun <- function(x, cond){ 
        dx <- dxfun; du <- dufun; qx <- qxfun
        dy <- function(cond){ 
            return(integrate(function(x, cond){ dx <- dxfun; du <- dufun; dx(x)*du(cond-x) }, 
                lower = qx(eps), upper = qx(1-eps), cond = cond)$value)
        }
        dx(x)*du(cond-x)/dy(cond)
    }
    body(dfun) <- substitute({ dx <- dxfun; du <- dufun; qx <- qxfun
                               dy <- function(cond){ 
                                   return(integrate(function(x, cond){ 
                                        dx <- dxfun; du <- dufun; dx(x)*du(cond-x) }, 
                                        lower = qx(eps), upper = qx(1-eps), 
                                        cond = cond)$value) }
                               dx(x)*du(cond-x)/dy(cond)},
                        list(dxfun = d(Regr), dufun = d(Error), qxfun = q(Regr), eps = TruncQuantile))
 
    pfun <- function(x, cond){ 
        d <- dfun; qx <- qxfun
        return(integrate(d, lower = qx(eps), upper = x, cond = cond)$value)
    }
    body(pfun) <- substitute({ d <- dfun; qx <- qxfun
                               return(integrate(d, lower = qx(eps), upper = x, cond = cond)$value)},
                        list(dfun = dfun, qxfun = q(Regr), eps = TruncQuantile))

    qfun <- function(x, cond){ qu <- qufun; cond - qu(1-x) }
    body(qfun) <- substitute({ qu <- qufun; cond - qu(1-x) },
                        list(qufun = q(Error)))
    
    return(new("AbscontCondDistribution", r = rfun, d = dfun, p = pfun, q = qfun, 
            param = param, cond = cond))
}
