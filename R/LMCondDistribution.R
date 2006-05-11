EuclCondition <- function(dimension){
    new("EuclCondition", Range = EuclideanSpace(dimension = dimension))
}

## access methods
setMethod("Range", "EuclCondition", function(object) object@Range)

LMParameter <- function(theta = 0, intercept = 0, scale = 1){
    if(any(!is.finite(theta)))
        stop("inifinite or missing values in 'theta'")
    if(length(intercept) != 1)
        stop("'intercept' has to be of length 1")
    if(!is.finite(intercept))
        stop("inifinite or missing value in 'intercept'")
    if(length(scale) != 1)
        stop("'scale' has to be of length 1")
    if(!is.finite(scale))
        stop("inifinite or missing value in 'scale'")

    LMP <- new("LMParameter")
    LMP@theta <- theta
    LMP@intercept <- intercept
    LMP@scale <- scale
    
    return(LMP)
}

## generating function
LMCondDistribution <- function(Error = Norm(), theta = 0, intercept = 0, 
                                scale = 1){
    if(!is(Error, "AbscontDistribution"))
        stop("distribution of 'Error' has to be of class 'AbscontDistribution'")
    param <- LMParameter(theta = theta, intercept = intercept, scale = scale)
    lth <- length(theta)
    cond <- EuclCondition(dimension = floor(lth))
    rfun <- function(n, cond){ 
        if(length(cond) != lth) 
            stop("'cond' has wrong dimension")
        r <- rfct
        intercept + cond %*% theta + scale*r(n) 
    }
    body(rfun) <- substitute({ if(length(cond) != lth) 
                                    stop("'cond' has wrong dimension")
                               r <- rfct
                               intercept + cond %*% theta + scale*r(n) },
                        list(rfct = r(Error), lth = lth, 
                             intercept = intercept, theta = theta, 
                             scale = scale))
                        
    dfun <- function(x, cond){ 
        if(length(cond) != lth) 
            stop("'cond' has wrong dimension")
        d <- dfct
        d((x - intercept - as.vector(cond %*% theta))/scale)/scale 
    }
    body(dfun) <- substitute({ if(length(cond) != lth) 
                                    stop("'cond' has wrong dimension")
                               d <- dfct
                               d((x - intercept - as.vector(cond %*% theta))/scale)/scale },
                        list(dfct = d(Error), lth = lth, 
                             intercept = intercept, theta = theta, 
                             scale = scale))
 
    pfun <- function(x, cond){ 
        if(length(cond) != lth) 
            stop("'cond' has wrong dimension")
        p <- pfct
        p((x - intercept - as.vector(cond %*% theta))/scale) 
    }
    body(pfun) <- substitute({ if(length(cond) != lth) 
                                    stop("'cond' has wrong dimension")
                               p <- pfct
                               p((x - intercept - as.vector(cond %*% theta))/scale) },
                        list(pfct = p(Error), lth = lth, 
                             intercept = intercept, theta = theta, 
                             scale = scale))

    qfun <- function(x, cond){ 
        if(length(cond) != lth) 
            stop("'cond' has wrong dimension")
        q <- qfct
        scale*q(x) + intercept + as.vector(cond %*% theta)
    }
    body(qfun) <- substitute({ if(length(cond) != lth) 
                                    stop("'cond' has wrong dimension")
                               q <- qfct
                               scale*q(x) + intercept + as.vector(cond %*% theta) }, 
                        list(qfct = q(Error), lth = lth, 
                             intercept = intercept, theta = theta, 
                             scale = scale))
    CD1 <- new("AbscontCondDistribution")
    CD1@r <- rfun 
    CD1@d <- dfun
    CD1@p <- pfun
    CD1@q <- qfun
    CD1@param <- param
    CD1@cond <- cond
    CD1@img <- Reals()
    CD1@.withSim <- FALSE
    CD1@.withArith <- FALSE
    return(CD1)
}
