EuclCondition <- function(dimension){
    new("EuclCondition", Range = EuclideanSpace(dimension = dimension))
}

## access methods
setMethod("Range", "EuclCondition", function(object) object@Range)

LMParameter <- function(theta = 0, intercept = 0, scale = 1){
    new("LMParameter", theta = theta, intercept = intercept, scale = scale)
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
                    
    return(new("AbscontCondDistribution", r = rfun, d = dfun, p = pfun, q = qfun, 
            param = param, cond = cond, img = Reals()))
}
