###############################################################################
## Convex Contamination
## (1-size)*e1 + size*e2, size~Binom(1, size)
## e1: ideal distribution
## e2: contaminating distribution
## size: amout of contamination (gross errors)
###############################################################################
setMethod("ConvexContamination", signature(e1 = "AbscontDistribution",
                                           e2 = "AbscontDistribution",
                                           size = "numeric"),
    function(e1, e2, size){
        if(length(size) != 1)
            stop("length of 'size' has to be 1")
        if((size < 0)|(size > 1))
            stop("'size' has to be in [0,1]")
        rfun <- function(n){ 
            r1 <- r1fun; r2 <- r2fun
            ind <- rbinom(n, prob=size, size=1)
            (1-ind)*r1(n) + ind*r2(n)
        }
        body(rfun) <- substitute({ r1 <- r1fun; r2 <- r2fun
                                   ind <- rbinom(n, prob=size, size=1)
                                   (1-ind)*r1(n) + ind*r2(n)},
                            list(size = size, r1fun = r(e1), r2fun = r(e2)))

        dfun <- function(x){ 
            d1 <- d1fun; d2 <- d2fun
            (1-size)*d1(x) + size*d2(x)
        }
        body(dfun) <- substitute({ d1 <- d1fun; d2 <- d2fun
                                   (1-size)*d1(x) + size*d2(x)},
                            list(size = size, d1fun = d(e1), d2fun = d(e2)))

        pfun <- function(x){ 
            p1 <- p1fun; p2 <- p2fun
            (1-size)*p1(x) + size*p2(x)
        }
        body(pfun) <- substitute({ p1 <- p1fun; p2 <- p2fun
                                   (1-size)*p1(x) + size*p2(x)},
                            list(size = size, p1fun = p(e1), p2fun = p(e2)))

        m1 <- min(q(e1)(distr::TruncQuantile), q(e2)(distr::TruncQuantile))
        m2 <- max(q(e1)(1-distr::TruncQuantile), q(e2)(1-distr::TruncQuantile))
        qfun <- function(x){
            pfunx <- seq(from = m1, to = m2, length = 1e5)
            p <- pfun; pfuny <- pfun(pfunx)
            qfun1 <- approxfun(x = pfuny, y = pfunx, rule = 2)
            y <- ifelse(x > 1, NA, ifelse(x < 0, NA, qfun1(x)))
            return(y)
        }
        body(qfun) <- substitute({ pfunx <- seq(from = m1, to = m2, length = 1e5)
                                   p <- pfun; pfuny <- p(pfunx)
                                   qfun1 <- approxfun(x = pfuny, y = pfunx, rule = 2)
                                   y <- ifelse(x > 1, NA, ifelse(x < 0, NA, qfun1(x)))
                                   return(y)},
                            list(m1 = m1, m2 = m2, pfun = pfun)) 
    
        return(new("AbscontDistribution", r = rfun, d = dfun, p = pfun, q = qfun))
    })
setMethod("ConvexContamination", signature(e1 = "DiscreteDistribution",
                                           e2 = "DiscreteDistribution",
                                           size = "numeric"),
    function(e1, e2, size){
        if(length(size) != 1)
            stop("length of 'size' has to be 1")
        if((size < 0)|(size > 1))
            stop("'size' has to be in [0,1]")
        supp <- union(support(e1), support(e2))
        len <- length(supp)
        if(length(usupp <- unique(supp)) < len){
            supp <- sort(usupp)
            len <- length(supp)
        }else{
            o <- order(supp)
            supp <- supp[o]
        }

        rfun <- function(n){ 
            r1 <- r1fun; r2 <- r2fun
            ind <- rbinom(n, prob=size, size=1)
            (1-ind)*r1(n) + ind*r2(n)
        }
        body(rfun) <- substitute({ r1 <- r1fun; r2 <- r2fun
                                   ind <- rbinom(n, prob=size, size=1)
                                   (1-ind)*r1(n) + ind*r2(n)},
                            list(size = size, r1fun = r(e1), r2fun = r(e2)))

        dfun <- function(x){ 
            d1 <- d1fun; d2 <- d2fun
            (1-size)*d1(x) + size*d2(x)
        }
        body(dfun) <- substitute({ d1 <- d1fun; d2 <- d2fun
                                   (1-size)*d1(x) + size*d2(x)},
                            list(size = size, d1fun = d(e1), d2fun = d(e2)))

        pfun <- function(x){ 
            p1 <- p1fun; p2 <- p2fun
            (1-size)*p1(x) + size*p2(x)
        }
        body(pfun) <- substitute({ p1 <- p1fun; p2 <- p2fun
                                   (1-size)*p1(x) + size*p2(x)},
                            list(size = size, p1fun = p(e1), p2fun = p(e2)))

        cumprob <- pfun(supp)
        qfun <- function(x){ supp[sum(cumprob<x)+1] }        
    
        return(new("DiscreteDistribution", r = rfun, d = dfun, p = pfun, q = qfun, 
                                           support = supp))
    })

setMethod("ConvexContamination", signature(e1 = "UnivariateDistribution",
                                           e2 = "UnivariateDistribution",
                                           size = "numeric"),
    function(e1, e2, size){
        if(length(size) != 1)
            stop("length of 'size' has to be 1")
        if((size < 0)|(size > 1))
            stop("'size' has to be in [0,1]")
        rfun <- function(n){ 
            r1 <- r1fun; r2 <- r2fun
            ind <- rbinom(n, prob=size, size=1)
            (1-ind)*r1(n) + ind*r2(n)
        }
        body(rfun) <- substitute({ r1 <- r1fun; r2 <- r2fun
                                   ind <- rbinom(n, prob=size, size=1)
                                   (1-ind)*r1(n) + ind*r2(n)},
                            list(size = size, r1fun = r(e1), r2fun = r(e2)))

        pfun <- function(x){ 
            p1 <- p1fun; p2 <- p2fun
            (1-size)*p1(x) + size*p2(x)
        }
        body(pfun) <- substitute({ p1 <- p1fun; p2 <- p2fun
                                   (1-size)*p1(x) + size*p2(x)},
                            list(size = size, p1fun = p(e1), p2fun = p(e2)))

        m1 <- min(q(e1)(distr::TruncQuantile), q(e2)(distr::TruncQuantile))
        m2 <- max(q(e1)(1-distr::TruncQuantile), q(e2)(1-distr::TruncQuantile))
        qfun <- function(x){
            pfunx <- seq(from = m1, to = m2, length = 1e5)
            p <- pfun; pfuny <- pfun(pfunx)
            qfun1 <- approxfun(x = pfuny, y = pfunx, rule = 2)
            y <- ifelse(x > 1, NA, ifelse(x < 0, NA, qfun1(x)))
            return(y)
        }
        body(qfun) <- substitute({ pfunx <- seq(from = m1, to = m2, length = 1e5)
                                   p <- pfun; pfuny <- p(pfunx)
                                   qfun1 <- approxfun(x = pfuny, y = pfunx, rule = 2)
                                   y <- ifelse(x > 1, NA, ifelse(x < 0, NA, qfun1(x)))
                                   return(y)},
                            list(m1 = m1, m2 = m2, pfun = pfun)) 
    
        return(new("UnivariateDistribution", img = img(e1), r = rfun, d = NULL, p = pfun, q = qfun))
    })
