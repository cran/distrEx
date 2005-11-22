###############################################################################
## Generating functions for "distr"
###############################################################################
EuclideanSpace <- function(dimension = 1){ 
    new("EuclideanSpace", dimension = dimension)
}
Reals <- function(){ new("Reals") }
Naturals <- function(){ new("Naturals") }

DiscreteDistribution <- function(supp, prob){
    if(!is.numeric(supp)) 
        stop("'supp' is no numeric vector")
    if(any(!is.finite(supp)))   # admit +/- Inf?
        stop("inifinite or missing values in supp")
    len <- length(supp)
    if(missing(prob)){
        prob <- rep(1/len, len)
    }else{
        if(len != length(prob))
            stop("'supp' and 'prob' must have equal lengths")
        if(any(!is.finite(prob)))
            stop("inifinite or missing values in prob")
        if(!identical(all.equal(sum(prob), 1, 
                            tolerance = 2*distr::TruncQuantile), TRUE))
            stop("sum of 'prob' has to be (approximately) 1")
        if(!all(prob >= 0))
            stop("'prob' contains values < 0")
    }
    if(length(usupp <- unique(supp)) < len){
        warning("collapsing to unique support values")
        prob <- as.vector(tapply(prob, supp, sum))
        supp <- sort(usupp)
        len <- length(supp)
    }else{
        o <- order(supp)
        supp <- supp[o]
        prob <- prob[o]
    }
    
    if(len > 1){
      if(min(supp[2:len] - supp[1:(len - 1)]) < distr::DistrResolution)
        stop("grid too narrow --> change DistrResolution")
    }
    rfun <- function(n){ 
        sample(x = supp, size = n, replace = TRUE, prob = prob) 
    }
    intervall <- distr::DistrResolution / 2  
  
    supp.grid <- as.numeric(matrix(
                      rbind(supp - intervall, supp + intervall), nrow = 1))
    prob.grid <- c(as.numeric(matrix(rbind(0, prob), nrow = 1)), 0)
    dfun <- function(x){ stepfun(x = supp.grid, y = prob.grid)(x) }
  
    cumprob <- cumsum(prob)
    pfun <- function(x){ stepfun(x = supp, y = c(0, cumprob))(x) }

    qfun <- function(x){ supp[sum(cumprob<x)+1] }

    new("DiscreteDistribution", r = rfun, d = dfun, q = qfun, support = supp)
}
