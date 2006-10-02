####################################################################################
# some worked out functionals
####################################################################################

###################################################################################
#Var
###################################################################################
setMethod("var", signature(x = "UnivariateDistribution"),
    function(x, fun = function(t) {t}, cond, withCond = FALSE, useApply = TRUE, ...){
        f2 <- function(t) {fun(t)^2}
        if(missing(cond))
            {
            m <- E(x, fun = fun, useApply = useApply, ...) 
            m2 <- E(x, fun = f2, useApply = useApply, ...)
            }
        else{
            m <- E(x, cond = cond, fun = fun, withCond  = withCond, useApply = useApply, ...) 
            m2 <- E(x, cond = cond, fun = f2, withCond  = withCond, useApply = useApply, ...)
            }
        return(m2-m^2)
    })


###################################################################################
#sd
###################################################################################
setMethod("sd", signature(x = "UnivariateDistribution"), 
    function(x, fun, cond, withCond = FALSE, useApply = TRUE, ...){
      if(missing(fun))
        {if(missing(cond))
           return(sqrt(var(x, useApply = TRUE, ...)))
        else
           return(sqrt(var(x, cond = cond, withCond = FALSE, useApply = TRUE, ...)))}
      else
        {if(missing(cond))
           return(sqrt(var(x, fun = fun, useApply = TRUE, ...)))
        else
           return(sqrt(var(x, fun = fun, cond = cond, withCond = FALSE, useApply = TRUE,...)))}           
    })

### overload "sd" method for "Norm" ...
setMethod("sd", signature(x = "Norm"), 
    function(x, fun, cond, withCond = FALSE, useApply = TRUE, ...){
      if(missing(fun))
        {if(missing(cond))
           return(sd(param(x)))
        else
           return(sqrt(var(x, cond = cond, withCond = FALSE, useApply = TRUE, ...)))}
      else
        {if(missing(cond))
           return(sqrt(var(x, fun = fun, useApply = TRUE, ...)))
        else
           return(sqrt(var(x, fun = fun, cond = cond, withCond = FALSE, useApply = TRUE,...)))}           
    }) 
    


###################################################################################
#median, mad, IQR
###################################################################################
setMethod("median", signature(x = "UnivariateDistribution"),
    function(x){
        return(q(x)(1/2))
    })

setMethod("mad", signature(x = "UnivariateDistribution"),
    function(x){
        m <- median(x)
        y <- abs(x-m) 
        return(q(y)(1/2))
    })

setMethod("IQR", signature(x = "UnivariateDistribution"),
    function(x){
        return(q(x)(3/4)-q(x)(1/4))
    })

##standardization
make01 <- function(x){
    if (!is(x, "UnivariateDistribution"))
        stop("This function is for distribution objects.")
    return((x-E(x))/sd(x))
    }


###
# some exact variances:
###
setMethod("var", signature(x = "Norm"),
    function(x,...){ 
    if((hasArg(fun))||(hasArg(cond)))
       return(var(as(x,"AbscontDistribution"),...))
    else
        return(sd(x)^2)
    })

setMethod("var", signature(x = "Binom"),
    function(x,...){
    if((hasArg(fun))||(hasArg(cond)))
        return(var(as(x,"DiscreteDistribution"),...))
    else
        return(size(x)*prob(x)*(1-prob(x)))
    })

setMethod("var", signature(x = "Cauchy"),
    function(x,...){    
    if((hasArg(fun))||(hasArg(cond)))
      return(var(as(x,"AbscontDistribution"),...))
    else
        return(NA)
    })

setMethod("var", signature(x = "Chisq"),
    function(x,...){    
    if((hasArg(fun))||(hasArg(cond)))
       return(var(as(x,"AbscontDistribution"),...))
    else
        return(2*(df(x)+2*ncp(x)))
    })

setMethod("var", signature(x = "Dirac"),
    function(x){return(0)})


setMethod("var", signature(x = "DExp"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(var(as(x,"AbscontDistribution"),...))
    else
        return(2/rate(x)^2)
    })

setMethod("var", signature(x = "Exp"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(var(as(x,"AbscontDistribution"),...))
    else
        return(1/rate(x)^2)
    })


setMethod("var", signature(x = "Fd"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
         return(var(as(x,"AbscontDistribution"),...))
    else
        {df1 <- df1(x)
         df2 <- df2(x)
         d <- ncp(x)
         Ex2 <- (E(x))^2 
         Exx <- df2^2/(df2-2)/(df2-4)*((df1+d)^2+2*df1+4*d)/df1^2
        return(ifelse(df2>4,Exx-Ex2, NA ))}
    })

setMethod("var", signature(x = "Gammad"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(var(as(x,"AbscontDistribution"),...))
    else
        return(shape(x)*scale(x)^2)
    })

setMethod("var", signature(x = "Geom"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(var(as(x,"DiscreteDistribution"),...))
    else
        return(prob(x)/(1-prob(x))^2)
    })

setMethod("var", signature(x = "Hyper"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(var(as(x,"DiscreteDistribution"),...))
    else
       {k <- k(x);
        m <- m(x); 
        n <- n(x);
        return(k*n/(m+n)*m/(m+n)*(m+n-k)/(m+n-1))}
    })

setMethod("var", signature(x = "Logis"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(var(as(x,"AbscontDistribution"),...))
    else
        return(pi^2/3*scale(x)^2)
    })

setMethod("var", signature(x = "Lnorm"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(var(as(x,"AbscontDistribution"),...))
    else
        return(exp(2*meanlog(x)+sdlog(x)^2)*(exp(sdlog(x)^2)-1))
    })

setMethod("var", signature(x = "Nbinom"),
    function(x, ...){    
    if((hasArg(fun))||(hasArg(cond))) 
         return(var(as(x,"DiscreteDistribution"),...))
    else
        return(size(x)*prob(x)/(1-prob(x))^2)
    })

setMethod("var", signature(x = "Pois"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(var(as(x,"DiscreteDistribution"),...))
    else
        return(lambda(x))
    })

setMethod("var", signature(x = "Td"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(var(as(x,"AbscontDistribution"),...))
    else
        {n <- df(x); d<- ncp(x)
         return(n/(n-2)+d^2*(n/(n-2)-n/2*exp(lgamma((n-1)/2)-lgamma(n/2))^2))
        }
    })


setMethod("var", signature(x = "Unif"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(var(as(x,"AbscontDistribution"),...))
    else
        return((Max(x)-Min(x))^2/12)
    })

setMethod("var", signature(x = "Weibull"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))) 
        return(var(as(x,"AbscontDistribution"),...))
    else
        return(scale(x)^2*(gamma(1+2/shape(x))- (gamma(1 + 1/shape(x)))^2))
    })
    
setMethod("var", signature(x = "Beta"),
    function(x, ...){
    if((hasArg(fun))||(hasArg(cond))||(!identical(all.equal(ncp(x),0), TRUE))) 
        return(var(as(x,"AbscontDistribution"),...))
    else
        {a<-shape1(x); b<- shape2(x)
        return(a*b/(a+b)^2/(a+b+1))}
    })
