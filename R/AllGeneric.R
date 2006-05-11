
if(!isGeneric("cond")){
    setGeneric("cond", function(object) standardGeneric("cond"))
}
if(!isGeneric("Range")){ 
    setGeneric("Range", function(object) standardGeneric("Range"))
}
if(!isGeneric("ContaminationSize")){ 
    setGeneric("ContaminationSize", function(e1, e2) standardGeneric("ContaminationSize"))
}
if(!isGeneric("TotalVarDist")){
    setGeneric("TotalVarDist", function(e1, e2) standardGeneric("TotalVarDist"))
}
if(!isGeneric("KolmogorovDist")){
    setGeneric("KolmogorovDist", function(e1, e2) standardGeneric("KolmogorovDist"))
}
if(!isGeneric("HellingerDist")){
    setGeneric("HellingerDist", function(e1, e2) standardGeneric("HellingerDist"))
}
if(!isGeneric("ConvexContamination")){ 
    setGeneric("ConvexContamination", function(e1, e2, size) standardGeneric("ConvexContamination"))
}
if(!isGeneric("loc")){
    setGeneric("loc", function(object) standardGeneric("loc"))
}
if(!isGeneric("loc<-")){ 
    setGeneric("loc<-", function(object, value) standardGeneric("loc<-"))
}
if(!isGeneric("E")){ 
    setGeneric("E", function(object, fun, cond, ...) standardGeneric("E"))
}
if(!isGeneric("m1df")){
    setGeneric("m1df", function(object, upper) standardGeneric("m1df"))
}
if(!isGeneric("m2df")){
    setGeneric("m2df", function(object, upper) standardGeneric("m2df"))
}
if(!isGeneric("liesInSupport")){
    setGeneric("liesInSupport", function(object, x) standardGeneric("liesInSupport"))
}

## to be included in the next release of distrEx (v1.7)

### intentionally mask functionals in order to have an additional ... argument P.R. 28-03-06
var <- function(x , ...)
       {dots <- list(...)
        if(hasArg(y)) y <- dots$"y"
        na.rm <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE)
        if(!hasArg(use)) 
             use <- ifelse (na.rm, "complete.obs","all.obs")
        else use <- dots$"use"
        if(hasArg(y))       
           stats::var(x = x, y = y, na.rm = na.rm, use)
        else
           stats::var(x = x, y = NULL, na.rm = na.rm, use)
        }   

## sd already masked in NormalDistribution.R in package "distr"

median <- function(x , ...)
       {dots <- list(...)
        na.rm <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE)
        stats::median(x = x, na.rm = na.rm)}

IQR <- function(x , ...)
       {dots <- list(...)
        na.rm <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE)
        stats::IQR(x = x, na.rm = na.rm)}

mad <- function(x , ...)
       {dots <- list(...)
        na.rm     <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE)
        low       <-  ifelse(hasArg(low), dots$"low", FALSE)
        high      <-  ifelse(hasArg(high), dots$"high", FALSE)
        center    <-  ifelse(hasArg(center), dots$"center", median(x))
        constant  <-  ifelse(hasArg(constant), dots$"constant", 1.4826)
        stats::mad(x = x, center = center, constant = constant , na.rm = na.rm, low = low, high = high)}

####################################################################################
#registration of generics
####################################################################################
if(!isGeneric("var")){ 
    setGeneric("var", function(x, ...) standardGeneric("var"))
}

##sd already registered as generic in package "distr"

if(!isGeneric("median")){ 
    setGeneric("median", function(x, ...) standardGeneric("median"))
}

if(!isGeneric("IQR")){ 
    setGeneric("IQR", function(x, ...) standardGeneric("IQR"))
}

if(!isGeneric("mad")){ 
    setGeneric("mad", function(x, ...) standardGeneric("mad"))
}

## resetting original functions as Methods:
setMethod("var", "ANY", function(x , ...)
       {dots <- list(...)
        if(hasArg(y)) y <- dots$"y"
        na.rm <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE)
        if(!hasArg(use)) 
             use <- ifelse (na.rm, "complete.obs","all.obs")
        else use <- dots$"use"
        if(hasArg(y))       
           stats::var(x = x, y = y, na.rm = na.rm, use)
        else
           stats::var(x = x, y = NULL, na.rm = na.rm, use)
        })
setMethod("median","ANY",function(x , ...)
       {dots <- list(...)
        na.rm <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE)
        stats::median(x = x, na.rm = na.rm)}
)
setMethod("IQR","ANY",function(x , ...)
       {dots <- list(...)
        na.rm <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE)
        stats::IQR(x = x, na.rm = na.rm)}
)
setMethod("mad","ANY",function(x , ...)
       {dots <- list(...)
        na.rm     <- ifelse(hasArg(na.rm), dots$"na.rm", FALSE)
        low       <-  ifelse(hasArg(low), dots$"low", FALSE)
        high      <-  ifelse(hasArg(high), dots$"high", FALSE)
        center    <-  ifelse(hasArg(center), dots$"center", median(x))
        constant  <-  ifelse(hasArg(constant), dots$"constant", 1.4826)
        stats::mad(x = x, center = center, constant = constant , na.rm = na.rm, low = low, high = high)})
