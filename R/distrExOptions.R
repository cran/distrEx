GLIntegrateTruncQuantile <- 10*.Machine$double.eps
GLIntegrateOrder <- 500
MCIterations <- 1e5
ElowerTruncQuantile <- 0
EupperTruncQuantile <- 0
ErelativeTolerance <- .Machine$double.eps^0.25
m1dfLowerTruncQuantile <- 0
m1dfRelativeTolerance <- .Machine$double.eps^0.25
m2dfLowerTruncQuantile <- 0
m2dfRelativeTolerance <- .Machine$double.eps^0.25

distrExOptions <- function(arg = "missing", value = -1){
    globals <- list(GLIntegrateTruncQuantile = GLIntegrateTruncQuantile,
                    GLIntegrateOrder = GLIntegrateOrder,
                    MCIterations = MCIterations,
                    ElowerTruncQuantile = ElowerTruncQuantile,
                    EupperTruncQuantile = EupperTruncQuantile,
                    ErelativeTolerance = ErelativeTolerance,
                    m1dfLowerTruncQuantile = m1dfLowerTruncQuantile,
                    m1dfRelativeTolerance = m1dfRelativeTolerance,
                    m2dfLowerTruncQuantile = m2dfLowerTruncQuantile,
                    m2dfRelativeTolerance = m2dfRelativeTolerance)
    if(arg == "missing"){
        print(globals)
        return(invisible())
    }
    if(!is.character(arg)) 
        arg <- as.character(substitute(arg))
    if(!any(arg == names(globals)))
        stop(paste("No such variable:", arg))
    if(value == -1)
        switch(arg,
               GLIntegrateTruncQuantile = GLIntegrateTruncQuantile,
               GLIntegrateOrder = GLIntegrateOrder,
               MCIterations = MCIterations,
               ElowerTruncQuantile = ElowerTruncQuantile,
               EupperTruncQuantile = EupperTruncQuantile,
               ErelativeTolerance = ErelativeTolerance,
               m1dfLowerTruncQuantile = m1dfLowerTruncQuantile,
               m1dfRelativeTolerance = m1dfrelativeTolerance,
               m2dfLowerTruncQuantile = m2dfLowerTruncQuantile,
               m2dfRelativeTolerance = m2dfrelativeTolerance)
    else
        eval.parent(parse(text = paste("assignInNamespace(\"", arg, "\",", value, ", \"distrEx\")", sep = "")))
}
