setMethod("show", "DistrList", 
    function(object){
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        for(i in 1:length(object)){
            cat("[[", i, "]]\n", sep = "")
            print(object[[i]])
        }
    })
setMethod("show", "MultivariateDistribution",
    function(object){
        cat("Distribution object of class: ", class(object)[1], "\n")
        parameter <- param(object)
        Names <- slotNames(parameter)
        if(length(Names) > 1){
          for(i in Names[Names != "name"])
            cat(i, ": ", slot(parameter, i), "\n")
        }
    })
setMethod("show", "EuclCondition",
    function(object){
        cat("name:\t", object@name, "\n")
        cat("Range:\t", object@Range@name, "with dimension ")
        cat(object@Range@dimension, "\n")
    })
setMethod("show", "LMParameter",
    function(object){
        cat("name:\t", object@name, "\n")
        cat("theta:\t", object@theta, "\n")
        cat("intercept:\t", object@intercept, "\n")
        cat("scale:\t", object@scale, "\n")
    })
setMethod("show", "UnivariateCondDistribution",
    function(object){
        cat("Distribution object of class: ", class(object)[1], "\n")
        parameter <- param(object)
        Names <- slotNames(parameter)
        if(length(Names) > 1){
          for(i in Names[Names != "name"])
            cat(i, ": ", slot(parameter, i), "\n")
        }
        cat("## cond:\n")
        show(object@cond)
    })
