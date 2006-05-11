# plot
setMethod("plot", "MultivariateDistribution", 
    function(x,y=NULL,...){ 
        warning("'plot' not yet implemented for objects",
                " of class ", class(x))
    })

setMethod("plot", "UnivariateCondDistribution", 
    function(x,y=NULL,...){ 
        warning("'plot' not yet implemented for objects",
                " of class ", class(x))
    })


# plot
setMethod("plot", "DistrList", 
    function(x,y=NULL,...){ 
        for(i in 1:length(x)){
            get(getOption("device"))()
            plot(x[[i]],...)
        }
    })
