.onLoad <- function(lib, pkg){
    require("methods", character = TRUE, quietly = TRUE) 
}

# list of distributions
setClass(Class = "DistrList", 
            prototype = prototype(list(new("Norm"))), 
            contains = "list",
            validity = function(object){
                nrvalues <- length(object)
                for(i in 1:nrvalues)
                    if(!is(object[[i]], "Distribution")) 
                        stop("element ", i, " is no 'Distribution'")
                return(TRUE) 
            })

# list of univariate distributions
setClass("UnivarDistrList", 
            prototype = prototype(list(new("Norm"))), 
            contains = "DistrList", 
            validity = function(object){
                nrvalues <- length(object)
                for(i in 1:nrvalues)
                    if(!is(object[[i]], "UnivariateDistribution"))
                        stop("element ", i, " is no 'UniveriateDistribution'")
                return(TRUE) 
            })

# multivariate distribution
setClass("MultivariateDistribution", 
            prototype = prototype(r = function(n){ matrix(rep(c(0,0), n), ncol=2) }, 
                                  d = NULL, p = NULL, q = NULL, param = NULL,
                                  img = new("EuclideanSpace", dimension = 2),
                                  support = matrix(c(0,0), ncol = 2)),
            contains = "Distribution")

# discrete mulitvariate distribution
setClass("DiscreteMVDistribution", representation(support = "matrix"),
            prototype(r = function(n){ matrix(rep(c(0,0), n), ncol=2) }, 
                      d = NULL, p = NULL, q = NULL, param = NULL,
                      img = new("EuclideanSpace", dimension = 2),
                      support = matrix(c(0,0), ncol = 2)),
            contains = "MultivariateDistribution")

# condition
setClass("Condition", representation(name = "character"),
            prototype(name = "a condition"))

# conditioning by an Euclidean space
setClass("EuclCondition", 
            representation(Range = "EuclideanSpace"), 
            prototype(name = "conditioning by an Euclidean space",
                      Range = new("EuclideanSpace")),
            contains = "Condition")

## conditional univariate distribution
setClass("UnivariateCondDistribution", 
            representation(cond = "Condition"), 
            prototype(r = function(n, cond){ rnorm(n, mean = 0, sd = 1) },
                      d = NULL, p = NULL, q = NULL, img = new("Reals"), 
                      param = NULL, cond = new("Condition")),
            contains = "Distribution")

# absolutely continuous conditional distribution
setClass("AbscontCondDistribution", 
            representation(cond = "Condition"), 
            prototype(r = function(n, cond){ rnorm(n, mean = 0, sd = 1) },
                      d = NULL, p = NULL, q = NULL, img = new("Reals"), 
                      param = NULL, cond = new("Condition")),
            contains = "UnivariateCondDistribution")

# discrete conditional distribution
setClass("DiscreteCondDistribution", 
            representation(support = "function", 
                           cond = "Condition"), 
            prototype(r = function(n, cond){ rep(0, n) },
                      d = NULL, p = NULL, q = NULL, img = new("Reals"), 
                      param = NULL, support = function(cond){0},
                      cond = new("Condition")),
            contains = "UnivariateCondDistribution")

# parameter of Gumbel distribution
setClass("GumbelParameter", representation(loc = "numeric", 
                                           scale = "numeric"), 
            prototype(name = "parameter of a Gumbel distribution",
                      loc = 0, scale = 1),
            contains = "Parameter",
            validity = function(object){
                if(length(object@scale) != 1)
                    stop("length of 'scale' is not equal to 1")
                if(length(object@loc) != 1)
                    stop("length of 'loc' is not equal to 1")
                if(object@scale <= 0)
                    stop("'scale' has to be positive")
                else return(TRUE)
            })

# Gumbel distribution
setClass("Gumbel", 
            prototype = prototype(r = function(n){ rgumbel(n, loc = 0, scale = 1) },
                                  d = function(x, ...){ dgumbel(x, loc = 0, scale = 1, ...) },
                                  p = function(x, ...){ pgumbel(x, loc = 0, scale = 1, ...) },
                                  q = function(x, ...){ qgumbel(x, loc = 0, scale = 1, ...) },
                                  img = new("Reals"),
                                  param = new("GumbelParameter")),
            contains = "AbscontDistribution")

# Parameter of a linear regression model (with intercept and scale)
setClass("LMParameter", 
            representation(theta = "numeric",
                           intercept = "numeric",
                           scale = "numeric"), 
            prototype(name = "parameter of a linear regression model",
                      theta = 0, intercept = 0, scale = 1),
            contains = "Parameter",
            validity = function(object){
                if(any(!is.finite(object@theta)))
                    stop("inifinite or missing values in 'theta'")
                if(length(object@intercept) != 1)
                    stop("'intercept' has to be of length 1")
                if(!is.finite(object@intercept))
                    stop("inifinite or missing value in 'intercept'")
                if(length(object@scale) != 1)
                    stop("'scale' has to be of length 1")
                if(!is.finite(object@scale))
                    stop("inifinite or missing value in 'scale'")
                return(TRUE)
            })
