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
