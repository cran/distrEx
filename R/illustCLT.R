if(!isGeneric("illustrateCLT")){
   setGeneric("illustrateCLT", function(Distr, ...) standardGeneric("illustrateCLT"))
}

setMethod("illustrateCLT", signature(Distr = "AbscontDistribution"),
   function(Distr, len, sleep = 0){
       graphics.off()
       par(mfrow = c(1,2))
  
       Sn <- 0
       N <- Norm()
       for(k in 1:len){
           suppressWarnings({Sn <- Sn + Distr})
           Tn <- make01(Sn)
           
           ## graphical output
           x <- seq(-5,5,0.01)
           dTn <- d(Tn)(x)
           ymax <- max(1/sqrt(2*pi), dTn)
           plot(x, d(Tn)(x), ylim = c(0, ymax), type = "l", ylab = gettext("densities"), main = k, lwd = 4)
           lines(x, d(N)(x), col = "orange", lwd = 2)
           plot(x, p(Tn)(x), ylim = c(0, 1), type = "l", ylab = gettext("cdfs"), main = k, lwd = 4)
           lines(x, p(N)(x), col = "orange", lwd = 2)
           Sys.sleep(sleep)
       }
   })
setMethod("illustrateCLT", signature(Distr = "DiscreteDistribution"),
   function(Distr, len, sleep = 0){
       graphics.off()
       par(mfrow = c(1,2))
  
       Sn <- 0
       N <- Norm()
       for(k in 1:len){
           Sn <- Sn + Distr
           Tn <- make01(Sn)

           ## graphical output
           supp <- support(Tn)
           supp <- supp[supp >= -5]
           supp <- supp[supp <= 5]
           dTn <- d(Tn)(supp)
           x <- seq(-5,5,0.01)
           ymax <- max(1/sqrt(2*pi), dTn)
           plot(supp, dTn / max(dTn) * ymax, ylim = c(0, ymax), type = "p",
                ylab = gettext("densities"), main = k, lwd = 4)
           lines(x, d(N)(x), col = "orange", lwd = 2)
           plot(supp, p(Tn)(supp), ylim = c(0, 1), type = "p", ylab = gettext("cdfs"),
                main = k, lwd = 4)
           lines(x, p(N)(x), col = "orange", lwd = 2)
           Sys.sleep(sleep)
       }
   })
