  LLN <- function(x = Norm(),n = c(1,3,5,10,25,50,100,500,1000,10000), m = 50,
                  step = 1, sleep = 0, ...){
  
  xc <- match.call(call = sys.call(sys.parent(1)))$x
  if(is.null(xc)) xc <- "Norm()"
  else xc <- as.character(deparse(xc))

  da <- matrix(NA,m,length(n))
  
  omar <- par("mar")
        ## getting the parameter
     slots <-  slotNames(param(x))
     slots <-  slots[slots != "name"]
     nrvalues <-  length(slots)
     if(nrvalues > 0){
           values <-  numeric(nrvalues)
       for(i in 1:nrvalues)
         values[i] <-  attributes(attributes(x)$param)[[slots[i]]]
         paramstring <-  paste("(",paste(values, collapse = ", "),")",sep = "")
     }
     else paramstring <- ""
  
  tit <- c(sprintf("LLN: Convergence against E[X] -- case %s%s", class(x), 
                  paramstring), sprintf("called with %s", xc))
  if ( is (x, "Cauchy"))
       tit <- c(sprintf("LLN: Non-Convergence against E[X] -- case %s%s", 
                class(x), paramstring), sprintf("called with %s", xc))
    
  LLNin <- function(x, n, from, to = from){
  for(i in seq(length(n)))
      da[from:to,i] <<- 
         rowMeans(matrix(r(x)(n[i]*(to-from+1)),(to-from+1),n[i]))
  }

   ## location and scale:
   
   mE <-  if(!is.na(E(x)))   E(x) else median(x)
   msd <- if(!is.na(sd(x))) sd(x) else 2 * mad(x)
 
   Ns <- seq(length(n))
   nn <- if(is(x,"Cauchy")) n*0+1 else n

   #confidence bounds based on CLT

   for(j in seq(1, m, by = step)) 
     {LLNin(x, n, j, j+step-1)
      matplot(Ns, t(da), pch = 16, col="black",  axes = FALSE,
           ylim = q(x)(c(0.02,0.98)), xlab = "Sample size n", 
           ylab = "", ..., main = tit)
 
      title(ylab = expression(paste("Realisations of ",
        ~~~~~bar(X)[n]==~~sum(X[i],i==1,n)/n)), line = 1.7)
      axis(1, at = Ns, labels = n)
      axis(2)
      abline(h=mE, ...)
      matlines(Ns, mE + qnorm(.975)*msd*1/sqrt(nn)%o%c(-1,1), lty=2, col="red",
               ...)
      coverage <- colMeans( t(t(da) <= (mE+qnorm(.975)*msd*1/sqrt(nn)) & 
                          t(da) >= (mE-qnorm(.975)*msd*1/sqrt(nn))) , 
                          na.rm= TRUE )
      mtext(at = c(0,Ns), 
         c("coverage",sprintf("%3.2f",round(coverage,2))),cex=0.7)
      Sys.sleep(sleep)
      }
  }
  
  


