

for 

Xe[,1] %*% diag(exp(lambda.0*tt[t]))[,1] %*% init.rotate[,1]

plot(Xe[,1], tt) 


teo.span.new <- function(K, pulse, tt){
  
  teo.results <- matrix(0, length(tt), 5)
  
  for(i in tt){
    
    teo.results[i,] <- zapsmall(t(teo.point(K, pulse,i) )[,1] )
    
  } #end for
  
  return(teo.results) 
  
}# end teo.span


x <- teo.span.new()