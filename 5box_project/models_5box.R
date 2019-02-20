


########## integration model

### integration model
### TODO make this apply and not a for loop
### TODO make this a function?


for(j in 1:10000){
  
  int.results[j+1,] <- int.results[j,] + K %*% int.results[j,] * delt
  
}


### plot integration model
matplot(tt, int.results, lty=1,col=c("black","red","blue","cyan","grey"),ylim=c(-.1,2),type="l")
legend(80,2.0, legend = c("Box 1",
                          "Box 2",
                          "Box 3",
                          "Box 4",
                          "Box 5"), col = c("black","red","blue","cyan","grey"), lty = 1)



########### TEO Model

teo.point <- function(K, c0,t){
  
  V0=eigen(K)
  
  
  Xe=V0$vectors
  lambda.0 <- V0$values
  Xe1=solve(Xe)
  
  # print("Xe") 
  # print(Xe) 
  # print("lambda") 
  # print(lambda.0) 
  # print("Xe1") 
  # print(Xe1) 
  
  init.rotate <- Xe1 %*% c0
  
  print("rotate")
  print( init.rotate) 
  
  ### find lambda.t 
  lambda.t <- diag(exp(lambda.0*tt[t])) 
  
  print("lambda t") 
  print(lambda.t) 
  
  
  ### find system state at t
  ct.t <- Xe %*% (lambda.t %*% init.rotate)
  
  print("ct") 
  print(ct.t) 
  
  return(ct.t) 
  
}# end teo.point


teo.span <- function(K, c0, t0, t1){
  
  teo.results <- matrix(0, t1-t0, 5)
  
  for(i in t0:t1){
    
    teo.results[i,] <- zapsmall(t(teo.point(K, c0,i) ) )
    
  } #end for
  
  return(teo.results) 
  
}# end teo.span

teo.span(K, c0, 0, 1000)
