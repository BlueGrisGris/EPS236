
########## integration model

### integration model
### TODO make this apply and not a for loop
### TODO make this a function?

int.results=matrix(0, nrow=10001, ncol=5)

int.results[1,]=c0

for(j in 1:10000){
  
  int.results[j+1,] <- int.results[j,] + K %*% int.results[j,] * delt
  
}

# 
# ### plot integration model
# matplot(tt, int.results, lty=1,col=c("black","red","blue","cyan","grey"),ylim=c(-.1,2),type="l")
# legend(80,2.0, legend = c("Box 1",
#                           "Box 2",
#                           "Box 3",
#                           "Box 4",
#                           "Box 5"), col = c("black","red","blue","cyan","grey"), lty = 1)
# 


########### TEO Model

teo.point <- function(K, c0, t){
  
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
  
  # print("rotate")
  # print( init.rotate) 
  
  ### find lambda.t 
  lambda.t <- diag(exp(lambda.0*tt[t])) 
  
  # print("lambda t") 
  # print(lambda.t) 
  
  
  ### find system state at t
  ct.t <- Xe %*% (lambda.t %*% init.rotate)
  
  # print("ct") 
  # print(ct.t) 
  
  return(ct.t) 
  
}# end teo.point



### many point TEO model
teo.span <- function(K, c0, t0, t1){
  
  teo.results <- matrix(0, t1 + 1 , 5)
  print(dim(teo.results) )
  
  for(i in t0:t1){
    
    teo.results[i,] <- zapsmall(t(teo.point(K, c0,i ) ) )
    
  } #end for
  
  return(teo.results) 
  
}# end teo.span

teo.results0 <- teo.span(K, c0, 1, 10000)


### difference betw/ int model and teo model
sys.error.teo.int <- teo.results0 - int.results

# ### plot sys error model
# matplot(tt, sys.error, lty=1,col=c("black","red","blue","cyan","grey") ,
#         ylim=c(-.001,.001) ,
#         xlim = c(0,30) , 
#         type="l")
# legend(80,2.0, legend = c("Box 1",
#                           "Box 2",
#                           "Box 3",
#                           "Box 4",
#                           "Box 5"), col = c("black","red","blue","cyan","grey"), lty = 1)

### greens functions
# 
teo.results1 <- teo.span(K, c1, 1, 10000)
teo.results2 <- teo.span(K, c2, 1, 10000)
teo.results3 <- teo.span(K, c3, 1, 10000)
teo.results4 <- teo.span(K, c4, 1, 10000)




  ### Superpoistion of greens functions 

new.pulse <- c(0,0,0,2,0) 

super.results.a <- teo.span(K, c1, 1,2000) 

super.results.b <- teo.span(K, new.pulse, 150,2000)

super.results <- super.results.a + super.results.b

### plot sys error model
# matplot(1:2000, super.results, lty=1,col=c("black","red","blue","cyan","grey") ,
#         ylim=c(-.1,2) ,
#         xlim = c(0,2000) , 
#         type="l")
# legend(80,2.0, legend = c("Box 1",
#                           "Box 2",
#                           "Box 3",
#                           "Box 4",
#                           "Box 5"), 
#        col = c("black","red","blue","cyan","grey"), lty = 1)



### reverse time

teo.point.rev <- function(K, c0, t){
  
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
  
  # print("rotate")
  # print( init.rotate) 
  
  ### find lambda.t 
  lambda.t <- diag(exp(lambda.0* (-1*tt[t]) ) ) 
  
  
  ### find system state at t
  ct.t <- Xe %*% (lambda.t %*% init.rotate)
  
  # print("ct") 
  # print(ct.t) 
  
  return(ct.t) 
  
}# end teo.point


teo.span.rev <- function(K, c0, t0, t1){
  
  teo.results <- matrix(0, t1 + 1 , 5)

  for(i in t0:t1){
    
    teo.results[i,] <- zapsmall(t(teo.point.rev(K, c0,i ) ) )
    
  } #end for
  
  return(teo.results) 
  
}# end teo.span

ct <- c(0.13788447, 0.14797195, 0.33182964, 0.33993688, 0.04237706) 


inverse.results <- teo.span.rev(K, ct, 1, 600) 



### introduce error
sd <- .01

ct.error <- c(0.13788447 + rnorm(1, mean = 0, sd = sd), 
              0.14797195 + rnorm(1, mean = 0, sd = sd), 
              0.33182964 + rnorm(1, mean = 0, sd = sd), 
              0.33993688 + rnorm(1, mean = 0, sd = sd),
              0.04237706 + rnorm(1, mean = 0, sd = sd)) 


inverse.results.error <- teo.span.rev(K, ct.error, 1, 600) 

sys.error.rev <- inverse.results - inverse.results.error




