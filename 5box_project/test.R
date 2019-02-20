
teo.point(K, c0, 13) 

t1 <- 10
t0 <- 0

teo.results <- matrix(0, t1-t0, 5)
teo.results[1,] <- c0

for(i in t0:t1){
  
  
  
  teo.results[i,] <- t(teo.point(K, c0, 41) ) 
  
} #end for
