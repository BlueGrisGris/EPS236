# A basic box model solved with the linear programming approach.  The time evolution operator
# Set up a 5-box problem similar to transport of tracers in the the earth's atmosphere
# Troposphere: High latitudes, Tropics/Subtropics, in each hemisphere; 
# Advection into and out of the stratosphere (uni-directional
# Slowest exchange into the stratopshere, next slowest across the equator

## The advection and exchange parameters
k21 = k34 = 2 
k23 = k32 = .2 
k25 = k35 = k54 = k51 = .02  
# yr-1

k12 = k21 + k51
k43 = k34 + k54

k50=0 ## loss process, only in the "stratosphere"
# All others = 0; 
# but in another case, allow for a loss process k5o = -0.01  (k5o = removal/loss)
k50.1= -.01

#The Transport matrix K

K = matrix(0, ncol = 5, nrow = 5)  ## initialize with zeros
K1=K

#      1         2          3          4          5
#
#1   -k12       k21                              k51
#2    k12  -(k21+k23+k25)  k32               
#3             k23     -(k32+k34+k35) k43
#4                         k34        -k43       k54
#5             k25         k35             -(k51+k54+ k50|k50.1 )
# vvvvvvvvvvvv
K[1,1]=-k12 
K[2,2]=-(k21+k23+k25) 
K[3,3]=-(k32+k34+k35) 
K[4,4]=-k43 
K[1,2]= k21 
K[1,5]= k51
K[2,1]= k12
K[2,3]= k32
K[3,2]= k23
K[3,4]= k43
K[4,3]= k34
K[4,5]= k54
K[5,5] = -(k51+k54) 
K[5,2]=k25
K[5,3]=k35

K1=K
K1[5,5]=-(k51+k54+k50.1)
#^^^^^^^^^^^^^


##    Integration of the transport equation ------------------------------------------------------------

delt=.01

##   tt is the time vector for the problem
tt=seq(0,100,delt) ## note "feature", seq produces a single precision result //all other R fcns double (!)
tt2 <-seq(0,20,delt)
tt3 <-seq(0,6,delt)
  
  
c0 <- c(1,0,0,0,0) ## an initial condition -> age spectrum of box 1
c1 <- c(0,1,0,0,0)
c2 <- c(0,0,1,0,0)
c3 <- c(0,0,0,1,0)
c4 <- c(0,0,0,0,1)


## solve the problem by integrating the 1st order differential equation (warning -- result may be biased...)
int.results=matrix(0, nrow=10001, ncol=5)

int.results[1,]=c0








