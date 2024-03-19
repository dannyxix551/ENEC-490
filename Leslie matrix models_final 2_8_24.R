
#  Quantitative Ecology             
#  Matrix population models           
#  Dan Crownover
#  2-1-2024


# uncomment this line to run - you only need to install a package (library) once
install.packages("popbio")  
#You need the package popbio to run the Leslie matrix model 
#It conveniently calculates stable age distribution etc.

library(popbio)   # load the 'popbio' package in R

#Load your matrix
TMat_inside_park <- matrix(     
  c(
    0, 0.4375, 0.214, 1.04, 0.932, 0.2, 0, 
    0.4375, 0, 0, 0, 0, 0, 0, 
    0, 0.714, 0, 0, 0, 0, 0,
    0, 0, 0.52, 0, 0, 0, 0, 
    0, 0, 0, 0.92, 0, 0, 0, 
    0, 0, 0, 0, 0.5, 0, 0, 
    0, 0, 0, 0, 0, 0.00833, 0
  ),
  nrow = 7, ncol = 7, byrow = TRUE
)


TMat_outside_park <- matrix(     
  c(
    0, 0.5, 2.4, 1, 0.6, 0.2, 0, 
    0.5, 0, 0, 0, 0, 0, 0, 
    0, 0.8, 0, 0, 0, 0, 0,
    0, 0, 0.5, 0, 0, 0, 0, 
    0, 0, 0, 0.6, 0, 0, 0, 
    0, 0, 0, 0, 0.5, 0, 0, 
    0, 0, 0, 0, 0, 0.0069, 0
  ),
  nrow = 7, ncol = 7, byrow = TRUE
)

TMat_inside_park    # print to the console to check!
TMat_outside_park
# Use the 'popbio' package to compute lambda 

lambda(TMat_inside_park) ## how fast pop is growing
lambda(TMat_outside_park) ## how fast pop is growing

# Use the 'popbio' package to compute the stable age distribution!

stable.stage(TMat)

eigen.analysis(TMat)

#Now we are going to use the Leslie matrix to calculate abundance over time
# Then we specify initial abundances for the three age classes

InitAbund <- c(1000,0,0)    # initial abundance vector
InitAbund    # print to the console to check!

# First calculate year-1 abundance:
# matrix multiplication in R uses the symbol '%*%'

## first year abundance
Year1 <- TMat %*% InitAbund  
Year1

# Calculate year-2 abundance

Year2 <- TMat %*% Year1  # matrix multiplication!
Year2

# Multi-year projection code ----------------------------

#  You may want to modify this code for the examples below:


# Set key parameters -----------------------

nYears <- 20                                            # set the number of years to project
TMat <- matrix(     # 
  c(
    0.4,     1.9,   0,
    0.4,   0,     0,
    0,     0.75,   0
  )
  ,nrow=3,ncol=3,byrow=T
)
InitAbund <- c(1000,0,0)                                # initial abundance vector
AgeStructured <- TRUE   # set to TRUE for Leslie matrix and FALSE for Lefkovitch 

# build a storage array for all stages and all years!
#This is NOT the Leslie matrix, it's just a table to store data
allYears <- matrix(0,nrow=nrow(TMat),ncol=nYears+1) 
#Print this out so you can see this is an "empty" matrix
#You often have to set up a "dummy" matrix to store values so good tool to learn
allYears
allYears[,1] <- InitAbund  # set the year 0 abundance 

# Here we are going to set up a "for loop"
# Loops are tremendously useful when you need to do the same thing "for" 
# each year, each species, etc.  
# Rather than rewrite the same code you just do the same thing for each "group"
# This for loop multiplies the Leslie matrix by  through all years 
# t is a variable that you make
for(t in 2:(nYears+1)){   
  allYears[,t] <-  TMat %*% allYears[,t-1]
}



#open your allYears matrix now which was all zeroes before

allYears

#What are all those numbers???
#Those are the numbers of individuals in each age class 1,2,3!

#Now we are going to plot the abundance of each age class by year
#First you make an empty plot
plot(1,1,pch="",ylim=c(0,max(allYears)),xlim=c(0,nYears+1),xlab="Years",ylab="Abundance",xaxt="n")  # set up blank plot
cols <- rainbow(ncol(TMat))    # set up colors to use
#Now you are going to use a loop again to plot out each life stage abundance, one at a time
for(s in 1:ncol(TMat)){
  points(allYears[s,],col=cols[s],type="l",lwd=2)     
}
axis(1,at=seq(1,nYears+1),labels = seq(0,nYears))   # label the axis
if(AgeStructured){
  leg <-  paste("Age",seq(0,(ncol(TMat))-1))
}else{
  leg <- paste("Stage",seq(1,ncol(TMat))) 
}
legend("topleft",col=cols,lwd=rep(2,ncol(TMat)),legend=leg,bty="n")  # put a legend on the plot

LMat <- matrix(     
  c(
    0.4721,  .8498,   0.1273,
    0.9924,   0,     0,
    0,   0.9826,   0 )

  ,nrow=3,ncol=3,byrow=T 
)
LMat
lambda(LMat) ## how fast pop is growing

eigen.analysis(LMat)

factors that scale with population density, such as competition for resources, predation, disease transmission, and intraspecific interactions.


