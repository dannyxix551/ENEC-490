#Exercise 3
## Dan Crownover
#Intro to Quantitative Ecology
## Yellow Fin Tuna Catch and Effort Models(Surplus)

#For this exercise we are going to use a package written by Malcolm Haddon
#He has written a book that teaches lots of quantitative methods:
#Modelling and Quantitative Methods in Fisheries

#First load packages
install.packages("MQMF")
install.packages("devtools")
library(devtools)
devtools::install_github("https://github.com/haddonm/MQMF")
library(MQMF)

#schaef contains the catches and cpue of the original yellow-fin tuna data
#from Schaefer (1957), which was an early example of surplus production models 
#the data is already in this package MQMF
#Catches are ’000s of lbs, effort is ’000s of standard class4 clipper days
#Catch per unit effort (cpue) is ’000s lbs/day.
data(schaef)  

#First plot the data
#mfpar is going to make a 3 panel plot with 3 rows, 1 column
#mar gives margins for mar(bottom, left, top, right)
# https://r-graph-gallery.com/74-margin-and-oma-cheatsheet.html

## par 3 panel plot with 3 rows(very simple and staightfoward)
par(mfrow=c(3,1),mar=c(3,3,1,1)) 

## this is catchees over data
plot(schaef[,"year"],schaef[,"catch"],ylab="Catch",xlab="Year",  
      type="l",lwd=2)  

## index of abundance-- decreasing that population is decreasing but landings are increasing
plot(schaef[,"year"],schaef[,"cpue"],ylab="CPUE",xlab="Year",  
      type="b",lwd=2)  
plot(schaef[,"catch"],schaef[,"cpue"],type="p",ylab="CPUE",  
      xlab="Catch",pch=16,cex=1.0) 

#Next run a linear regression between catch and CPUE
## this is a simple linear regression model
## catch per unit effort y catch is x
model <- lm(schaef[,"cpue"] ~ schaef[,"catch"])  

#Check how well the model fits
summary(model)

#plot the model on the last plot(this isn't very good )
abline(model,lwd=2,col=2)   

#Test for autocorrelation using a cross correlation function (ccf)
parset(cex=0.85) #sets par parameters for a tidy base graphic

ccf(x=schaef[,"catch"],y=schaef[,"cpue"],type="correlation",  
    ylab="Correlation",plot=TRUE)  
## above is looking at coorelation between catch and catch per unit effort
## now plot schaef data with timelag of 2 years on cpue   

par(mfrow=c(3,1),mar=c(3,3,1,1)) 
#This first plot should be the same
plot(schaef[1:20,"year"],schaef[1:20,"catch"],ylab="Catch",  
      xlab="Year",type="l",lwd=2) 
#This plot should look different-you are plotting the CPUE lagged 2 years
plot(schaef[3:22,"year"],schaef[3:22,"cpue"],ylab="CPUE",  
      xlab="Year",type="b",lwd=2)  
plot(schaef[1:20,"catch"],schaef[3:22,"cpue"],type="p",  
      ylab="CPUE",xlab="Catch",cex=1.0,pch=16)  
model2 <- lm(schaef[3:22,"cpue"] ~ schaef[1:20,"catch"])  
abline(model2,lwd=2,col=2)  

#Plot CPUE as a function of Effort
plot(schaef[1:20,"cpue"],schaef[3:22,"effort"],type="p",  
     ylab="CPUE",xlab="effort",cex=1.0,pch=16) 
model3<-lm(schaef[1:20,"cpue"] ~ schaef[1:20,"effort"])

summary(model2)
#Get the polynomial function from your linear model
est_catch<-model3$coefficients[1]*schaef[,"effort"]+model3$coefficients[2]*schaef[,"effort"]^2
est_catch<-model3$coefficients[1]*schaef[,"effort"]+model3$coefficients[2]*schaef[,"effort"]^2
## takes the value of effort and calculates what the modelm effort is.
#Now plot the Catch by effort and your model
par(mfrow=c(1,1))

plot(schaef[,"effort"],est_catch, type = "p", ylab="Catch",
     xlab="Effort", pch=20,xlim=c(5000,50000))


points(schaef[,"effort"], schaef[,"catch"], col = 'red')

