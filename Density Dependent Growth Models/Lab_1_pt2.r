
  # Density Independent Model Dynamics
  ##library(magrittr)
  ##library(tidyverse)
  ##library(dplyr)
  ##library(ggplot2)
  #Pick a value of lambda for an increasing population
  #Replace ??? below with that value
 ## Sys.setenv(_R_USE_PIPEBIND_ = TRUE)
  ## lambda is used for discrete models
  lambda= 1.2
  ## the rate of the growth (growth rate)
  years <-0:20
  
  N.t.105<-10*lambda^years
  
  #Pick of value of lambda for a decreasing population
  #Replace ??? below with that value
  lambda=.4
  ##growth rate second curve
  N.t.095<-10*lambda^years
  
  #Now use the code below to recreate Fig 2.2 in Ch 1
  #The exact trajectory of the lines will be different BUT
  #You should have 3 lines, one increasing, one decreasing, and one stable
  xlims<-c(0,20)
  ylims<-c(0,30)
  par(las = 1)
   plot1 <- plot(years,
       N.t.105,
       type = "l",
       col = "black",
       lwd = 2,
       xlab = "Time",
       ylab = expression(paste("N"["t"])),
       xlim = xlims,
       ylim = ylims
  )
  lines(years, N.t.095,
        col = "black",
        lwd = 2)
  abline(h = 10,
         col = "black",
         lwd = 2) # this adds horizontal line associated with lambda = 1
  ## text on the plot and location of the text on the plot
  text(x = 10, y= 17, labels = parse(text=expression(paste(lambda,">1",sep=""))))
  text(x = 10, y= 11, labels = parse(text=expression(paste(lambda,"=1",sep=""))))
  text(x = 10, y= 5, labels = parse(text=expression(paste(lambda,"<1",sep=""))))
  
show(plot1)
## displaying the plot

