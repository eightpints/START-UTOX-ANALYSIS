#simulating data
library(refund)
library(plotrix)
library(ggplot2)
library(refund.shiny)
library(fda)
library(splines)
#i - patient index

#part one, generating time dependent probability density functions 
#first 200, flat responders
prob_patient<-matrix(NA,1000,26)
utox_patient<-matrix(NA,1000,26)
utox_bsplines <- matrix (NA, 1000, 101)


for (i in 1: 250)
{
  #each patient has one value throughout the study
  flat_value=rnorm(1, mean = 0.95, sd = 0.05) #probability of negative urine
  prob_patient[i,]<-matrix(flat_value,1,26)  
  
  #generate a batch of uniform random numbers
  random_value<-runif(26, 0, 1)
  utox_patient[i,]<-as.numeric(random_value>prob_patient[i,])
}

for (i in 251: 500)
{
  #each patient has one value throughout the study
  flat_value=rnorm(1, mean = 0.5, sd = 0.05)
  prob_patient[i,]<-matrix(flat_value,1,26)  
  
  #generate a batch of uniform random numbers
  random_value<-runif(26, 0, 1)
  utox_patient[i,]<-as.numeric(random_value>prob_patient[i,])
}

for (i in 501: 1000)
{
  #each patient has one value throughout the study
  alpha=rnorm(1, mean=0.5,sd=0.01)
  
  prob_patient[i,]<- 1-exp(-alpha*(1:26))
  
  #generate a batch of uniform random numbers
  random_value<-runif(26, 0, 1)
  utox_patient[i,]<-as.numeric(random_value>prob_patient[i,])
}

utox.bspline.fake <- function(data , plot = TRUE){
  data[data == -5] <- NA
  
  x <- seq(1, 26, by = 1)
  y <- data
  
  mod <- lm(y ~ bs(x, knots = seq(1,26, by = 2)))
  
  #From http://people.stat.sc.edu/Hitchcock/splinemethodsRexample704.txt
  x.values <- seq(from=1, to=26, by=.25)
  bspline <- data.frame(x = x.values, y = predict(mod,data.frame(x=x.values)))
  
  if(plot){
    mainlab <- paste(c("Patient", colnames(data)[1]), collapse = " ")
    plot(x, y, xlab = "Week", ylab = "Positive/Negative", main = mainlab )
    lines(bspline, col = "red", lwd = 2)
  }
  return(bspline)
}


for (i in 1:1000){
  data <- utox_patient[i,]
  bspline <- utox.bspline.fake(data)
  utox_bsplines[i,] <- t(bspline[,2])
}

fpca.result <- fpca.sc(utox_bsplines)
plot_shiny(fpca.result)
