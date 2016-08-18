#simulating data
library(refund)
library(plotrix)
library(ggplot2)

#i - patient index

#part one, generating time dependent probability density functions 
#first 200, flat responders
prob_patient<-matrix(NA,1000,24)
utox_patient<-matrix(NA,1000,24)



for (i in 1: 250)
{
  #each patient has one value throughout the study
  flat_value=rnorm(1, mean = 0.95, sd = 0.05) #probability of negative urine
  prob_patient[i,]<-matrix(flat_value,1,24)  
  
  #generate a batch of uniform random numbers
  random_value<-runif(24, 0, 1)
  utox_patient[i,]<-as.numeric(random_value>prob_patient[i,])
}



for (i in 251: 500)
{
  #each patient has one value throughout the study
  flat_value=rnorm(1, mean = 0.5, sd = 0.05)
  prob_patient[i,]<-matrix(flat_value,1,24)  
  
  #generate a batch of uniform random numbers
  random_value<-runif(24, 0, 1)
  utox_patient[i,]<-as.numeric(random_value>prob_patient[i,])
}



for (i in 501: 1000)
{
  #each patient has one value throughout the study
  alpha=rnorm(1, mean=0.5,sd=0.01)
  
  prob_patient[i,]<- 1-exp(-alpha*(1:24))
  
  #generate a batch of uniform random numbers
  random_value<-runif(24, 0, 1)
  utox_patient[i,]<-as.numeric(random_value>prob_patient[i,])
}



image(utox_patient,xlim=range(1:24))

#class information
utox_class<-matrix(NA,1000,1)
utox_class[1:250]<-rep(1,250)
utox_class[1:251]<-rep(3,500)
utox_class[501:1000]<-rep(2,500)

### PCA DOES A VERY BAD JOB ######
utox_pca<-princomp(utox_patient[,])$scores

dat <- data.frame(c = utox_class, 
                  xvar = utox_pca[,1],
                  yvar = utox_pca[,2])
p<-ggplot(dat, aes(x=xvar, y=yvar, color=c))
p + geom_point()


d=kmeans(utox_pca,3)$cluster
p<-ggplot(dat, aes(x=xvar, y=yvar, color=d))
p + geom_point()




###### Gaussian Process Regression
# Demo of Gaussian process regression with R
# James Keirstead
# 5 April 2012

# Chapter 2 of Rasmussen and Williams's book `Gaussian Processes
# for Machine Learning' provides a detailed explanation of the
# math for Gaussian process regression.  It doesn't provide
# much in the way of code though.  This Gist is a brief demo
# of the basic elements of Gaussian process regression, as
# described on pages 13 to 16.


# Load in the required libraries for data manipulation
# and multivariate normal distribution
require(MASS)
require(plyr)
require(reshape2)



# Define the points at which we want to define the functions
# in this case, t = time to utox collection
x.star <- seq(1,24,len=24)


# Calculates the covariance matrix sigma using a
# simplified version of the squared exponential function.
#
# Parameters:
#  X1, X2 = vectors
# 	l = the scale length parameter
# Returns:
# 	a covariance matrix
calcSigma <- function(X1,X2,l=1) {
  Sigma <- matrix(rep(0, length(X1)*length(X2)), nrow=length(X1))
  for (i in 1:nrow(Sigma)) {
    for (j in 1:ncol(Sigma)) {
      Sigma[i,j] <- exp(-0.01*(abs(X1[i]-X2[j])/l)^2)
    }
  }
  return(Sigma)
}

# Calculate the covariance matrix
sigma <- calcSigma(x.star,x.star)
k.xx <- calcSigma(x,x)
k.xxs <- calcSigma(x,x.star)
k.xsx <- calcSigma(x.star,x)
k.xsxs <- calcSigma(x.star,x.star)

# that each of the observed data points have some
# normally-distributed noise.

# The standard deviation of the noise
sigma.n <- 0.1

utox.f.bar<-matrix(NA,1000,24)
utox.f.coefs<-matrix(NA,1000,2)



for (i in 1:1000)
  {
# 2. Now let's assume that we have some known data points;
# this is the case of Figure 2.2(b). In the book, the notation 'f'
# is used for f$y below.  I've done this to make the ggplot code
# easier later on.
f <- data.frame(x=1:24,
                y=utox_patient[i,])

# Calculate the covariance matrices
# using the same x.star values as above
x <- f$x

# Recalculate the mean and covariance functions
f.bar.star <- k.xsx%*%solve(k.xx + sigma.n^2*diag(1, ncol(k.xx)))%*%f$y
cov.f.star <- k.xsxs - k.xsx%*%solve(k.xx + sigma.n^2*diag(1, ncol(k.xx)))%*%k.xxs

utox.f.bar[i,]<-f.bar.star

#extract two coefficients assuming that f.bar is exponential 
y=log(f.bar.star+1)
x=1:24
utox.f.coefs[i,]<-glm(y~x)$coefficients

}






#plot the mean functions using straight PCA
utox.f.bar.pca<-princomp(log(utox.f.bar+1))$scores

dat <- data.frame(c = utox_class, 
                  xvar = utox.f.coefs[,1],
                  yvar = utox.f.coefs[,2])
p<-ggplot(dat, aes(x=xvar, y=yvar, color=c))
p + geom_point()




d=kmeans(utox.f.coefs,3)$cluster
p<-ggplot(dat, aes(x=xvar, y=yvar, color=d))
p + geom_point()

