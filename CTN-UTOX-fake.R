#simulating data
library(refund)
library(plotrix)
library(ggplot2)
library(STARTclindata)

data("startdata")
UDS <- startdata$UDS

opiate.col <- c(seq (from = 27, to= 802, by = 31))
Opiate_UDS <- UDS[,opiate.col]
Opiate_UDS = as.matrix(as.data.frame(lapply(Opiate_UDS, as.numeric)))

#convert missing to positive
Opiate_UDS[Opiate_UDS == -5] <- 1
Opiate_UDS[is.na(Opiate_UDS)] <-1

opiate_data <- Opiate_UDS[1,]
treatment_group <- startdata$demog[1,c(1,6)]


for (x in 1:1278){
  test <- Opiate_UDS[x,]
  treatment <- startdata$demog[x,c(1,6)]
  
  if (any(is.na(test))){
  }else {
    if (any(test=="-5")){
    }else{
      opiate_data <- rbind(test, opiate_data)
      treatment_group <- rbind(treatment, treatment_group)
    }
  }
}

#shapes for later on
treatment_group[treatment_group == 1] <- 16
treatment_group[treatment_group == 2] <- 17

#method is only for the first 24 weeks
opiate_data <- opiate_data[,1:24]

#class information
utox_class<-matrix(NA,1278,1)
utox_class[1:1278]<-rep(1,1278)


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

utox.f.bar<-matrix(NA,1278,24)
utox.f.coefs<-matrix(NA,1278,2)



for (i in 1:1278)
{
  # 2. Now let's assume that we have some known data points;
  # this is the case of Figure 2.2(b). In the book, the notation 'f'
  # is used for f$y below.  I've done this to make the ggplot code
  # easier later on.
  f <- data.frame(x=1:24,
                  y=opiate_data[i,])
  
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


d=kmeans(utox.f.coefs,2)$cluster
p<-ggplot(dat, aes(x=xvar, y=yvar, color=d))
p + geom_point(shape = treatment_group[1:1278,2])+ 
  labs(title = "K-means cluster")
  
