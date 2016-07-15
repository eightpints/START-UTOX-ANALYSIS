library(STARTclindata)
library(splines)


utox.bspline <- function(data , plot = TRUE){
  data[data == -5] <- NA
  
  x <- seq(1, 26, by = 1)
  y <- data[,1]
  
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

utox.predict.missing <- function(data){
  data[data == -5] <- NA
  bspline <- utox.bspline(data, plot = FALSE)
  
  missing.rows <- which(is.na(data))
  m.r.bspline <- missing.rows*4-3
  
  predicted.data <- data
  
  predicted.data[missing.rows] <- bspline$y[m.r.bspline]
  
  for(r in predicted.data[missing.rows]){
    if(r >= .5){
      predicted.data[which(predicted.data==r)] <-1
    }
    if( r< .5){
      predicted.data[which(predicted.data==r)] <- 0
    }
  }
  utox.bspline(predicted.data)
  return(predicted.data)
}

pdf(file = "C:\\Users\\Galen\\Documents\\2016 Lab\\SF36_PCA\\UTOX Predictions.pdf")
par(mfrow=c(2,2))
for (x in 1:50){
  data <- t(UDS[x,opiate.col])
  
  if (any(is.na(data))){
    print(paste(c("has na", x, collapse = " ")))
    bspline <- utox.bspline(data)
    missing <- utox.predict.missing(data)
  }
}
dev.off()
tempdata <- t(UDS[7,opiate.col])
 