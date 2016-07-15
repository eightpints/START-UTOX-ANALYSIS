week1 <- read.table("C:\\Users\\Galen\\Documents\\2016 Lab\\LabWorkspace\\SF36DataCleanup\\Week1CompletedPats.csv", header = TRUE, sep= ",")
week4 <- read.table("C:\\Users\\Galen\\Documents\\2016 Lab\\LabWorkspace\\SF36DataCleanup\\Week4CompletedPats.csv", header = TRUE, sep= ",")
week24 <- read.table("C:\\Users\\Galen\\Documents\\2016 Lab\\LabWorkspace\\SF36DataCleanup\\Week24CompletedPats.csv", header = TRUE, sep= ",")

row.num <- nrow(week1)

physical <- c(4:13)
mental.a <- c(25,26,29)
mental.b <- c(27,31)
mental <- c(mental.a, mental.b)

rand.week1 <- matrix(NA, nrow = row.num, ncol = 37)
rand.week4 <- matrix(NA, nrow = row.num, ncol = 37)
rand.week24 <- matrix(NA, nrow = row.num, ncol = 37)

rand.week1 <- as.data.frame(rand.week1)
rand.week4 <- as.data.frame(rand.week4)
rand.week24 <- as.data.frame(rand.week24)

rand.week1[,1] <- week1$cell_line_
rand.week4[,1] <- week4$cell_line_
rand.week24[,1] <- week24$cell_line_

#conversion groups
group.1 <- c(2,3,21,23,35,37)
group.2 <- c(4:13)
group.3 <- c(14:20)
group.4 <- c(22,24,27,28,31)
group.5 <- c(25,26,29,30,32)
group.6 <- c(33,34,36)

for (col in 1:37){
  for (row in 1:row.num){
    if(col == 1){
      
    }
    
    if(col %in% group.1){
      rand.week1[row,col] <- (5-week1[row,col])*25
      rand.week4[row,col] <- (5-week4[row,col])*25
      rand.week24[row,col] <- (5-week24[row,col])*25
    }
    
    if(col %in% group.2){
      rand.week1[row,col] <- (week1[row,col]-1)*50
      rand.week4[row,col] <- (week4[row,col]-1)*50
      rand.week24[row,col] <- (week24[row,col]-1)*50}
    
    if(col %in% group.3){
      rand.week1[row,col] <- (week1[row,col])*100
      rand.week4[row,col] <- (week4[row,col])*100
      rand.week24[row,col] <- (week24[row,col])*100
    }
    if(col %in% group.4){
      rand.week1[row,col] <- (6-week1[row,col])*20
      rand.week4[row,col] <- (6-week4[row,col])*20
      rand.week24[row,col] <- (6-week24[row,col])*20
    }
    
    if(col %in% group.5){
      rand.week1[row,col] <- (week1[row,col]-1)*20
      rand.week4[row,col] <- (week4[row,col]-1)*20
      rand.week24[row,col] <- (week24[row,col]-1)*20
    }
    if(col %in% group.6){
      rand.week1[row,col] <- (week1[row,col]-1)*25
      rand.week4[row,col] <- (week4[row,col]-1)*25
      rand.week24[row,col] <- (week24[row,col]-1)*25
    }
  }
}
#physical vs mental
pm.week1 <- matrix(NA, nrow = row.num, ncol = 2)
pm.week4 <- matrix(NA, nrow = row.num, ncol = 2)
pm.week24 <- matrix(NA, nrow = row.num, ncol = 2)

pm.week1[,1] <- rowSums(rand.week1[,physical])
pm.week1[,2] <- rowSums(rand.week1[,mental])

pm.week4[,1] <- rowSums(rand.week4[,physical])
pm.week4[,2] <- rowSums(rand.week4[,mental])

pm.week24[,1] <- rowSums(rand.week24[,physical])
pm.week24[,2] <- rowSums(rand.week24[,mental])

pat.selection <- c(1:20)

x.1 <-  pm.week1[pat.selection,1]
y.1 <- pm.week1[pat.selection,2]
x.2 <- pm.week4[pat.selection,1]
y.2 <- pm.week4[pat.selection,2]
x.3 <- pm.week24[pat.selection,1]
y.3 <- pm.week24[pat.selection,2]

vals <- rbind(pm.week1[pat.selection,],pm.week4[pat.selection,],pm.week24[pat.selection,])
par(mfrow=c(2,2))

plot(vals, xlab = "Physical", ylab = "Mental", main = "Physical vs Mental")



testDate <- c(1,4,24)

for (i in 1:30){
  splineX <- c(x.1[i],x.2[i],x.3[i])
  splineY <- c(y.1[i],y.2[i],y.3[i])
  xspline(splineX, splineY, shape = c(0,rep(-1, 10-2),0), border="red")
  text(splineX, splineY, testDate, pos=3)
}

plot(pm.week1, xlab = "Physical", ylab = "Mental", main = "Week 1")
plot(pm.week4, xlab = "Physical", ylab = "Mental", main = "Week 4")
plot(pm.week24, xlab = "Physical", ylab = "Mental", main = "Week 24")

