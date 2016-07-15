library(STARTclindata)
data("startdata")
SF36 <- startdata$SF36

week1 <- SF36[1:1278, c(1,4:42)]
week4 <- SF36[1:1278, c(1,86:124)]
week24 <- SF36[1:1278, c(1,45:83)]

write.table(na.omit(week1), "c:\\Users\\Galen\\Documents\\2016 Lab\\week1NaOmit.txt", sep=",")
write.table(na.omit(week4), "c:\\Users\\Galen\\Documents\\2016 Lab\\week4NaOmit.txt", sep=",")
write.table(na.omit(week24), "c:\\Users\\Galen\\Documents\\2016 Lab\\week24NaOmit.txt", sep=",")

week1 <- read.table("C:\\Users\\Galen\\Documents\\2016 Lab\\LabWorkspace\\SF36DataCleanup\\Week1CompletedPats.csv", header = TRUE, sep= ",")
week4 <- read.table("C:\\Users\\Galen\\Documents\\2016 Lab\\LabWorkspace\\SF36DataCleanup\\Week4CompletedPats.csv", header = TRUE, sep= ",")
week24 <- read.table("C:\\Users\\Galen\\Documents\\2016 Lab\\LabWorkspace\\SF36DataCleanup\\Week24CompletedPats.csv", header = TRUE, sep= ",")

pca.week1 <- princomp(week1[,-1], scores=TRUE)
pca.week4 <- princomp(week4[,-1], scores=TRUE)
pca.week24 <- princomp(week24[,-1], scores=TRUE)

par(mfrow=c(2,2))
plot(pca.week1$scores[,1:2], main = "Week1", xlab = "PC1", ylab = "PC2", ylim = c(-6,6))
plot(pca.week4$scores[,1:2], main = "Week4", xlab = "PC1", ylab = "PC2", ylim = c(-6,6))
plot(pca.week24$scores[,1:2], main = "Week24", xlab = "PC1", ylab = "PC2", ylim = c(-6,6))


x1 <-  pca.week1$scores[1:10,1]
y1 <- pca.week1$scores[1:10,2]
x2 <- pca.week4$scores[1:10,1]
y2 <- pca.week4$scores[1:10,2]
x3 <- pca.week24$scores[1:10,1]
y3 <- pca.week24$scores[1:10,2]
vals <- rbind(pca.week1$scores[1:10,1:2], pca.week4$scores[1:10,1:2], pca.week24$scores[1:10,1:2])

xlim <- c(-6, 6)
ylim <- c(-4,4)

par(mfrow=c(1,1))
plot(vals, xlim=xlim, ylim=ylim)
testDate <- c(1,4,24)

for (i in 1:10){
  splineX <- c(x1[i],x2[i],x3[i])
  splineY <- c(y1[i],y2[i],y3[i])
  xspline(splineX, splineY, shape = c(0,rep(-1, 10-2),0), border="red")
  text(splineX, splineY, testDate, pos=3)
}

