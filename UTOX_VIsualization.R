library(STARTclindata)
library(plotrix)
data("startdata")

UDS <- startdata$UDS

amp.col <- c(seq(from = 15, to = 790, by = 31 ))
benzo.col <- c(seq(from =17, to = 792, by = 31))
methadone.col <- c(seq(from =19, to = 794, by = 31))
oxy.col  <- c(seq(from =21, to = 796, by = 31))
cocaine.col<- c(seq(from =23, to = 798, by = 31))
meth.col<- c(seq(from =25, to = 800, by = 31))
opiate.col <- c(seq (from = 27, to= 802, by = 31))
cannabis.col<- c(seq(from =29, to = 804, by = 31))
prop.col <- c(seq(from =31, to = 806, by = 31))

utox.visualization <- function(x, substance, numpatients){
  
  data <- UDS[c(1:numpatients), x]
  data <- data.matrix(data)
  data[is.na(data)] <- -5
  data[data == 0] <- -10
  cellcolors <- matrix (NA,nrow = numpatients, ncol = 26)
  
  #red for positive
  cellcolors[data == 1 ] <- 
    color.scale(data[data == 1 ], cs1 = 0.7, cs2 = 0,cs3 = 0)
  
  #blank for unknowns
  cellcolors[data == -5 ] <- 
    color.scale(data[data == -5], cs1 = 1, cs2 = 1,cs3 = 1)
  
  #green for negative
  cellcolors[data == -10] <- 
    color.scale(data[data == -10], cs1 = 0, cs2 = 0.7,cs3 = 0)
  
  color2D.matplot(data, xlab = "Week", ylab = "Patient", cellcolors = cellcolors, main = substance )
  
}

pdf(file = "C:\\Users\\Galen\\Documents\\2016 Lab\\SF36_PCA\\UTOX Visualization.pdf")
utox.visualization(opiate.col, substance = "Opiates",numpatients =  150)
utox.visualization(amp.col, "Amphetamines", numpatients = 150)
utox.visualization(benzo.col, "Benzodiapines", 150)
utox.visualization(methadone.col, "Methadone",150)
utox.visualization(oxy.col, "Oxycodone", 150)
utox.visualization(cocaine.col, "Cocaine", 150)
utox.visualization(meth.col, "Meth", 150)
utox.visualization(cannabis.col, "Cannabis", 150)
utox.visualization(prop.col, "Propoxyphene", 150)
dev.off()
