# R Statistics Essential Training
# Creating scatter plots and regression lines for categorical variables

# Load packages
require(e1071)
require(lattice)

#load dataset into a dataframe.
mydata <- read.csv ("/Users/vinitgohil/Desktop/R/biomechanical-features-of-orthopedic-patients/column_2C_weka.csv")

plot(mydata[,1:6], col=c("orangered", "cornflowerblue"), pch=c(3,1),
     main = "Scatter Plot of all dependent variables against Normal and Abnormal classes", cex.main = 1.0, cex.lab=0.6)

rm(list = ls())
