# R Statistics Essential Training
# Creating scatter plots and regression lines for categorical variables


install.packages("caret")
# Load packages
require(e1071)
require(ggplot2)
require(caret)
require(dplyr)
require(ROCR)
install.packages("pROC")
require(pROC)
install.packages("klaR")
require(klaR)

# LOAD DATASETS

Train_ds <- read.csv ("/Users/vinitgohil/Desktop/R/biomechanical-features-of-orthopedic-patients/column_2C_weka.csv")

#Subset the data into three columns of Sacral_Slope, Degree_Spoldylolisthesis and #Class
mydata <- Train_ds[c(6,4,7)]

a_dfrm <- data.frame(mydata %>% slice(1:50))
b_dfrm<- data.frame(mydata %>% slice(211:260))

df<- rbind(a_dfrm,b_dfrm) #combine dataframes

# build a naive model
nb <- NaiveBayes(class ~., data=df)
nbprediction <- predict(nb, df, type="raw")

#Plot ROC Curve
score = nbprediction$posterior[,2]
actual.class = df$class
pred = prediction(score, actual.class)
nb.perf = performance(pred, "tpr", "fpr")
plot(nb.perf, main="Naive Bayes", colorize = TRUE, lwd = 2)
abline(a=0, b=1, lwd=1, lty=2)

#Area Under the Curve
perf.auc <- performance(pred, measure="auc")
unlist(perf.auc@y.values)


rm(list=ls())
