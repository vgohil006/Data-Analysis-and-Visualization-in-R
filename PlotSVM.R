# R Statistics Essential Training
# Creating scatter plots and regression lines for categorical variables


install.packages("caret")
# Load packages
require(e1071)
require(ggplot2)
require(caret)
require(dplyr)
require(ROCR)
require(pROC)

# LOAD DATASETS

newdataset <- read.csv ("/Users/vinitgohil/Desktop/R/biomechanical-features-of-orthopedic-patients/column_2C_weka.csv")

newdataset

#Subset the data into three columns of Sacral_Slope, Degree_Spoldylolisthesis and #Class
mydata <- newdataset[c(6,4,7)]

#Breakdown the subset to just show only Normal Class
#plotdata <- mydata[mydata$class %in% c("Normal", "Abnormal"), ]

max(mydata$sacral_slope)
min(mydata$sacral_slope)
max(mydata$degree_spondylolisthesis)
min(mydata$degree_spondylolisthesis)

#Define your x and y variables for the plot
x = mydata$sacral_slope
y = mydata$degree_spondylolisthesis

?plot

#Label the Class variables for the legend labels when plotting
plotclass <- factor(mydata$class, c("Abnormal", "Normal"), labels = c("1 - Abnormal", "0 - Normal"))

plotclass

#Plot the data 
plot(x,y, col=c("red", "mediumseagreen") [plotclass], pch=c(18,20) [plotclass], 
     type = "p",
       cex=1.0,
     main = "Plot of Degrees of Spondylolisthesis against Sacral Slope for
     Normal and Abnormal Classes", cex.main = 0.8, cex.lab=0.8,
     ylab = "Degree of Spondylolisthesis",
     ylim=c(-10,150),
     xlab = "Sacral Slope",
     xlim=c(0,90))
#Add the plotclass legends
legend(x="topleft", legend = levels(plotclass), col=c("red", "mediumseagreen"), pch=c(18,20), cex = 0.7)

?points

#define the regression line
lm(y~x)
abline(lm(y~x), h=0, v=0, col="blue", lwd = 1.5, lty=3)

?svm

#Fit an SVM Model
svm.model <- svm(mydata$class ~., data=mydata, kernel ='linear', cost=0.1, scale = FALSE, probability = TRUE)

#Determine the support vectors
svm.model$index

#Plot the data using the SVM Model
plot(svm.model, data=mydata, xlim=c(15,90), ylim=c(-10,180), col=c("lightpink1", "slategray2"), pch=c(3,1), cex=0.5)
abline(h=0,v=0,lty=3, lwd=1.5, col="blue")

?points
#Determine the support vectors with points command
points(mydata[svm.model$index,1], pch=10, col="dodgerblue4", cex=0.8)
points(mydata[svm.model$index,2], pch=8, col="firebrick2", cex=0.8)

#Get the parameters of the hyperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <-svm.model$rho

#Define the hyperplane from the parameters calculated
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=3, lwd=1.5)

#Run a prediction against the SVM Model
#table of predcition will validate the confusion matrix
predict(svm.model, mydata)
svm.pred <- predict(svm.model, mydata[,-3])
table(prediction = svm.pred, true = mydata[,3])
summary(svm.pred)

#The following determines the confusion matrix using the SVM prediction
levels(mydata$class)
confusionMatrix(svm.pred, sample(mydata$class))

#The following determines the confusion matrix using the actual test data
confusionMatrix(mydata$class, sample(mydata$class))


#Plot ROC Cuve based on the SVM Prediction
model <- glm(svm.model, data=mydata, family =  binomial)
summary(model) #summary of the SVM prediction
predict <- predict(model, type = "response") #Run a test prediction
head(predict)

svm.pred <- prediction(predictions = predict, labels = mydata$class)
svm.perf <- performance(svm.pred, measure="tpr", x.measure="fpr")

#Plot the curve of True Positive against False Positivbe
plot(svm.perf, main = "ROC Curve for SVM Model", colorize = TRUE, lwd=2)
abline(a=0, b=1, lwd=1, lty=2)

#Area under Curve
roc(mydata$class, predict, smooth=TRUE)

rm(list = ls())
