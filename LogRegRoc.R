install.packages("caret")
# Load packages
require(e1071)
require(ggplot2)
require(caret)
require(dplyr)
require(ROCR)
install.packages("pROC")
require(pROC)
require(caTools)

# LOAD DATASETS

train_ds <- read.csv ("/Users/vinitgohil/Desktop/R/biomechanical-features-of-orthopedic-patients/column_2C_weka.csv")

#Subset the data into three columns of Sacral_Slope, Degree_Spoldylolisthesis and #Class
mydata <- train_ds[c(6,4,7)]
a_dfrm <- data.frame(mydata %>% slice(1:50))
b_dfrm<- data.frame(mydata %>% slice(211:260))

train_df <- rbind(a_dfrm,b_dfrm) #combine dataframes

set.seed(100)
split <- sample.split(train_df$class, SplitRatio=0.5)

#get training and test data
dresstrain <-subset(train_df, split == TRUE)
dresstest <- subset(train_df, split == FALSE)

#logistic regression model
model <- glm(class ~ ., data=dresstest, family = binomial)
summary(model)
predict <- predict(model, type = 'response')

predict

#Confusion Matrix
table(dresstest$class, predict > 0.5)

#ROCR Curve
ROCRpred <- prediction(predict, dresstest$class)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf,  main= "Logistic Regression ROC Curve", colorize = TRUE, lwd = 2)
abline(a=0, b=1, lwd=1, lty=2)

#Area under Curve
roc(dresstrain$class, predict, smooth=TRUE)


rm(list = ls())
