#Final Project
#Shivani Malandkar & Sojwal Shetye

#Cleaning 
#No NAs in dataset
EmployeeAttrition=read.csv("C:/Users/shrey/Downloads/HR-Employee-Attrition.csv")
summary(EmployeeAttrition)
#After looking into Summary ,We can see that there are no NA'S and Some variables 
#such as Education,JobInvolvement..etc  which are factors are 
##stored as integers ,so Converting these  continuos variables to Categorical data##

names <- c('WorkLifeBalance' ,'StockOptionLevel','PerformanceRating','JobSatisfaction',
           'RelationshipSatisfaction','JobLevel','JobInvolvement','EnvironmentSatisfaction','Education')
cleaned_data[,names] <- lapply(cleaned_data[,names] , factor)
head(EmployeeAttrition)
#Checking for missing value and removing non value attribute
apply(is.na(EmployeeAttrition),2,sum)
EmployeeAttrition$EmployeeNumber=NULL
EmployeeAttrition$StandardHours=NULL
EmployeeAttrition$Over18=NULL
EmployeeAttrition$EmployeeCount=NULL
cat("Data Set has",dim(EmployeeAttrition)[1],"Rows and",dim(EmployeeAttrition)[2],"Columns")
sum(is.na(duplicated(EmployeeAttrition)))#No missing values and no duplicate
#Removing columns which have same value for all
cleaned_data=EmployeeAttrition[,-c(9,10,22,27)]
#replacing all blank cells with NA
cleaned_data[cleaned_data==""]=NA
#removing all rows with any blank cell
cleaned_data=cleaned_data[complete.cases(cleaned_data), ]
str(cleaned_data)




library(ggplot2)

library(tidyr)

library(dplyr)

library(corrplot)

library(miscset)
library(purrr)
require(gridExtra)

library(caTools)


library(e1071)

library(glmnet)
#Exploring the data 
dim(cleaned_data)


#performing cross validation, its important to maintain this turnover ratio
attrition<-as.factor(cleaned_data$Attrition)
summary(attrition)#We can see that 237 employees have been retained whereas 1233 employees have been let go of.




#Exploratory data plots 
# Histogram with normal curve for monthly income
# Histogram
histogram.curve <- hist(cleaned_data$MonthlyIncome, breaks = 10, col = "purple", xlab = "Monthly Income", main = "Histogram with Normal Curve")
# Adding normal curve to the histogram
xfit <- seq(min(cleaned_data[,19]), max(cleaned_data[,19]), length=40)
yfit <- dnorm(xfit, mean=mean(cleaned_data[,19]), sd=sd((cleaned_data[,19])))
yfit <- yfit*diff(histogram.curve$mids[1:2])*length(cleaned_data$MonthlyIncome)
lines(xfit, yfit, col ="black", lwd=2)

# Plot showing relationships between employees leaving the company with respect to monthly income, percent salary hike and job level
library(ggplot2)
pl <- ggplot(cleaned_data, aes(x=MonthlyIncome, y=PercentSalaryHike)) + geom_point(shape=2)+ ggtitle("Effect of Job Level(1-5), PercentSalaryHike and MonthlyIncome on Attrition(Y/N)")
pl + facet_grid(Attrition ~ JobLevel)

ggplot(cleaned_data,aes(x = ï..Age ,fill = Attrition)) + geom_bar(position = "fill")


co2 <- ggplot(cleaned_data,aes(x = PercentSalaryHike,fill = Attrition)) + 
  geom_bar(position = "fill") 

co3 <- ggplot(cleaned_data,aes(x = TotalWorkingYears,fill = Attrition)) + 
  geom_bar(position = "fill")

co4 <- ggplot(cleaned_data,aes(x = TrainingTimesLastYear,fill = Attrition)) + 
  geom_bar(position = "fill")

co5 <- ggplot(cleaned_data,aes(x = YearsAtCompany,fill = Attrition)) + 
  geom_bar(position = "fill")

co6 <- ggplot(cleaned_data,aes(x = YearsInCurrentRole,fill = Attrition)) + 
  geom_bar(position = "fill")

co7 <- ggplot(cleaned_data,aes(x = YearsSinceLastPromotion,fill = Attrition)) + 
  geom_bar(position = "fill")

co8 <- ggplot(cleaned_data,aes(x = YearsWithCurrManager,fill = Attrition)) + 
  geom_bar(position = "fill")

co9 <- ggplot(cleaned_data,aes(x = DistanceFromHome,fill = Attrition)) + 
  geom_bar(position = "fill")

co10 <- ggplot()

grid.arrange(co2,co3,co4,co5,co6,co7,co8,co9,ncol=2)

pc1 <- ggplot(cleaned_data,aes(x = BusinessTravel,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill") 


pc2 <- ggplot(cleaned_data,aes(x = Department,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill") + 
  theme(axis.text.x = element_text(size  = 10, angle = 45,hjust = 1,vjust = 1))

pc3 <- ggplot(cleaned_data,aes(x = EducationField,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill") +
  theme(axis.text.x = element_text(size  = 10, angle = 45,hjust = 1,vjust = 1))



pc5 <- ggplot(cleaned_data,aes(x = JobRole,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill") +
  theme(axis.text.x = element_text(size  = 10, angle = 45,hjust = 1,vjust = 1))

pc6 <- ggplot(cleaned_data,aes(x = MaritalStatus,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill")

pc7 <- ggplot(cleaned_data,aes(x = OverTime,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill")


pc9 <- ggplot(cleaned_data,aes(x = JobInvolvement,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill")

pc10 <- ggplot(cleaned_data,aes(x = JobLevel,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill")

pc11 <- ggplot(cleaned_data,aes(x = JobSatisfaction,..count..)) + 
  geom_bar(aes(fill = Attrition),position = "fill")

grid.arrange(pc1,pc2,pc3,pc5,pc6,ncol =2)

####From these graphs, we can Infer that Education and Performance Rating,Training times since
#last year  doesnot impact on Employee Attrition

grid.arrange(pc7,pc9,pc10,pc11,ncol =2)
#From this ,We can infer that employees who  travel frequently will leave
#company when company when compared to Non-Travellers. More than 25% of Employees who work Overtime leave the company

#So, Education Field, Gender, Department ,Trainingtimessincelastyear, Performance rating and 
##Education Field are not strong predictors and I will not be including these variables.


#Checking if there is Multi-Co linearity - High Correlation between 
#ndependent variables
library(corrplot)
empn <- which(sapply(cleaned_data,is.numeric))
corrplot(cor(cleaned_data[empn]),type = "upper",method='color',tl.cex = .7,cl.cex = .7,number.cex = 0.7)


#scatter plot between monthly income, work life balance and attrition
ggplot(EmployeeAttrition,aes(EmployeeAttrition$MonthlyIncome,EmployeeAttrition$WorkLifeBalance, color=Attrition))+geom_point()

#scatter plot between monthly income, JobLevel and attrition
ggplot(EmployeeAttrition,aes(EmployeeAttrition$MonthlyIncome,EmployeeAttrition$JobLevel, color=Attrition))+geom_point()


#boxplot between monthly income and attrition
ggplot(cleaned_data,aes(Attrition,MonthlyIncome,fill=Attrition))+geom_boxplot()


#Logistic Regression
#We split the data into two chunks: training and testing set. The training set will be used to fit our model.
train <- cleaned_data[1:799,]
test<-cleaned_data[800:1400,]
model <- glm(Attrition ~ ï..Age  +BusinessTravel+Department+DistanceFromHome+
               Education+EducationField+JobInvolvement+
               JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+
               OverTime+PercentSalaryHike+RelationshipSatisfaction+
               TotalWorkingYears+TrainingTimesLastYear+ 
               YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
               YearsWithCurrManager,family=binomial(link='logit'),data=train)
summary(model)

# Predicting the results using testing dataset
LR_model.predict <- predict(model, test, type = "response")
length(LR_model.predict)
length(test$Attrition)
library(caTools)
colAUC(LR_model.predict,test$Attrition, plotROC=TRUE)
#Column under ROC

#Make use of the confusion matrix 
conf_mat=table(LR_model.predict,test$Attrition)
#To evaluate this model, we will use 10 repeats of 10-fold cross-validation and use the 100 holdout samples to evaluate the overall accuracy of the model.
set.seed(123) 

library(devtools)

library(caret)

# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)
# Fit Naive Bayes Model
model2 <- train(Attrition ~., data=cleaned_data, trControl=train_control, method="nb")
# Summarise Results
print(model2)


#decision tree
# Load CART packages
library(rpart)
## Warning: package 'rpart' was built under R version 3.3.3
library(rpart.plot)
## Warning: package 'rpart.plot' was built under R version 3.3.3
decisiontree = rpart(Attrition ~ ., data=train, method="class")


#Plot the model

prp(decisiontree)

#Predict on the test data
prediction <- predict(decisiontree, newdata=test, type="class")
#Baseline Accuracy vs CART Accuracy


table(test$Attrition)
504/nrow(test)
#Confusion matrix 
table(test$Attrition, prediction)

#CART model accuracy
(478+27)/(nrow(test))
#Baseline Accuracy - If we just predict attrition as “No” for every observation, we will get an accuracy of 83.8%. Model Accuracy - The model gave us an accuracy of 84%, an improvement of approx. 1% over the baseline accuracy.

#As a fully grown tree is prone to overfitting, lets prune the tree and see if we can improve the model.
printcp(decisiontree)
plotcp(decisiontree)
bestcp <- decisiontree$cptable[which.min(decisiontree$cptable[,"xerror"]),"CP"]
prunedModel <-prune(decisiontree, cp= bestcp)
prp(prunedModel)
printcp(prunedModel)
plotcp(prunedModel)
#Predict on the test data
prediction_pm <- predict(prunedModel, newdata=test, type="class")
table(test$Attrition, prediction_pm)
(487+20)/nrow(test)
#So the pruning does not improve the model accuracy
library(ROCR)

prediction_ROC <- predict(prunedModel, newdata=test)
pred = prediction(prediction_ROC[,2], test$Attrition)
perf = performance(pred, "tpr", "fpr")
plot(perf)
#Area under the curve
as.numeric(performance(pred, "auc")@y.values)


#Random Forest 

 
library(randomForest)
randomForestModel <- randomForest(Attrition~.,data=train,ntree=100,mtry=5, importance=TRUE)
print(randomForestModel)

plot(randomForestModel, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest")

## List the importance of the variables.
impVar <- round(randomForest::importance(randomForestModel), 2)
impVar[order(impVar[,3], decreasing=TRUE),]

# Tuning Random Forest
tunedRf <- tuneRF(x = train[,-2], 
                  y=as.factor(train$Attrition),
                  mtryStart = 5, 
                  ntreeTry=60, 
                  stepFactor = 2, 
                  improve = 0.001, 
                  trace=TRUE, 
                  plot = TRUE,
                  doBest = TRUE,
                  nodesize = 5, 
                  importance=TRUE
)

impvarTunedRf <- tunedRf$importance
impvarTunedRf[order(impvarTunedRf[,3], decreasing=TRUE),]

predictionRf <- predict(tunedRf, test, type="class")



#RandomForest Accuracy
#Confusion matrix 

t2 <- table(test$Attrition, predictionRf)
t2
#RandomForest model accuracy
(t2[1]+t2[4])/(nrow(test))

#Xtreme Gradient Boosting


library(caret)
library(xgboost)

control <- trainControl(method="repeatedcv", number=5)
set.seed(123)
model_xgb <- train(as.factor(Attrition)~., data=train, method="xgbTree",  trControl=control)
#Output Prediction
pred_xgb <- predict(model_xgb, newdata=test)

library(ROCR)
ROCRpred <- prediction(as.numeric(pred_xgb), as.numeric(test$Attrition))
ROCRpref <- performance(ROCRpred,"auc")
auc_xgb <- as.numeric(ROCRpref@y.values)
perf_ROC <- performance(ROCRpred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(auc_xgb, digits=5, scientific=FALSE)))

#Confusion Matrix
confusionMatrix(pred_xgb, test$Attrition)


