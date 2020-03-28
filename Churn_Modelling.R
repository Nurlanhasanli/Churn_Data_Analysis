library(tidyverse)
library(readr)
churn <- read_csv('C:/Users/Churn_Modelling.csv')
glimpse(churn)


sapply(churn,function(x) sum(is.na(x)))


df <- subset(churn,select=c(4:14))


#Model fitting
train <- df[1:8000,]
test <- df[8001:10000,]


model <- glm(Exited ~.,family=binomial,data=train)
summary(model)

#Analysis of deviance table
anova(model, test="Chisq")

#install.packages('pscl')
library(pscl)
pR2(model)


#Assessing the predictive ability of the model
fitted.results <- predict(model,newdata = subset(test,select=1:10),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Exited)
print(paste('Accuracy',1-misClasificError))


#ROC
#install.packages('ROCR')
library(ROCR)
p <- predict(model, newdata = subset(test,select=c(1,2,3,4,5,6,7,8,9,10)), type="response")
pr <- prediction(p, test$Exited)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,colorize=TRUE)



auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# > auc
# [1] 0.7421134
