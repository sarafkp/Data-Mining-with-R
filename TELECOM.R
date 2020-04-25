library(readxl)
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)

telecom <- read.csv("C:/Users/gundawade.p/OneDrive - Procter and Gamble/Documents/Spring 2020/Data Mining II/Project/Data-Mining-with-R/WA_Fn-UseC_-Telco-Customer-Churn.csv")

head(telecom)

colnames(telecom)

dim(telecom)

summary(telecom)

str(telecom)


sapply(telecom, function(x) sum(is.na(x)))

complete.cases(telecom)


telecom <- telecom[complete.cases(telecom),]

cols_recode1 <- c(10:15)
for(i in 1:ncol(telecom[,cols_recode1])) {
  telecom[,cols_recode1][,i] <- as.factor(mapvalues
                                        (telecom[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

telecom$MultipleLines <- as.factor(mapvalues(telecom$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

telecom$tenure_group <- sapply(telecom$tenure,group_tenure)
telecom$tenure_group <- as.factor(telecom$tenure_group)

telecom$SeniorCitizen <- as.factor(mapvalues(telecom$SeniorCitizen,from = c("0","1"),to = c("1","0")))

telecom$customerID <- NULL
telecom$tenure <- NULL

numeric.var <- sapply(telecom,is.numeric)

corr.matrix <- cor(telecom[,numeric.var])
corrplot(corr.matrix, main="/n/nCorrelation Plot for Numerical Variables",method = "circle")

library(corrplot)

install.packages("corrplot")

intrain<- createDataPartition(telecom$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- telecom[intrain,]
testing<- telecom[-intrain,]

LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))


anova(LogModel, test = "Chisq")


testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))


table(testing$Churn, fitted.results > 0.5)

 // Decision Tree //
  
install.packages("partykit")  
library(partykit)
  
tree <- ctree(Churn~Contract+tenure_group+PaperlessBilling, training)
plot(tree)

tree1 <- rpart(Churn~.,training)

library(rpart.plot) 
 
prp(tree1) 

plot(tree)


str(telecom)

head(telecom)

unique(telecom$Contract)
unique(telecom$InternetService)
unique(telecom$Churn)
