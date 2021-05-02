

#RA-Take home Assignment-Quiz-2
#############################################
#Build and Test multiple logistic regression#
#############################################

## The data set provided by Prof. Tathagata Bandyopadhyay for Regression Analysis Course.
# Basic idea here is to assess the creditworthiness of an individual based on the demographic and other
# variables using logistic regression to find the probability that a person will defaults or not based 
# on the independent variables.

#Reading the data  
path <-"https://raw.githubusercontent.com/Kanikaitbhu/Data-Files/main/Credit%20Data.csv"

credit.df <- read.csv(path)
credit.df <- credit.df[ , -c(1)]  # Dropping OBS# code column.

### attaching the variable names
attach(credit.df)

## data summary to see the min/max/median values for variables
summary(credit.df)

#  datatype of the variables
str(credit.df)
# all the variables are integer type.

# data distribution of the Responses
table(credit.df$RESPONSE)

### Distribution of continuous variables -descriptive statistics

par(mfrow = c(1,3))

#distribution of Duration, Amount and Age variables
for( i in c(2,10,22)){
  hist(credit.df[,i], main = colnames(credit.df)[i],xlab =     colnames(credit.df)[i], col = 'yellow')
}



#a. Use descriptive statistics to identify the explanatory variables(categorical) which have an effect 
#on the response.
# y axis has the proportion of people who had good credit rating.
par(mfrow = c(3,3))


barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$CHK_ACCT), 
                  mean, rm.na = T)[,2], xlab = "Checking Account Status", ylab = "credit rating", 
        names.arg = c("<0DM", "0-200DM", ">200DM", "No checking"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$HISTORY), 
                  mean, rm.na = T)[,2], xlab = "Credit History", ylab = "credit rating", 
        names.arg = c("No Credit", "All Paid Back", "Existing paid back", "Delay in Paying", " Critical Account"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$NEW_CAR), 
                  mean, rm.na = T)[,2], xlab = "Purpose of Credit_car(new)", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$USED_CAR), 
                  mean, rm.na = T)[,2], xlab = "Purpose of Credit_car(old)", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$FURNITURE), 
                  mean, rm.na = T)[,2], xlab = "Purpose of Credit_furniture/equipment", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$RADIO.TV), 
                  mean, rm.na = T)[,2], xlab = "Purpose of Credit_radio/TV", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$EDUCATION), 
                  mean, rm.na = T)[,2], xlab = "Purpose of Credit_education", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$RETRAINING), 
                  mean, rm.na = T)[,2], xlab = "Purpose of Credit_RETRAINING", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$SAV_ACCT), 
                  mean, rm.na = T)[,2], xlab = "Checking Account Status", ylab = "credit rating", 
        names.arg = c("<100DM", "101-500DM", "501-1000DM", ">1000DM", "unknwon/No account"))


barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$EMPLOYMENT), 
                  mean, rm.na = T)[,2], xlab = "Present Employment Since", ylab = "credit rating", 
        names.arg = c("unemployed", "< 1 year", "1-3 year", "4-6 years","> 7 years"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$INSTALL_RATE), 
                  mean, rm.na = T)[,2], xlab = "Installment rate", ylab = "credit rating", 
        names.arg = c( "1","2", "3", "4"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$MALE_DIV), 
                  mean, rm.na = T)[,2], xlab = "Male is Divorced", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$MALE_SINGLE), 
                  mean, rm.na = T)[,2], xlab = "Male is single", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$MALE_MAR_or_WID), 
                  mean, rm.na = T)[,2], xlab = "Male is married or Widowed", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$CO.APPLICANT), 
                  mean, rm.na = T)[,2], xlab = "Co-applicant", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$GUARANTOR), 
                  mean, rm.na = T)[,2], xlab = "Has Guaranter", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$PRESENT_RESIDENT), 
                  mean, rm.na = T)[,2], xlab = "Present Resident since", ylab = "credit rating", 
        names.arg = c("<= 1 year", "1-2 year", "2-3 year", ">= 3 year"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$REAL_ESTATE), 
                  mean, rm.na = T)[,2], xlab = "Applicant owns a Real estate", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$PROP_UNKN_NONE), 
                  mean, rm.na = T)[,2], xlab = "Property is unknown or none", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$OTHER_INSTALL), 
                  mean, rm.na = T)[,2], xlab = "Other installment plan", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$RENT), 
                  mean, rm.na = T)[,2], xlab = "Applicant rent", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$OWN_RES), 
                  mean, rm.na = T)[,2], xlab = "Own Residence", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$NUM_CREDITS), 
                  mean, rm.na = T)[,2], xlab = "Number of existing credits at this bank", ylab = "credit rating", 
        names.arg = c( "1","2", "3", "4"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$JOB), 
                  mean, rm.na = T)[,2], xlab = "Nature of job", ylab = "credit rating", 
        names.arg = c("0", "1","2", "3"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$NUM_DEPENDENTS), 
                  mean, rm.na = T)[,2], xlab = "Number of Dependents", ylab = "credit rating", 
        names.arg = c("1", "2"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$TELEPHONE), 
                  mean, rm.na = T)[,2], xlab = "Applicant has Phone number in his or her Name", ylab = "credit rating", 
        names.arg = c("No", "Yes"))

barplot(aggregate(credit.df$RESPONSE == "1", by = list(credit.df$FOREIGN), 
                  mean, rm.na = T)[,2], xlab = "Applicant is a foreign worker", ylab = "credit rating", 
        names.arg = c("No", "Yes"))



#. Use descriptive statistics to identify the explanatory variables(continous) which have an effect 
#on the response.
par(mfrow = c(1,3))
boxplot(credit.df$DURATION~credit.df$RESPONSE, ylab="Duration", xlab= "Response", col="light blue")
boxplot(credit.df$AMOUNT~credit.df$RESPONSE, ylab="Amount", xlab= "Response", col="light blue")
boxplot(credit.df$AGE~credit.df$RESPONSE, ylab="Age", xlab= "Response", col="light blue")



### Creating bins for the continuous variables

credit.df$AGE <- as.factor(ifelse(credit.df$AGE<=20,"1-20",ifelse(credit.df$AGE<=30,"21-30",ifelse(credit.df$AGE<=40,"31-40","50+"))))
credit.df$AMOUNT <- as.factor(ifelse(credit.df$AMOUNT<=4000,"1-4000",ifelse(credit.df$AMOUNT<=8000,"4001-8000",ifelse(credit.df$AMOUNT<=12000,"8001-12000","12000+"))))
credit.df$DURATION <- as.factor(ifelse(credit.df$DURATION<=20,"1-20",ifelse(credit.df$DURATION<=40,"21-40",ifelse(credit.df$DURATION<=60,"41-60","60+"))))


### creating levels for categorical variables.

credit.df$CHK_ACCT<- as.factor(credit.df$CHK_ACCT)
credit.df$HISTORY<- as.factor(credit.df$HISTORY)
credit.df$SAV_ACCT<- as.factor(credit.df$SAV_ACCT)
credit.df$EMPLOYMENT<- as.factor(credit.df$EMPLOYMENT)
credit.df$PRESENT_RESIDENT<- as.factor(credit.df$PRESENT_RESIDENT)
credit.df$JOB<- as.factor(credit.df$JOB)
credit.df$NUM_CREDITS<- as.factor(credit.df$NUM_CREDITS)

# checking the data structure after creating bins and levels.
str(credit.df)


##b. Run the logistic regression model using all predictors and Interpret the result.
logit.reg.full <- glm(RESPONSE ~ ., data = credit.df, family = "binomial") 

## coefficients from the regression
summary(logit.reg.full)

##The odds ratios for all the variables are as below
exp(coef(logit.reg.full))

#checking the measures of model fit
BIC(logit.reg.full)
-2* logLik(logit.reg.full)


##c. Use either backward or forward section method for selection of variables. Use AIC and BIC values as the criteria for model selection.

step(logit.reg.full,direction = "both",trace = 1)


##the reduced model using backward selection model
final_model1 <-glm(formula = RESPONSE ~ CHK_ACCT + HISTORY + NEW_CAR + USED_CAR + 
                     EDUCATION + AMOUNT + SAV_ACCT + INSTALL_RATE + MALE_SINGLE + 
                     GUARANTOR + PRESENT_RESIDENT + PROP_UNKN_NONE + OTHER_INSTALL + 
                     RENT + TELEPHONE + FOREIGN, family = "binomial", data = credit.df)

summary(final_model1)

exp(coef(final_model1))

#checking the measures of model fit
BIC(final_model1)
-2* logLik(final_model1)


##Removing the variable PRESENT_RESIDENT from the Regression variables

final_model2 <-glm(formula = RESPONSE ~ CHK_ACCT + HISTORY + NEW_CAR + USED_CAR + 
                     EDUCATION + AMOUNT + SAV_ACCT + INSTALL_RATE + MALE_SINGLE + 
                     GUARANTOR + PROP_UNKN_NONE + OTHER_INSTALL + 
                     RENT + TELEPHONE + FOREIGN, family = "binomial", data = credit.df)

summary(final_model2)

exp(coef(final_model2))

#checking the measures of model fit
BIC(final_model2)
-2* logLik(final_model2)


#d. Partition the data randomly into training (60%) and validation (40%) sets. Build the regression model selected in (c) based on the training data. Classify the customers in the validation set into two categories, "Credit Rating- Good" and " Credit Rating- Not Good" using the model built from the training data. Find the confusion matrix. For classification, find cut-offs such that, the sensitivity is (i) 80% (ii) 85% and (iii) 90%.

#Splitting the data into training (60%) and validation (40%) datasets. 

# partition data
set.seed(500)
train.index <- sample(c(1:dim(credit.df)[1]), dim(credit.df)[1]*0.6)  
train.df <- credit.df[train.index, ]
valid.df <- credit.df[-train.index, ]

#running regression on the training data


predict_model<-glm(formula = RESPONSE ~ CHK_ACCT + HISTORY + NEW_CAR + USED_CAR + 
                     EDUCATION + AMOUNT + SAV_ACCT + INSTALL_RATE + MALE_SINGLE + 
                     GUARANTOR + PROP_UNKN_NONE + OTHER_INSTALL + 
                     RENT + TELEPHONE + FOREIGN, family = "binomial", data = credit.df)


# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(predict_model , valid.df[,-31], type= "response")

rmsetest=rmse(valid.df$RESPONSE,logit.reg.pred)
rmsetest



library(caret)
## Find the confusion matrix. For classification, use a cut-off point such that,the sensitivity is at least 80%.

confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.639, 1, 0)), 
                as.factor(valid.df[,31]),positive ="1") # Note that we are classifying positive as "1"

## Find the confusion matrix. For classification, use a cut-off point such that,the sensitivity is at least 85%.

confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.595, 1, 0)), 
                as.factor(valid.df[,31]),positive ="1") # Note that we are classifying positive as "1"

## Find the confusion matrix. For classification, use a cut-off point such that,the sensitivity is at least 90%.

confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.52, 1, 0)), 
                as.factor(valid.df[,31]),positive ="1") # Note that we are classifying positive as "1"


## e)Draw the ROC curve for (b) and (d) and comment on the trade-off between sensitivity and specificity.

## ROC curve for full model as per b)

par(mfrow = c(1,1))
library(pROC)
full_model_roc <- glm(RESPONSE~., data= train.df, family = "binomial" )
full_model_roc_pred <- predict(full_model_roc , valid.df[, -31], type = "response")
par(pty="s")
roc_info<-roc(valid.df$RESPONSE,full_model_roc_pred,plot=TRUE,legacy.axes=T,xlab="(1-Specificity)",ylab="(Sensitivity)",levels = c("1","0"))

roc_info

## ROC curve for model as per d)
par(pty="s")
roc_info1<-roc(valid.df$RESPONSE,logit.reg.pred,plot=TRUE,legacy.axes=T,xlab="(1-Specificity)",ylab="(Sensitivity)",levels = c("1","0"))

roc_info1




#Leave one out cross validation - LOOCV & K-fold validation

library(caret)
library(pander)

#Full Model

T<-train(RESPONSE~., data = valid.df,
         method = "glm",family=binomial(), trControl = trainControl(method = "LOOCV"))

print(T)


train.control <- trainControl(method = "cv", number = 10)
# Train the model
U <- train(RESPONSE~., data = valid.df, method = "glm",family=binomial(),
           trControl = trainControl(method = "cv", number=10 ))
# Summarize the results
print(U)










