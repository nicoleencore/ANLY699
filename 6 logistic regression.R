#DATA PREPROCESSING
train=read.csv('C:/Users/Nicole Guo/Desktop/H1B MATERIAL/CPT/HU/6-ANLY 699/project/kaggle healthcare-data/train_2v.csv')
str(train)

train [train =="Other"] <- NA
sum(is.na(train)==T)
train=na.omit(train) # Removed Rows With Missing Values (e.g., Gender=="Other" to make geneder-related results easier to interpret)
train=train[,-1]
names(train)[7] <- "residence_type"
str(train) #thus missing value rows removed, column renamed, and the number of observations is big enough


test=read.csv('C:/Users/Nicole Guo/Desktop/H1B MATERIAL/CPT/HU/6-ANLY 699/project/kaggle healthcare-data/test_2v.csv')
str(test)

test [test =="Other"] <- NA
sum(is.na(test)==T)
test=na.omit(test) # Removed Rows With Missing Values (including Gender=="Other")
test=test[,-1]
names(test)[7] <- "residence_type"
str(test) #thus missing value rows removed, renamed, and the number of observations is big enough





###############

#LOGISTIC REGRESSION
##heart disease vs hypertension, hypertension vs bmi
mod.hearthyp <- glm(heart_disease ~ hypertension, train, family = binomial)
summary(mod.hearthyp)
anova(mod.hearthyp, test="Chisq")#thus ANOVA gives the same finding
coef(mod.hearthyp)
#thus probability (of having heart disease) will (based on the strength of the relationship):
#increase by 1.33 as hypertension increases by 1 (having hypertension) 


#other methods for evaluation show that:
#The null deviance show how well the response variable is predicted by the intercept and is of the value 14905.

#The residual deviance show how well the response variable is predicted by adding the independent variables and is of the value 14468.

#The AIC show how well the model is fitting the data and is of the value 14472. The value is undesirable as it is high.

#The number of Fisher Scoring iterations show that performing the fit needs 6 models. The value is desirable as it is low.


###check outliers
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(mod.hearthyp) #thus as shown in the Residuals vs Leverage plot, outliers don't exist. No cases are outside of the Cook's distance (meaning they have high Cook's distance scores) and influential to the regression results

library(effects)
plot(allEffects(mod.hearthyp)) #thus heart disease is strongly predicted by hypertension






mod.hypbmi <- glm(hypertension ~ bmi, train, family = binomial)
summary(mod.hypbmi)
anova(mod.hypbmi, test="Chisq")#thus ANOVA gives the same finding
coef(mod.hypbmi)
#thus probability (of having hypertension) will (based on the strength of the relationship):
#increase by 0.06 as BMI increases by 1 


#other methods for evaluationd show that:
#The null deviance show how well the response variable is predicted by the intercept and is of the value 24888.

#The residual deviance show how well the response variable is predicted by adding the independent variables and is of the value 23912.

#The AIC show how well the model is fitting the data and is of the value 23916. The value is undesirable as it is high.

#The number of Fisher Scoring iterations show that performing the fit needs 5 models. The value is desirable as it is low.


###check outliers
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(mod.hypbmi) #thus as shown in the Residuals vs Leverage plot, outliers don't exist. No cases are outside of the Cook's distance (meaning they have high Cook's distance scores) and influential to the regression results

library(effects)
plot(allEffects(mod.hypbmi)) #thus hypertension is strongly predicted by BMI







##heart disease vs all - attempt 1
###build the model using the training dataset
mod.heartall<- glm(heart_disease ~ gender+age+ever_married+work_type+residence_type+smoking_status+avg_glucose_level+bmi, data = train, family = binomial)
summary(mod.heartall) #thus heart disease is predicted by (significant if p value < 0.05): gender, age, smoking status, average glucose level, BMI, AND NOT BY marital status/work type/residence type
anova(mod.heartall, test="Chisq") #thus ANOVA gives the same finding
coef(mod.heartall) #thus big difference between work type and the other predictors
unique(train$gender)
unique(train$ever_married)
unique(train$residence_type)
#thus probability (of having heart disease) will (based on the strength of the relationship):
#1) increase by 0.78 as gender increases by 1 (becoming male, instead of female) 
#2) increase by 0.08 as age increases by 1 year
#3) increase by 0.15 as marital status increases by 1 (becoming married, instead of single)
#4) decrease by 0.14 as work type (government job) increases by 1
#5) decrease by 10.71 as work type (never worked) increases by 1
#6) decrease by 0.10 as work type (private) increases by 1
#7) decrease by 0.09 as work type (self-employed) increases by 1
#8) decrease by 0.07 as residence type increases by 1 (becoming urban, instead of rural)
#9) increase by 0.26 as smoking status (formerly smoked) increases by 1
#10) decrease by 0.08 as smoking status (never smoked) increases by 1
#11) increase by 0.64 as smoking status (smokes) increases by 1
#12) increase by 0.01 as average glucose level increases by 1
#13) increase by 0.02 as BMI increases by 1


#other methods for evaluation show that:
#The null deviance show how well the response variable is predicted by the intercept and is of the value 14905.

#The residual deviance show how well the response variable is predicted by adding the independent variables and is of the value 11336.

#The AIC show how well the model is fitting the data and is of the value 11364. The value is undesirable as it is high.

#The number of Fisher Scoring iterations show that performing the fit needs 15 models. The value is UNDESIRABLE as it is high.



###check multicollinearity
library(car)
vif(mod.heartall) #thus there is no collinearity: all variables have a value of VIF well below 5

###check outliers
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(mod.heartall) #thus as shown in the Residuals vs Leverage plot, outliers don't exist. No cases are outside of the Cook's distance (meaning they have high Cook's distance scores) and influential to the regression results

###effect plot
library(effects)
mod.heartall.plot <- allEffects(mod.heartall)
plot(mod.heartall.plot) #thus heart disease is strongly predicted by: gender, age, smoking status, average glucose level, BMI 



###predict on the testing dataset
mod.heartall.pred <- round(predict(mod.heartall, test, type='response')) #predict heart disease
heartall.actuals_preds <- data.frame(cbind(actuals=test$heart_disease, predicteds=mod.heartall.pred)) # make actuals_predicteds dataframe
#heartall.correlation_accuracy <- cor(heartall.actuals_preds) 
#heartall.correlation_accuracy #5.51% and is undesirable (a low value implies that the actuals and predicted values do not have similar directional movement, i.e. when the actuals values increase the predicted values do not also increase and vice-versa)
head(heartall.actuals_preds)
DMwR::regr.eval(heartall.actuals_preds$actuals, heartall.actuals_preds$predicteds) #thus mean squared error is 4.36% and is desirable (the lower the better)

table(test$heart_disease, mod.heartall.pred) #thus from confusion matrix, we made 785 errors, including 778 false negatives and 7 false positives
mean(test$heart_disease == mod.heartall.pred) #thus prediction accuracy is 95.64%









##heart disease vs all - attempt 2 with fewer predictors
###build the model using the training dataset
mod2.heartall<- glm(heart_disease ~ gender+age+smoking_status+avg_glucose_level+bmi, data = train, family = binomial)
summary(mod2.heartall)
#thus heart disease is predicted by (significant if p value < 0.05) (all predictors listed): gender, age, smoking status, average glucose level, BMI
anova(mod2.heartall, test="Chisq") #thus ANOVA gives the same finding
coef(mod2.heartall) #thus big difference between gender and the other predictors
unique(train$gender)
#thus probability (of having heart disease) will (based on the strength of the relationship):
#1) increase by 0.78 as gender increases by 1 (becoming male, instead of female)
#2) increase by 0.08 as age increases by 1 year
#3) increase by 0.26 as smoking status (formerly smoked) increases by 1
#4) decrease by 0.08 as smoking status (never smoked) increases by 1
#5) increase by 0.65 as smoking status (smokes) increases by 1
#6) increase by 0.01 as average glucose level increases by 1
#7) increase by 0.02 as BMI increases by 1

#other methods for evaluation show that:
#The null deviance show how well the response variable is predicted by the intercept and is of the value 14905.

#The residual deviance show how well the response variable is predicted by adding the independent variables and is of the value 11342.

#The AIC show how well the model is fitting the data and is of the value 11358. The value is undesirable as it is high.

#The number of Fisher Scoring iterations show that performing the fit needs 8 models. The value is desirable as it is low.


###check multicollinearity
library(car)
vif(mod2.heartall) #thus there is no collinearity: all variables have a value of VIF well below 5


###check outliers
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(mod2.heartall) #thus as shown in the Residuals vs Leverage plot, outliers don't exist. No cases are outside of the Cook's distance (meaning they have high Cook's distance scores) and influential to the regression results


###effect plot
library(effects)
mod2.heartall.plot <- allEffects(mod2.heartall)
plot(mod2.heartall.plot) #thus heart disease is strongly predicted by: gender, age, smoking status, average glucose level, BMI 



###predict on the testing dataset
mod2.heartall.pred <- round(predict(mod2.heartall, test, type='response')) #predict heart disease
heartall2.actuals_preds <- data.frame(cbind(actuals=test$heart_disease, predicteds=mod2.heartall.pred)) # make actuals_predicteds dataframe
head(heartall2.actuals_preds)
DMwR::regr.eval(heartall2.actuals_preds$actuals, heartall2.actuals_preds$predicteds) #thus mean squared error is 4.36% and is desirable (the lower the better)

table(test$heart_disease, mod2.heartall.pred) #thus from confusion matrix, we made 785 errors, including 780 false negatives and 5 false positives
mean(test$heart_disease == mod2.heartall.pred) #thus prediction accuracy is 95.64% (same as attemp 1)







##hypertension vs all - attempt 1
###build the model using the training dataset
mod.hypall<- glm(hypertension ~ gender+age+ever_married+work_type+residence_type+smoking_status+avg_glucose_level+bmi, data = train, family = binomial)
summary(mod.hypall) #thus hypertension is predicted by (significant if p value < 0.05): gender, age, marital status, work type, smoking status, average glucose level, BMI, AND NOT BY residence type
anova(mod.hypall, test="Chisq") #thus ANOVA gives the same finding
coef(mod.hypall) #thus not much differences among predictors
unique(train$gender)
unique(train$ever_married)
unique(train$residence_type)
#thus probability (of having hypertension) will (based on the strength of the relationship):
#1) increase by 0.19 as gender increases by 1 (becoming male, instead of female)
#2) increase by 0.05 as age increases by 1 year
#3) increase by 0.28 as marital status increases by 1 (becoming married, instead of single)
#4) increase by 2.36 as work type (government job) increases by 1
#5) increase by 1.45 as work type (never worked) increases by 1
#6) increase by 2.32 as work type (private) increases by 1
#7) increase by 2.34 as work type (self-employed) increases by 1
#8) decrease by 0.02 as residence type increases by 1 (becoming urban, instead of rural)
#9) increase by 0.54 as smoking status (formerly smoked) increases by 1
#10) increase by 0.69 as smoking status (never smoked) increases by 1
#11) increase by 0.74 as smoking status (smokes) increases by 1
#12) increase by 0.00 as average glucose level increases by 1
#13) increase by 0.05 as BMI increases by 1

#other methods for evaluation show that:
#The null deviance show how well the response variable is predicted by the intercept and is of the value 24888.

#The residual deviance show how well the response variable is predicted by adding the independent variables and is of the value 20505.

#The AIC show how well the model is fitting the data and is of the value 20533. The value is undesirable as it is high.

#The number of Fisher Scoring iterations show that performing the fit needs 10 models. The value is desirable as it is low.


###check multicollinearity
library(car)
vif(mod.hypall) #thus there is no collinearity: all variables have a value of VIF well below 5.


###check outliers
plot(mod.hypall) #thus as shown in the Residuals vs Leverage plot, outliers don't exist. No cases are outside of the Cook's distance (meaning they have high Cook's distance scores) and influential to the regression results


###effect plot
library(effects)
mod.hypall.plot <- allEffects(mod.hypall)
plot(mod.hypall.plot) #thus hypertension is strongly predicted by: gender, age, marital status, work type, smoking status, average glucose level, BMI 


###predict on the testing dataset
mod.hypall.pred <- round(predict(mod.hypall, test, type='response')) #predict hypertension
hypall.actuals_preds <- data.frame(cbind(actuals=test$hypertension, predicteds=mod.hypall.pred)) # make actuals_predicteds dataframe
#hypall.correlation_accuracy <- cor(hypall.actuals_preds) 
#hypall.correlation_accuracy #3.31% and is undesirable (a low value implies that the actuals and predicted values do not have similar directional movement, i.e. when the actuals values increase the predicted values do not also increase and vice-versa)
head(hypall.actuals_preds)
DMwR::regr.eval(hypall.actuals_preds$actuals, hypall.actuals_preds$predicteds) #thus mean squared error is 8.87% and is desirable (the lower the better)

table(test$hypertension, mod.hypall.pred) #thus from confusion matrix, we made 803 errors, including 772 false negatives and 31 false positives
mean(test$hypertension == mod.hypall.pred) #thus prediction accuracy is 91.13%








##hypertension vs all - attempt 2 with fewer predictors
###build the model using the training dataset
mod2.hypall<- glm(hypertension ~ gender+age+ever_married+work_type+smoking_status+avg_glucose_level+bmi, data = train, family = binomial)
summary(mod2.hypall) #thus hypertension is predicted by (significant if p value < 0.05): gender, age, marital status, work type, smoking status, average glucose level, BMI
anova(mod2.hypall, test="Chisq") #thus ANOVA gives the same finding
coef(mod2.hypall) #thus not much differences among predictors
unique(train$gender)
unique(train$ever_married)
#thus probability (of having hypertension) will (based on the strength of the relationship):
#1) increase by 0.19 as gender increases by 1 (becoming male, instead of female)
#2) increase by 0.05 as age increases by 1 year
#3) increase by 0.28 as marital status increases by 1 (becoming married, instead of single)
#4) increase by 2.36 as work type (government job) increases by 1
#5) increase by 1.45 as work type (never worked) increases by 1
#6) increase by 2.32 as work type (private) increases by 1
#7) increase by 2.34 as work type (self-employed) increases by 1
#8) increase by 0.54 as smoking status (formerly smoked) increases by 1
#9) increase by 0.69 as smoking status (never smoked) increases by 1
#10) increase by 0.74 as smoking status (smokes) increases by 1
#11) increase by 0.00 as average glucose level increases by 1
#12) increase by 0.05 as BMI increases by 1

#other methods for evaluation show that:
#The null deviance show how well the response variable is predicted by the intercept and is of the value 24888.

#The residual deviance show how well the response variable is predicted by adding the independent variables and is of the value 20505.

#The AIC show how well the model is fitting the data and is of the value 20531. The value is undesirable as it is high.

#The number of Fisher Scoring iterations show that performing the fit needs 10 models. The value is desirable as it is low.


###check multicollinearity
library(car)
vif(mod2.hypall) #thus there is no collinearity: all variables have a value of VIF well below 5.


###check outliers
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(mod2.hypall) #thus as shown in the Residuals vs Leverage plot, outliers don't exist. No cases are outside of the Cook's distance (meaning they have high Cook's distance scores) and influential to the regression results


###effect plot
library(effects)
mod2.hypall.plot <- allEffects(mod2.hypall)
plot(mod2.hypall.plot) #thus hypertension is strongly predicted by: gender, age, marital status, work type, smoking status, average glucose level, BMI 


###predict on the testing dataset
mod2.hypall.pred <- round(predict(mod2.hypall, test, type='response')) #predict hypertension
hypall2.actuals_preds <- data.frame(cbind(actuals=test$hypertension, predicteds=mod2.hypall.pred)) # make actuals_predicteds dataframe
#hypall2.correlation_accuracy <- cor(hypall2.actuals_preds) 
#hypall2.correlation_accuracy #3.31% and is undesirable (a low value implies that the actuals and predicted values do not have similar directional movement, i.e. when the actuals values increase the predicted values do not also increase and vice-versa)
head(hypall2.actuals_preds)
DMwR::regr.eval(hypall2.actuals_preds$actuals, hypall2.actuals_preds$predicteds) #thus mean squared error is 8.87% and is desirable (the lower the better)

table(test$hypertension, mod2.hypall.pred) #thus from confusion matrix, we made 803 errors, including 772 false negatives and 31 false positives
mean(test$hypertension == mod2.hypall.pred) #thus prediction accuracy is 91.13% (same as attemp 1)











##stroke vs all - attempt 1
###build the model using the training dataset
mod.strokeall<- glm(stroke ~ gender+age+ever_married+work_type+smoking_status+avg_glucose_level+bmi+hypertension, data = train, family = binomial)
summary(mod.strokeall) #thus hypertension is predicted by (significant if p value < 0.05): gender, age, smoking status, average glucose level, hypertension, AND NOT BY marital status, work type, BMI
anova(mod.strokeall, test="Chisq") #thus ANOVA gives the same finding
coef(mod.strokeall) #thus not much differences among predictors
unique(train$gender)
unique(train$ever_married)
unique(train$residence_type)
#thus probability (of having stroke) will (based on the strength of the relationship):
#1) increase by 0.14 as gender increases by 1 (becoming male, instead of female)
#2) increase by 0.08 as age increases by 1 year
#3) increase by 0.19 as smoking status (formerly smoked) increases by 1
#4) increase by 0.21 as smoking status (never smoked) increases by 1
#5) increase by 0.48 as smoking status (smokes) increases by 1
#6) increase by 0.00 as average glucose level increases by 1
#7) decrease by 0.01 as BMI increases by 1
#8) increase by 0.43 as hypertension increases by 1 (hypertension=yes)

#other methods for evaluation show that:
#The null deviance show how well the response variable is predicted by the intercept and is of the value 6648.5.

#The residual deviance show how well the response variable is predicted by adding the independent variables and is of the value 5459.4.

#The AIC show how well the model is fitting the data and is of the value 5487.4. The value is undesirable as it is high.

#The number of Fisher Scoring iterations show that performing the fit needs 15 models. The value is UNDESIRABLE as it is high.


###check multicollinearity
library(car)
vif(mod.strokeall) #thus there is no collinearity: all variables have a value of VIF well below 5.


###check outliers
plot(mod.strokeall) #thus as shown in the Residuals vs Leverage plot, outliers don't exist. No cases are outside of the Cook's distance (meaning they have high Cook's distance scores) and influential to the regression results


###effect plot
library(effects)
mod.strokeall.plot <- allEffects(mod.strokeall)
plot(mod.strokeall.plot) #thus stroke is strongly predicted by: age, average glucose level, BMI, hypertension


###predict on the testing dataset
#n/a as testing dataset does not contain the stroke variable







##stroke vs all - attempt 2 with fewer predictors
###build the model using the training dataset
mod2.strokeall<- glm(stroke ~ gender+age+smoking_status+avg_glucose_level+hypertension, data = train, family = binomial)
summary(mod2.strokeall) #thus stroke is predicted by (significant if p value < 0.05): age, smoking status, average glucose level, hypertension, AND NOT BY gender
anova(mod2.strokeall, test="Chisq") #thus ANOVA gives the same finding
coef(mod2.strokeall) #thus not much differences among predictors
unique(train$gender)
unique(train$ever_married)
unique(train$residence_type)
#thus probability (of having stroke) will (based on the strength of the relationship):
#1) increase by 0.08 as age increases by 1 year
#2) increase by 0.19 as smoking status (formerly smoked) increases by 1
#3) increase by 0.22 as smoking status (never smoked) increases by 1
#4) increase by 0.50 as smoking status (smokes) increases by 1
#5) increase by 0.00 as average glucose level increases by 1
#6) increase by 0.42 as hypertension increases by 1 (hypertension=yes)

#other methods for evaluation show that:
#The null deviance show how well the response variable is predicted by the intercept and is of the value 6648.5.

#The residual deviance show how well the response variable is predicted by adding the independent variables and is of the value 5461.9.

#The AIC show how well the model is fitting the data and is of the value 5477.9. The value is undesirable as it is high.

#The number of Fisher Scoring iterations show that performing the fit needs 8 models. The value is DESIRABLE as it is high.


###check multicollinearity
library(car)
vif(mod2.strokeall) #thus there is no collinearity: all variables have a value of VIF well below 5.


###check outliers
plot(mod2.strokeall) #thus as shown in the Residuals vs Leverage plot, outliers don't exist. No cases are outside of the Cook's distance (meaning they have high Cook's distance scores) and influential to the regression results


###effect plot
library(effects)
mod2.strokeall.plot <- allEffects(mod2.strokeall)
plot(mod2.strokeall.plot) #thus stroke is strongly predicted by: gender (BUT previously found insignificant), age, smoking status, average glucose level, hypertension


###predict on the testing dataset
#n/a as testing dataset does not contain the stroke variable