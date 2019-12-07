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

#MULTIPLE LINEAR REGRESSION: bmi vs rest 7 - attempt 1
##build model using the training dataset
mod.bmirest = lm(bmi ~ gender+age+ever_married+work_type+residence_type+smoking_status+avg_glucose_level, data=train)
summary(mod.bmirest) #thus BMI predicted by (significant if p value < 0.05): gender, marital status, work type, smoking status, average glucose level, AND NOT BY age/residence type
coef(mod.bmirest) 
#thus the strength of the relationship showed that BMI will:
#1) increase by 0.22 as gender increases by 1 (becoming male, instead of female) 
#2) increase by 0.00 as age increases by 1 year
#3) increase by 1.69 as marital status increases by 1 (becoming married, instead of single)
#4) increase by 8.43 as work type (government job) increases by 1
#5) increase by 5.67 as work type (never worked) increases by 1
#6) increase by 8.10 as work type (private) increases by 1
#7) increase by 7.72 as work type (self-employed) increases by 1
#8) decrease by 0.07 as residence type increases by 1 (becoming urban, instead of rural)
#9) increase by 0.98 as smoking status (formerly smoked) increases by 1
#10) increase by 0.60 as smoking status (never smoked) increases by 1
#11) increase by 0.35 as smoking status (smokes) increases by 1
#12) increase by 0.02 as average glucose level increases by 1


#other methods for evaluation show that:
#The standard error values (0.07, 0.10, 0.17, 0.52, 0.14, 0.17, 0.11, 0.10, 0.11, 0.00) show the standard deviation of an estimate. The values are desirable as they are low.

#The t values (3.21, 17.36, 51.00, 10.93, 58.97, 45.30, 8.75, 6.51, 3.08, 29.60) show how many standard deviations the coefficient estimate is far away from 0. The values are desirable as they are mosty high (i.e., rejecting the null hypothesis).

#The residual standard error value (6.76) is a measure of the quality of the linear regression fit, and shows an average distance that the response deviates from true regression line. The value is desirable as the model only has 6.76 off as residual.

#The adjusted R-squared value (0.24) is a measure of fit (how well the model is fitting the data), adjusted based on the number of variables. The value is undesirable as it is not strong (only 24% of the variance in the response variable distance can be predicted by the independent variables).

#The F-statistic value (1119) is an indicator of relationship between predictors and response. The value is desirable as 1119 is much larger than 1.



##check multicollinearity
library(car)
vif(mod.bmirest) #thus there is no collinearity: all variables have a value of VIF well below 5



##check linear relationship, the normal distribution, the equal variance or homoscedasticity, outliers
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(mod.bmirest) 
#thus by looking at the diagnostic plots of the multiple linear regression model:

#As shown in the Residuals vs Fitted graph, there is a linear relationship between the predictor variables (gender, age, marital status, work type, residence type, smoking status, and average glucose level) and the outcome variable (BMI). The residuals are equally spread around a horizontal line without distinct patterns.

#As shown in the Normal Q-Q plot, the residuals are normally distributed as the residuals are mostly lined well on the straight dashed line.

#As shown in the Scale-Location plot, the residuals are spread equally along the ranges of the predictors. Equal variance (homoscedasticity) is true, because there is a horizontal line with equally (randomly) spread points.

#As shown in the Residuals vs Leverage plot, outliers don't exist. No cases are outside of the Cook's distance (meaning they have high Cook's distance scores) and influential to the regression results.



##predict on the testing dataset:
mod.bmirest.pred <- round(predict(mod.bmirest, test, type='response')) #predict BMI
bmirest.actuals_preds <- data.frame(cbind(actuals=test$bmi, predicteds=mod.bmirest.pred)) # make actuals_predicteds dataframe
bmirest.correlation_accuracy <- cor(bmirest.actuals_preds) 
bmirest.correlation_accuracy #49.85% prediction accuracy is achieved and is not very desirable (a low value implies that the actuals and predicted values do not have similar directional movement, i.e. when the actuals values increase the predicted values do not also increase and vice-versa)
head(bmirest.actuals_preds)
mape <- mean(abs((bmirest.actuals_preds$predicteds - bmirest.actuals_preds$actuals))/bmirest.actuals_preds$actuals)
mape #thus mean absolute percentage deviation is 17.99%
DMwR::regr.eval(bmirest.actuals_preds$actuals, bmirest.actuals_preds$predicteds) #thus mean squared error (MSE) is 45.22 and is desirable (the lower the better), and mean absolute percentage error (MAPE) is 17.99% and is desirable (the lower the better)









#MULTIPLE LINEAR REGRESSION: bmi vs rest 7 - attempt 2 with fewer predictors
##build model using the training dataset
mod2.bmirest = lm(bmi ~ gender+ever_married+work_type+smoking_status+avg_glucose_level, data=train)
summary(mod2.bmirest) #thus BMI is predicted by (significant if p value < 0.05) (all predictors listed): gender, marital status, work type, smoking status, average glucose level
coef(mod2.bmirest) 
#thus the strength of the relationship showed that BMI will:
#1) increase by 0.22 as gender increases by 1 (becoming male, instead of female) 
#2) increase by 1.74 as marital status increases by 1 (becoming married, instead of single)
#3) increase by 8.48 as work type (government job) increases by 1
#4) increase by 5.69 as work type (never worked) increases by 1
#5) increase by 8.15 as work type (private) increases by 1
#6) increase by 7.80 as work type (self-employed) increases by 1
#7) increase by 0.99 as smoking status (formerly smoked) increases by 1
#8) increase by 0.60 as smoking status (never smoked) increases by 1
#9) increase by 0.35 as smoking status (smokes) increases by 1
#10) increase by 0.02 as average glucose level increases by 1


#other methods for evaluation show that:
#The standard error values (0, 0, 0, 0, 0, 0, 0, 0, 0, 0) show the standard deviation of an estimate. The values are desirable as they are low.

#The t values (3.23, 20.61, 54.74, 10.98, 64.35, 51.48, 8.93, 6.52, 3.06, 30.21) show how many standard deviations the coefficient estimate is far away from 0. The values are desirable as they are all high (i.e., rejecting the null hypothesis).

#The residual standard error value (6.76) is a measure of the quality of the linear regression fit, and shows an average distance that the response deviates from true regression line. The value is desirable as the model only has 6.76 off as residual.

#The adjusted R-squared value (0.24) is a measure of fit (how well the model is fitting the data), adjusted based on the number of variables. The value is undesirable as it is not strong (only 24% of the variance in the response variable distance can be predicted by the independent variables).

#The F-statistic value (1342) is an indicator of relationship between predictors and response. The value is desirable as 1342 is much larger than 1.




##check multicollinearity
library(car)
vif(mod2.bmirest) #thus there is no collinearity: all variables have a value of VIF well below 5



##check linear relationship, the normal distribution, the equal variance or homoscedasticity, outliers
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(mod2.bmirest) 
#thus by looking at the diagnostic plots of the multiple linear regression model:

#As shown in the Residuals vs Fitted graph, there is a linear relationship between the predictor variables (gender, marital status, work type, smoking status, and average glucose level) and the outcome variable (BMI). The residuals are equally spread around a horizontal line without distinct patterns.

#As shown in the Normal Q-Q plot, the residuals are normally distributed as the residuals are mostly lined well on the straight dashed line.

#As shown in the Scale-Location plot, the residuals are spread equally along the ranges of the predictors. Equal variance (homoscedasticity) is true, because there is a horizontal line with equally (randomly) spread points.

#As shown in the Residuals vs Leverage plot, outliers don't exist. No cases are outside of the Cook's distance (meaning they have high Cook's distance scores) and influential to the regression results.



##predict on the testing dataset:
mod2.bmirest.pred <- round(predict(mod2.bmirest, test, type='response')) #predict BMI
bmirest2.actuals_preds <- data.frame(cbind(actuals=test$bmi, predicteds=mod2.bmirest.pred)) # make actuals_predicteds dataframe
bmirest2.correlation_accuracy <- cor(bmirest2.actuals_preds) 
bmirest2.correlation_accuracy #49.83% prediction accuracy is achieved and is not very desirable (a low value implies that the actuals and predicted values do not have similar directional movement, i.e. when the actuals values increase the predicted values do not also increase and vice-versa)
head(bmirest2.actuals_preds)
mape2 <- mean(abs((bmirest2.actuals_preds$predicteds - bmirest2.actuals_preds$actuals))/bmirest2.actuals_preds$actuals)
mape2 #thus mean absolute percentage deviation is 17.99%
DMwR::regr.eval(bmirest2.actuals_preds$actuals, bmirest2.actuals_preds$predicteds) #thus mean squared error (MSE) is 45.23 and is desirable (the lower the better), and mean absolute percentage error (MAPE) is 17.99% and is desirable (the lower the better)


