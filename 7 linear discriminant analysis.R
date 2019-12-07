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

###############
# CONDUCT LDA #
###############
##heart disease vs ratio/interval##

# load the data
train.lda.heart <- train[, c("age", "avg_glucose_level", "bmi", "heart_disease")]
str(train.lda.heart)


#build the model using the training set
library(MASS)
mod.lda.heart <- lda(heart_disease ~ ., data = train.lda.heart, prior = c(1,1)/2) 
mod.lda.heart
#thus:
#Prior probabilities of groups: the proportion of training observations in each group. For example, there are 50% of the training observations in the heart disease=no group
#Group means: group center of gravity. For example, the mean of the age variable in the heart disease=no group is 40.68.
#Coefficients of linear discriminants Shows the linear combination of predictor variables that are used to form the LDA decision rule.
  #LD1 = 0.04*age + 0.01*avg_glucose_level - 0.02*bmi.

# for this two class problem, plot the LDA results as stacked histograms; in this case, a two class problem will only have a single dimension, i.e., LD1 in the output:
plot(mod.lda.heart, dimen = 1, type = "both", col = as.numeric(train.lda.heart$heart_disease) + 1)

# predict on the testing dataset
mod.lda.heart.predict <- predict(mod.lda.heart)
prediction.table.heart <- data.frame(actual = train.lda.heart$heart_disease, predicted = mod.lda.heart.predict$class)
head(prediction.table.heart)

confusion.heart <- xtabs(~ actual + predicted, prediction.table.heart)
sum(diag(confusion.heart))/sum(confusion.heart)*100 #thus prediction accuracy is 74.30% 


# compute the Wilks' lambda statistics using any of the following methods (note: lower Wilks' lambda values are better):
library(rrcov)
wilks <- summary(manova(cbind(train.lda.heart$age, train.lda.heart$avg_glucose_level, train.lda.heart$bmi) ~ heart_disease, data = train.lda.heart), test="Wilks") # this conducts a manova and produces Wilks' lambda for the overall model using the actual manova function

wilks$stats[1,2] #The wilks' lambda shows the percent variance in dependent variables not explained by differences in levels of the independent variable. The value is undesirable as 0.93 close to 1, i.e., the function does not explain the group membership well.




##hypertension vs ratio/interval##

# load the data
train.lda.hyp <- train[, c("age", "avg_glucose_level", "bmi", "hypertension")]
str(train.lda.hyp)


#build the model using the training set
library(MASS)
mod.lda.hyp <- lda(hypertension ~ ., data = train.lda.hyp, prior = c(1,1)/2) 
mod.lda.hyp
#thus:
#Prior probabilities of groups: the proportion of training observations in each group. For example, there are 50% of the training observations in the heart disease=no group
#Group means: group center of gravity. For example, the mean of the age variable in the heart disease=no group is 40.68.
#Coefficients of linear discriminants Shows the linear combination of predictor variables that are used to form the LDA decision rule.
#LD1 = 0.04*age + 0.01*avg_glucose_level - 0.02*bmi.

# for this two class problem, plot the LDA results as stacked histograms; in this case, a two class problem will only have a single dimension, i.e., LD1 in the output:
plot(mod.lda.hyp, dimen = 1, type = "both", col = as.numeric(train.lda.hyp$hypertension) + 1)

# predict on the testing dataset
mod.lda.hyp.predict <- predict(mod.lda.hyp)
prediction.table.hyp <- data.frame(actual = train.lda.hyp$hypertension, predicted = mod.lda.hyp.predict$class)
head(prediction.table.hyp)

confusion.hyp <- xtabs(~ actual + predicted, prediction.table.hyp)
sum(diag(confusion.hyp))/sum(confusion.hyp)*100 #thus prediction accuracy is 69.70% 


# compute the Wilks' lambda statistics using any of the following methods (note: lower Wilks' lambda values are better):
library(rrcov)
wilks <- summary(manova(cbind(train.lda.heart$age, train.lda.heart$avg_glucose_level, train.lda.heart$bmi) ~ heart_disease, data = train.lda.heart), test="Wilks") # this conducts a manova and produces Wilks' lambda for the overall model using the actual manova function

wilks$stats[1,2] #The wilks' lambda shows the percent variance in dependent variables not explained by differences in levels of the independent variable. The value is undesirable as 0.93 close to 1, i.e., the function does not explain the group membership well.







##stroke vs ratio/interval##

# load the data
train.lda.stroke <- train[, c("age", "avg_glucose_level", "bmi", "stroke")]
str(train.lda.stroke)


#build the model using the training set
library(MASS)
mod.lda.stroke <- lda(stroke ~ ., data = train.lda.stroke, prior = c(1,1)/2) 
mod.lda.stroke
#thus:
#Prior probabilities of groups: the proportion of training observations in each group. For example, there are 50% of the training observations in the stroke=no group
#Group means: group center of gravity. For example, the mean of the age variable in the stroke=no group is 41.43.
#Coefficients of linear discriminants Shows the linear combination of predictor variables that are used to form the LDA decision rule.
#LD1 = 0.04*age + 0.01*avg_glucose_level - 0.04*bmi.

# for this two class problem, plot the LDA results as stacked histograms; in this case, a two class problem will only have a single dimension, i.e., LD1 in the output:
plot(mod.lda.stroke, dimen = 1, type = "both", col = as.numeric(train.lda.stroke$stroke) + 1)

# predict on the testing dataset
#n/a as testing dataset does not contain the stroke variable

# compute the Wilks' lambda statistics using any of the following methods (note: lower Wilks' lambda values are better):
library(rrcov)
wilks <- summary(manova(cbind(train.lda.stroke$age, train.lda.stroke$avg_glucose_level, train.lda.stroke$bmi) ~ stroke, data = train.lda.stroke), test="Wilks") # this conducts a manova and produces Wilks' lambda for the overall model using the actual manova function

wilks$stats[1,2] #The wilks' lambda shows the percent variance in dependent variables not explained by differences in levels of the independent variable. The value is undesirable as 0.97 close to 1, i.e., the function does not explain the group membership well.

