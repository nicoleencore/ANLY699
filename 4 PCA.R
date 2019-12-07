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






##############

#PCA: bmi's rest 7 - attempt 1
library(caret)

dummies<- data.frame(sapply(train, function(x) data.frame(model.matrix(~x-1,data =train))[,-1])) #turned into dummy var for gender + ever_married + work_type + residence_type + smoking_status
str(dummies)

age=subset(train, select=2)
glucose=subset(train, select=8)

bmialliv <- cbind(dummies, age, glucose)
str(bmialliv)

par(mfrow = c(1, 1))
library(psych)
ev.bmi <- eigen(cor(bmialliv)) # compute eigen values
#VSS.scree(bmialliv) # simple scree plot showing number of principal components or factors
#fa.parallel(bmialliv) # more complex scree plot method showing number of principal components or factors


#fit.principal <- principal(nfactors = 6, r = bmialliv, rotate = "varimax") #conduct a principal components analysis (PCA) with varimax rotation 
#fit.principal #shows factor loadings
#summary(fit.principal)
#fa.diagram(fit.principal)

#attempt 1 did not work due to error







#PCA: bmi's rest 7 - attempt 2 with the Level "Other" in Gender variable 
train.1=read.csv('C:/Users/Nicole Guo/Desktop/H1B MATERIAL/CPT/HU/6-ANLY 699/project/kaggle healthcare-data/train_2v.csv')
str(train.1)

sum(is.na(train.1)==T)
train.1=na.omit(train.1) # Removed Rows With Missing Values (e.g., Gender=="Other" to make geneder-related results easier to interpret)
train.1=train.1[,-1]
names(train.1)[7] <- "residence_type"
str(train.1) #thus missing value rows removed, column renamed, and the number of observations is big enough



library(caret)

dummies.1<- data.frame(sapply(train.1, function(x) data.frame(model.matrix(~x-1,data =train.1))[,-1])) #turned into dummy var for gender + ever_married + work_type + residence_type + smoking_status
str(dummies.1)

age.1=subset(train.1, select=2)
glucose.1=subset(train.1, select=8)

bmialliv.1 <- cbind(dummies.1, age.1, glucose.1)
str(bmialliv.1)

par(mfrow = c(1, 1))
library(psych)
ev.bmi.1 <- eigen(cor(bmialliv.1)) # compute eigen values
VSS.scree(bmialliv.1) # simple scree plot showing number of principal components or factors
fa.parallel(bmialliv.1) # more complex scree plot showing number of principal components or factors


fit.principal.bmi.1 <- principal(nfactors = 6, r = bmialliv.1, rotate = "varimax") #conduct a principal components analysis (PCA) with varimax rotation
fit.principal.bmi.1 #shows factor loadings
summary(fit.principal.bmi.1)
fa.diagram(fit.principal.bmi.1)

#thus:
#Component 1: age, marital status, average glucose level
#Component 2: work type (self employed)
#Component 3: smoking status (formerly smoked)
#Component 4: smoking status (smokes)
#Component 5: work type (government job)
#Component 6: gender, residence type, work type (never worked)

#Thus, I decided to use all 7 variables in all components, because only three variables were excluded from the above categorization, including work type (private), smoking status (never smoked), and gender (other).







#PCA: stroke's rest 9 - attempt 1
library(caret)

dummies<- data.frame(sapply(train, function(x) data.frame(model.matrix(~x-1,data =train))[,-1])) #turned into dummy var for gender + ever_married + work_type + residence_type + smoking_status
str(dummies)

age=subset(train, select=2)
glucose=subset(train, select=8)
bmi=subset(train.1, select=9)
hypertension=subset(train, select=3)

strokealliv <- cbind(dummies, age, glucose, bmi, hypertension)
str(strokealliv)

par(mfrow = c(1, 1))
library(psych)
ev.stroke <- eigen(cor(strokealliv)) # compute eigen values
#VSS.scree(strokealliv) # simple scree plot showing number of principal components or factors
#fa.parallel(strokealliv) # more complex scree plot method showing number of principal components or factors


#fit.principal <- principal(nfactors = 6, r = strokealliv, rotate = "varimax") #conduct a principal components analysis (PCA) with varimax rotation
#fit.principal #shows factor loadings
#summary(fit.principal)
#fa.diagram(fit.principal)
#attempt 1 did not work due to error






#PCA: stroke's rest 9 - attempt 2 with the Level "Other" in Gender variable 
train.1=read.csv('C:/Users/Nicole Guo/Desktop/H1B MATERIAL/CPT/HU/6-ANLY 699/project/kaggle healthcare-data/train_2v.csv')
str(train.1)

sum(is.na(train.1)==T)
train.1=na.omit(train.1) # Removed Rows With Missing Values (e.g., Gender=="Other" to make geneder-related results easier to interpret)
train.1=train.1[,-1]
names(train.1)[7] <- "residence_type"
str(train.1) #thus missing value rows removed, column renamed, and the number of observations is big enough



library(caret)

dummies.1<- data.frame(sapply(train.1, function(x) data.frame(model.matrix(~x-1,data =train.1))[,-1])) #turned into dummy var for gender + ever_married + work_type + residence_type + smoking_status
str(dummies.1)

age.1=subset(train.1, select=2)
glucose.1=subset(train.1, select=8)
bmi=subset(train.1, select=9)
hypertension.1=subset(train.1, select=3)

strokealliv.1 <- cbind(dummies.1, age.1, glucose.1, bmi, hypertension.1)
str(strokealliv.1)

par(mfrow = c(1, 1))
library(psych)
ev.stroke.1 <- eigen(cor(strokealliv.1)) # compute eigen values
VSS.scree(strokealliv.1) # simple scree plot showing number of principal components or factors
fa.parallel(strokealliv.1) # more complex scree plot method showing number of principal components or factors


fit.principal.stroke.1 <- principal(nfactors = 6, r = strokealliv.1, rotate = "varimax") #conduct a principal components analysis (PCA) with varimax rotation
fit.principal.stroke.1 #shows factor loadings
summary(fit.principal.stroke.1)
fa.diagram(fit.principal.stroke.1)

#thus:
#Component 1: age, marital status, bmi, hypertension
#Component 2: work type (self employed)
#Component 3: smoking status (formerly smoked)
#Component 4: smoking status (smokes)
#Component 5: work type (government job)
#Component 6: gender, average glucose level, work type (never worked)

#Thus, I decided to use all 9 variables except for residence type in all components, because only three variables were excluded from the above categorization including work type (private), smoking status (never smoked), and gender (other), but residence_type did not constitute any principal component. In other words, the variables used for following analyses include: gender, age, marital status, work type, smoking status, glucose level, BMI, and hypertension.
