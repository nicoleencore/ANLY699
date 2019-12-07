install.packages("twitterR")
install.packages("wordcloud")
install.packages("tm")
install.packages("plyr")
install.packages("caret")
install.packages("psych")
install.packages("effects")
install.packages("car")
install.packages("doBy")
install.packages("multcomp")
install.packages("cluster")
install.packages("ggfortify")
install.packages("MASS")
install.packages("rrcov")
  
  

############

# TWITTER WORD CLOUD
library("twitteR")
library("wordcloud")
library("tm")
library("plyr")

consumer_key <-  'msmChN0bNpiMp0HovRZyeOCD7'
consumer_secret <- 'TsTYtHhb0p89CgIA4NKEYagy0kj2pU0ssY0VO80vdvgsOdVCh5'
access_token <- '3423530003-ec2kThW8iJ86n6xV25rAYPSdTAZtzAn1CmpEHuz'
access_secret <- 'plOPkkKMLgPPBw2uQgfgTfEhHFnhDn3M7jB5j7TdyGwfV'
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

num_tweets <- 100
r_stats <- searchTwitter("#heart disease", n=num_tweets)

r_stats_text <- sapply(r_stats, function(x) x$getText())
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))

r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower)) 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
wordcloud(r_stats_text_corpus)

#thus some of the most commonly used Twitter words about "heart disease" are as follows: protect, risk, chronic and health.





############

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

###################

#EDA
unique(train$work_type) #shows the levels in work type
unique(train$residence_type) #shows the levels in residence type
unique(train$smoking_status) #thus it is worth noticing that one level of response is empty
head(train) #thus formatting is normal in the beginning and no extra comment lines 
tail(train) #thus formatting is normal at the end and no extra comment lines

##summary statistics
summary(train) #median is robust to outliers

##variability#1: standard deviation (SD). the variance is an average of the squared deviations, and the standard deviation is the square root of the variance; not robust to outliers
sd(train[["age"]]) 
sd(train[["avg_glucose_level"]]) 
sd(train[["bmi"]]) 

##variability#2: interquartile range (IQR). the difference between the 75th percentile and the 25th percentile; robust to outliers as it measures the range of the data after dropping values from each end
IQR(train[["age"]]) 
IQR(train[["avg_glucose_level"]]) 
IQR(train[["bmi"]])

##variability#3: median absolute deviation from the median (MAD). the mean of the absolute value of the deviations from the mean; robust to outliers
mad(train[["age"]]) 
mad(train[["avg_glucose_level"]]) 
mad(train[["bmi"]])


##bar chart for nominal var
par(mfrow = c(3, 2))
nomgender=table(train$gender)
barplot(nomgender, xlab="Gender")

nommarital=table(train$ever_married)
barplot(nommarital, xlab="Marital Status")

nomwork=table(train$work_type)
barplot(nomwork, xlab="Work Type")

nomresidence=table(train$residence_type)
barplot(nomresidence, xlab="Residence Type")

nomsmoke=table(train$smoking_status)
barplot(nomsmoke, xlab="Smoking Status")


##histogram for interval and ratio var
par(mfrow = c(3, 3))
hist(train$age) #thus symmetrical
hist(train$avg_glucose_level) #thus +vely skewed where mode<median<mean
hist(train$bmi) #thus +vely skewed where mode<median<mean

library(moments)
skewness(train$age)
kurtosis(train$age)
#A skewness value of -0.08 shows that age is symmetrical since a skewness value of absolute value below 0.5 is fairly symmetrical. 
#A kurtosis value of 2.00 shows that age has a leptokurtic (peaked) distribution since the value is positive (if negative, platykurtic or flat distribution).

skewness(train$bmi)
kurtosis(train$bmi)
#A skewness value of -0.90 shows that bmi is not symmetrical since a skewness value of absolute value above 0.5 is not symmetrical. 
#A kurtosis value of 4.99 shows that bmi has a leptokurtic (peaked) distribution since the value is positive (if negative, platykurtic or flat distribution).

skewness(train$avg_glucose_level)
kurtosis(train$avg_glucose_level)
#A skewness value of 1.72 shows that avg_glucose_level is not symmetrical since a skewness value of absolute value above 0.5 is not symmetrical. 
#A kurtosis value of 5.42 shows that avg_glucose_level has a leptokurtic (peaked) distribution since the value is positive (if negative, platykurtic or flat distribution).

#############

#################
# LOAD PACKAGES AND DATA #
#################

library(foreign)
library(car)
library(effects)
library(doBy) 
library(multcomp)
library(ggfortify)
library(cluster)

train.cluster=as.data.frame(lapply(train, as.numeric)) #converted all non-numeric variables to numeric
str(train.cluster)

###################################
# FIND OPTIMAL NUMBER OF CLUSTERS #
###################################

# use the gap statistic with bootstrapping to determine the optimal number of clusters (using k-means as the function [i.e., FUN = kmeans], but also consider pam [i.e., FUN = pam]); the argument "B" sets the number of bootstrapping iterations to run, where higher is generally better, although time can be an issue; "K.max" is the maximum number of clusters to consider
library(cluster)
plot(clusGap(train.cluster, FUN = kmeans, K.max = 7, B = 3)) #thus optimal number of clusters is 4

###################################
# CONDUCT K-MEANS CLUSTER ANALYSIS #
###################################

# conduct a k-means clustering with our selected number of clusters
chico.cluster <- kmeans(train.cluster, centers = 4)
names(chico.cluster) # see what we can pull out

table(chico.cluster$cluster) # this is a table of the class membership predicted by the cluster analysis; thus undesirable as observations are not equally distributed across clusters

chico.cluster$centers # see the cluster center means

####################
# FOLLOW-UP ANOVAS #
####################

# run one-way ANOVAs and generate descriptives based on selected predictors:

# create a new data.frame that includes the original data and the cluster class results:
dat.chico.cluster <- cbind(cluster = factor(chico.cluster$cluster), train.cluster)

#gender:
# build model
mod.cluster.gender <- lm(gender ~ cluster, data = dat.chico.cluster)

Anova(mod.cluster.gender)
summary(mod.cluster.gender) 

# look at pairwise comparisons:
library(multcomp)
summary(glht(mod.cluster.gender, linfct = mcp(cluster = "Tukey")), test = adjusted(type = "none")) #thus the means are MOSTLY significantly different across clusters


#age:
# build model
mod.cluster.age <- lm(age ~ cluster, data = dat.chico.cluster)

Anova(mod.cluster.age)
summary(mod.cluster.age) 

# look at pairwise comparisons:
library(multcomp)
summary(glht(mod.cluster.age, linfct = mcp(cluster = "Tukey")), test = adjusted(type = "none")) #thus the means are significantly different across clusters


#marital status:
# build model
mod.cluster.marital <- lm(ever_married ~ cluster, data = dat.chico.cluster)

Anova(mod.cluster.marital)
summary(mod.cluster.marital) 

# look at pairwise comparisons:
library(multcomp)
summary(glht(mod.cluster.marital, linfct = mcp(cluster = "Tukey")), test = adjusted(type = "none")) #thus the means are significantly different across clusters


#work type:
# build model
mod.cluster.work <- lm(work_type ~ cluster, data = dat.chico.cluster)

Anova(mod.cluster.work)
summary(mod.cluster.work) 

# look at pairwise comparisons:
library(multcomp)
summary(glht(mod.cluster.work, linfct = mcp(cluster = "Tukey")), test = adjusted(type = "none")) #thus the means are significantly different across clusters


#residence type:
# build model
mod.cluster.residence <- lm(residence_type ~ cluster, data = dat.chico.cluster)

Anova(mod.cluster.residence)
summary(mod.cluster.residence) 

# look at pairwise comparisons:
library(multcomp)
summary(glht(mod.cluster.residence, linfct = mcp(cluster = "Tukey")), test = adjusted(type = "none")) #thus the means are NOT significantly different across clusters


#smoking status:
# build model
mod.cluster.smoking <- lm(smoking_status ~ cluster, data = dat.chico.cluster)

Anova(mod.cluster.smoking)
summary(mod.cluster.smoking) 

# look at pairwise comparisons:
library(multcomp)
summary(glht(mod.cluster.smoking, linfct = mcp(cluster = "Tukey")), test = adjusted(type = "none")) #thus the means are significantly different across clusters


#average glucose level:
# build model
mod.cluster.glucose <- lm(avg_glucose_level ~ cluster, data = dat.chico.cluster)

Anova(mod.cluster.glucose)
summary(mod.cluster.glucose) 

# look at pairwise comparisons:
library(multcomp)
summary(glht(mod.cluster.glucose, linfct = mcp(cluster = "Tukey")), test = adjusted(type = "none")) #thus the means are significantly different across clusters


#BMI:
# build model
mod.cluster.bmi <- lm(bmi ~ cluster, data = dat.chico.cluster)

Anova(mod.cluster.bmi)
summary(mod.cluster.bmi) 

# look at pairwise comparisons:
library(multcomp)
summary(glht(mod.cluster.bmi, linfct = mcp(cluster = "Tukey")), test = adjusted(type = "none")) #thus the means are significantly different across clusters


#stroke:
# build model
mod.cluster.stroke <- lm(stroke ~ cluster, data = dat.chico.cluster)

Anova(mod.cluster.stroke)
summary(mod.cluster.stroke) 

# look at pairwise comparisons:
library(multcomp)
summary(glht(mod.cluster.stroke, linfct = mcp(cluster = "Tukey")), test = adjusted(type = "none")) #thus the means are significantly different across clusters


#heart disease:
# build model
mod.cluster.heart <- lm(bmi ~ heart_disease, data = dat.chico.cluster)

Anova(mod.cluster.heart)
summary(mod.cluster.heart) #thus the means are significantly different across clusters


#hypertension:
# build model
mod.cluster.hyp <- lm(bmi ~ hypertension, data = dat.chico.cluster)

Anova(mod.cluster.hyp)
summary(mod.cluster.hyp) #thus the means are significantly different across clusters



#########################
# CLUSTER VISUALIZATION #
#########################
  
library(ggfortify)

# use chico.cluster, which had our k-means solution generated above:
clusterplot <- autoplot(chico.cluster, data = train.cluster, frame = TRUE)
clusterplot

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


# compute the Wilks' lambda statistics:
library(rrcov)
wilks <- summary(manova(cbind(train.lda.heart$age, train.lda.heart$avg_glucose_level, train.lda.heart$bmi) ~ heart_disease, data = train.lda.heart), test="Wilks") # this conducts a manova and produces Wilks' lambda for the overall model using the actual manova function

wilks$stats[1,2] #The wilks' lambda shows the percent variance in dependent variables not explained by differences in levels of the independent variable. The value is undesirable as 0.93 was close to 1, i.e., the function does not explain the group membership well.




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
#Group means: group center of gravity. For example, the mean of the age variable in the hypertension=no group is 40.68.
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

