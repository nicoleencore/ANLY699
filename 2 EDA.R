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

