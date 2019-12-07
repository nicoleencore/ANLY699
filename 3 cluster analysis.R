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
