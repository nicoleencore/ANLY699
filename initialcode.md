# ANLY699
train=read.csv('C:/Users/Nicole Guo/Desktop/H1B MATERIAL/CPT/HU/6-ANLY 699/project/kaggle healthcare-data/train_2v.csv')
str(train)
summary(train)
sum(is.na(train)==T)
train <- na.omit(train)
str(train)

test=read.csv('C:/Users/Nicole Guo/Desktop/H1B MATERIAL/CPT/HU/6-ANLY 699/project/kaggle healthcare-data/test_2v.csv')
str(test)
summary(test)
sum(is.na(test)==T)
test <- na.omit(test)
str(test)