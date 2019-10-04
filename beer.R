#Author: David Weinflash
#Assignment: Week 9 - Apply what you've learned
#Date: 7/30/2019
#Class: ISTA 321 - Summer 2019

# Packages and data
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
beer <- read_csv("https://docs.google.com/spreadsheets/d/1FQvlCVdeGiMttYgBCsUXge3P_KfL8AmI12cvSXxkPuU/gviz/tq?tqx=out:csv")

## Pre processing

# take a high level look at the beer file
glimpse(beer)
summary(beer)

# Replace NAs in abv with median abv
beer$abv[is.na(beer$abv)] <- 6.425

# Remove all other NAs
beer <- na.omit(beer)

# confirm the beer file includes international beer
summary(factor(beer$brewery_country))

# get df with all domestic beer companies
beer_us <- beer %>%
  filter(brewery_country == 'United States')

# confirm only US cities in beer_us
summary(factor(beer_us$brewery_city))


## Which states have the best beer?

# mean beer score per state
states_scores <- aggregate(beer_us[, 6], list(beer_us$brewery_city), mean)
colnames(states_scores) <- c("state", "score")

# plot the mean score per brewery_city
p1 <- ggplot(states_scores, aes(x= state, y= score, label=state))+
  geom_point() + geom_text(aes(label=state),hjust=0, vjust=0) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



## Additional features to beer ratings

# create an age column
beer_us$age <- 2019 - beer_us$year

# regions for states
west <- c('California', 'Oregon', 'Washington', 'Alaska', 'Hawaii', 'Idaho',
          'Montana', 'Wyoming', 'Nevada', 'Utah', 'Colorado', 'Arizona', 'New Mexico')
midwest <- c('Illinois', 'Indiana', 'Iowa', 'Kansas', 'Michigan', 'Minnesota', 'Missouri', 
             'Nebraska', 'North Dakota', 'Ohio', 'South Dakota', 'Wisconsin')
south <- c('District of Columbia','Texas', 'Oklahoma', 'Louisiana', 'Arkansas', 'Mississippi', 'Alabama', 'Tennessee', 
           'Kentucky', 'Georgia', 'Florida', 'South Carolina', 'North Carolina', 'West Virginia')
east <- c('Maine', 'New Hampshire', 'Massachusetts', 'Rhode Island', 'Connecticut', 'New York', 
          'New Jersey', 'Delaware', 'Maryland', 'Virginia', 'Vermont', 'Pennsylvania')

# one hot encode each beer based on US region
beer_us$west <- ifelse(beer_us$brewery_city %in% west,1,0)
beer_us$east <- ifelse(beer_us$brewery_city %in% east,1,0)
beer_us$midwest <- ifelse(beer_us$brewery_city %in% midwest,1,0)
beer_us$south <- ifelse(beer_us$brewery_city %in% south,1,0)

# confirm each beer in a US region
# Observations = Sum(Region Flags)
glimpse(beer_us)
sum(beer_us[,c(12:15)])



## Use multiple linear regression to predict beer score

# error df
mlr_df <- data.frame(matrix(ncol = 2, nrow = 10))
colnames(mlr_df) <- c('k', 'mse')


for(i in 1:10) {
  
  # training and test data
  split_index <- createDataPartition(beer_us$beer_overall_score, p = 0.8, list = F) 
  training <- beer_us[split_index,]
  features_test <- beer_us[-split_index, !(colnames(beer_us) %in% c('beer_overall_score'))]
  target_test <- beer_us[-split_index, 'beer_overall_score']
  
  # fit model
  beer_mlr <- lm(beer_overall_score ~ style + abv + year + age + west + east + midwest + south, data=training)
  
  # predict
  mlr_preds <- predict(beer_mlr, newdata = features_test)
  mlr_mse <- mean((mlr_preds - target_test$beer_overall_score)^2)
  
  # store in error df
  mlr_df[i, 'k'] <- i
  mlr_df[i, 'mse']  <- sqrt(mlr_mse)
}

# plot error rates
ggplot(mlr_df, aes(x=k, y=mse)) + geom_line()

# My MLR model is pretty awful with a poor goodness of fit (R2 ~ 0.14)
# On average it's prediction is off by almost a full point
# Beer style appears to be an important factor based on the small p-values
# It's interesting to note that the coefficient for 'west' is negative
summary(beer_mlr)