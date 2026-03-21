## BUILDING HOUSE PREDICTION MODEL USING LINEAR REGRESSION

# Download the raw data from kaggle using the URL
# https://www.kaggle.com/datasets/iamsouravbanerjee/house-rent-prediction-dataset?select=House_Rent_Dataset.csv

# loading external packages
install.packages("tidyverse") # install the packages if you don't have it already installed
install.packages("car")
library(tidyverse) # loading the tidyverse package
library(car) # loading the car package

# importing the dataset
house_rent <- read_csv(choose.files()) # locate the dataset in its directory and import

# This analyses involves three stages:
# 1. Data exploration and preparation
# 2. Fitting the model
# 3. Testing the model


# STAGE 1: DATA EXPLORATION AND PREPARATION
# checking the imported dataset
glimpse(house_rent) # the dataset contains 4746 observation and 12 variables

summary(house_rent) # a quick summary of the data set

naniar::vis_miss(house_rent) # checking for missing value (their is no missing variable)

# removing duplicates
house_rent %>% 
  select(-`Posted On`) %>% 
  distinct()

# removing some of the variables that will not be relevant to model fitting
house_rent <- house_rent %>% 
  select(-`Posted On`, -`Point of Contact`, - `Tenant Preferred`, - `Area Locality`) # four variable are removed 

# normal continuous variable is vital for model performance!
# checking the distribution of the continuous variables
# --checking the distribution of "Rent" variable (response variable)
house_rent %>%   
  ggplot(aes(Rent)) +
  geom_histogram() 
# the rent variable is skewed to the right

# taking the log of "Rent" variable normalizes the distribution
house_rent %>% 
  ggplot(aes(log(Rent))) +
  geom_histogram()

# checking the distribution of "Size" variable
house_rent %>% 
  ggplot(aes(Size)) +
  geom_histogram()
# the variable Size is slightly skewed to the left 

# taking the square root of the "Size" variable normalizes the distribution
house_rent %>% 
  ggplot(aes(sqrt(Size))) +
  geom_histogram()

# checking the relationship between the two transformed continuous variables (response and predicting variable) 
house_rent %>% 
  ggplot(aes(sqrt(Size), log(Rent))) + # ploting the two transformed variables
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, # imposing a linear model line on the data points
              aes(color = "Linear Model")) + 
  geom_smooth(method = "loess", se = FALSE, # imposing LOESS model line into data points
              aes(color = "LOESS")) +
  scale_color_manual(values = c("Linear Model" = "blue","LOESS" = "red")) +
  labs(color = "Model")
# the relationship follows approximately the same curve indicating linearity

# checking the correlation of the transformed variables
with(house_rent, cor(log(Rent), sqrt(Size))) 
# correlation of 0.57 indicates a high correlation

# changing the some of th variable to the right format
house_rent <- house_rent %>% 
  mutate(Rent = log(Rent),   # transforming (log) rent variable
         Size = sqrt(Size),  # transforming (square root) the size variable
         Floor = fct(Floor), # changing to floor variable to factor
         Floor = as.numeric(Floor), # rechanging floor variable to a numeric factor
         `Area Type` = fct(`Area Type`), 
         `Area Type` = as.numeric(`Area Type`),
         City = fct(City),
         City = as.numeric(City),
         `Furnishing Status` = factor(`Furnishing Status`),
         `Furnishing Status` = as.numeric(`Furnishing Status`))

# separating the data set into train and test samples
set.seed(123) # initiate reproducible result by setting seed
sam <- sample(1:nrow(house_rent), # building a numeric vector from 1 to the row number of the dataset
              3/4 * nrow(house_rent)) # selecting a random 75% of the vector

house_rent_train <- house_rent[sam,] # isolating the training dataset
house_rent_test <- house_rent[-sam,] # isolating the testing dataset


# STAGE 2: FITTING THE REGRESSION MODEL 
# we will fit a linear regression since we are predicting a continuous variable

# building the model
rent.model <- lm(Rent ~ ., data = house_rent_train)

# summarizing the model 
summary(rent.model)
# all of the predictors are highly significant with a P-value of less than 0.001
# R-Squared is 0.65; this is the predictors are explaining 65 percent of house rent 

# checking for any possible multicollinearity using varience-inflation-factor (VIF)
car::vif(rent.model) %>% 
  barplot(main = "Variance Inflation Factor")
# there is no multicollinearity as all the variable has VIF of less than 10

# checking the model performance
plot(rent.model, which = 1) # residual appear to be normally distributed around zero
plot(rent.model, which = 2) # the residual is more or less normally distributed
plot(rent.model, which = 3) # there is no visible pattern indicating equal variance across the residual
plot(rent.model, which = 4) # using cooks'S distance threshold of 1, it appears there is no outlier


# STAGE 3: TESTING THE MODEL
# predicting with the model with prediction interval included

house_rent_test1 <- house_rent_test %>% 
  select(-Rent)

pred <- predict(rent.model,   # model used for the prediction
                newdata = house_rent_test1,  # using the test dataset
                interval = "prediction")    # including 95 prediction interval

pred <- exp(pred) # taking exponential of the prediction to return to the actual house rent
pred.conf <- pred[,2:3] # selecting the lower and upper bound of the prediction interval

# making a dataframe with the actual rent and its confidence interval
pred_test <- data.frame("lower" = pred.conf[,1], 
                        "upper" = pred.conf[,2],
                        "actual" = exp(house_rent_test$Rent))

pred_test <- tibble(pred_test) # converting to a tibble

# checking for accuracy of the model
# if the actual Rent falls between the confidence interval predicted by the model it is interpreted as "yes", otherwise "no"
prediction <- pred_test %>% 
  mutate(rent_range = case_when(between(actual, lower, upper) ~ "Yes",
                           .default = "No")) %>% 
  group_by(rent_range) %>% 
  count()

(accuracy <- (prediction[2,2]/sum(prediction[2,2] + prediction[1,2])) * 100)
# the accuracy of the model is  approximately 95 percent
# this simply means that the model is predicting the correct rent range 94 percent of the time
