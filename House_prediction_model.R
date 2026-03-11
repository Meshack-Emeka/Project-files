## BUILDING HOUSE PREDICTION MODEL USING LINEAR REGRESSION

# Download the raw data from kaggle using the URL
https://www.kaggle.com/datasets/iamsouravbanerjee/house-rent-prediction-dataset?select=House_Rent_Dataset.csv

library(tidyverse) # loading the tidyverse package
library(car) # loading car package
house_rent <- read_csv(choose.files()) # locate the downloaded file and import into RStudio

# checking the imported dataset
glimpse(house_rent) # the dataset contains 4746 observation and 12 variables

summary(house_rent) # a quick summary of the data set

naniar::vis_miss(house_rent) # checking for missing value (thier is no missing variable)

house_rent <- unique(house_rent) # removing any possible duplicates

# removing some of the variables that is not relevant to model
house_rent1 <- house_rent %>% 
  select(-`Posted On`, -`Point of Contact`, - `Tenant Preferred`) # three variable are removed 

# checking the distribution of the continuous variables
# checking the distribution of rent variable (response variable)
house_rent1 %>%   
  ggplot(aes(Rent)) +
  geom_histogram() 
# the rent variable is heavily skewed to the right

# taking the log of Rent variable normalizes the distribution
house_rent1 %>% 
  ggplot(aes(log(Rent))) +
  geom_histogram()

# checking the distribution of Size variable
house_rent1 %>% 
  ggplot(aes(Size)) +
  geom_histogram()
# the variable Size is slightly skewed to the left 

# taking the square root of the size variable normalizes the variable
house_rent1 %>% 
  ggplot(aes(sqrt(Size))) +
  geom_histogram()

# checking the relationship between the two transformed variables 
house_rent1 %>% 
  ggplot(aes(sqrt(Size), log(Rent))) + # ploting the two transformed variables
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, # imposing a linear model line on the data points
              aes(color = "Linear Model")) + 
  geom_smooth(method = "loess", se = FALSE, # imposing LOESS model line into data points
              aes(color = "LOESS")) +
  scale_color_manual(values = c("Linear Model" = "blue","LOESS" = "red")) +
  labs(color = "Model")
# the two models follows approximately the same curve indicating the the realtionship is linear

# checking the correlation
with(house_rent1, cor(log(Rent), sqrt(Size))) # correlation of 0.56 indicated a high correlation


# changing the variables to the right format suitable for modeling
house_rent_lm <- house_rent1 %>% 
  mutate(Rent = log(Rent),      # transforming(log) the Rent variable
         Size = sqrt(Size),     # tranforming(square root) the Size variable
         Floor = fct(Floor),    # changing the floor variable to factor
         Floor = as.numeric(Floor), # rechanging floor variable to a discret numeric varaible
         `Area Type` = fct(`Area Type`), 
         `Area Locality` = fct(`Area Locality`),
         `Area Type` = as.numeric(`Area Type`),
         `Area Locality` = as.numeric(`Area Locality`),
         City = fct(City),
         City = as.numeric(City),
         `Furnishing Status` = factor(`Furnishing Status`),
         `Furnishing Status` = as.numeric(`Furnishing Status`)) 


# seperating the data set into train and test samples
set.seed(123) # initiate reploducible result by setting seed
sam <- sample(1:nrow(house_rent_lm), # building a numeric vector from 1 to the row number of the dataset
              3/4 * nrow(house_rent_lm)) # selecting a random 75% of the vector

house_rent_lm_train <- house_rent_lm[sam,] # isolating the training dataset
house_rent_lm_test <- house_rent_lm[-sam,] # isolating the testing dataset

# fitting the linear regression model
# building a kitchen-sink model using all the variables
rent.model <- lm(Rent ~ ., data = house_rent_lm_train)

# summarising the model 
summary(rent.model)
# most of the variables is significant except the City variable

car::vif(rent.model) %>% # checking for possible multicollinearity
  barplot(main = "Variance Inflation Factor") 
# "Area Locality" and "City" variable appear to be highly correlated 
# there we need to remove one of the variables from the model

house_rent_lm_train <- house_rent_lm_train %>% 
  select(-`Area Locality`) # removing the "Area Locality" variable

# rebuildind the model without the "Area Locality" variable
rent.model1 <- lm(Rent ~ ., data = house_rent_lm_train)

summary(rent.model1) # all the variables are highly significant

car::vif(rent.model1) %>% # variables appear to be normal hence no multicollinearity
  barplot(main = "Variance Inflation Factor") 

# checking the model performance
plot(rent.model1, which = 1) # residual appear to be normally distributed around zero
plot(rent.model1, which = 2) # the standard residual is more or less normally distributed
plot(rent.model1, which = 3) # variance appear to be equal across the residual
plot(rent.model1, which = 4) # using 1 as the treshold it appears there is no outlier


# predicting with the model with prediction interval included
pred <- predict(rent.model1, # model used for the prediction
                newdata = house_rent_lm_test,  # using the test dataset
                interval = "prediction")

pred <- exp(pred) # taking exponential of the prediction to return to the actual house rent
pred.conf <- pred[,2:3] # selecting the lower and upper bound of the prediction interval

# isolating the actual Rent variable in the test data set
house_rent_test <- house_rent_lm_test %>% 
  mutate(Rent = exp(Rent)) %>% # returning to the actual house rent by taking the exponential
  pull(Rent)

# making a dataframe with the actual rent and its confidence interval
pred_test <- data.frame("lower" = pred.conf[,1], 
                        "upper" = pred.conf[,2],
                        "actual" = house_rent_test)

pred_test <- tibble(pred_test) # converting to a tibble


# checking for accuracy of the model
# if the actual Rent falls in between the confidence interval predicted by the model it is interpreted as "yes", otherwise "no"
prediction <- pred_test %>% 
  mutate(rent_range = case_when(between(actual, lower, upper) ~ "Yes",
                           .default = "No")) %>% 
  group_by(rent_range) %>% 
  count()

accuracy <- ((prediction[2,2] - prediction[1,2])/prediction[2,2]) * 100
# the accuracy of the model is  approximately 95 percent
# this simply means that the model is predicting the rent range 95 percent of the time


