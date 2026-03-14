# You can download the dataset used for the modeling in any of these sites
#https://www.kaggle.com/datasets/rishidamarla/heart-disease-prediction
#https://archive.ics.uci.edu/dataset/45/heart+disease

# BUILDING INDIA HOUSE PREDICTION MODEL USING LINEAR REGRESSION 

# loading some the external packages that will be used for the analysis
library(ResourceSelection)
library(car)

# importing the data set 
heart_data <- read.csv(choose.files()) # select the file directory and import

str(heart_data) 

# summarizing the dataset
summary(heart_data) # dataset contains 270 observations and 14 variables

#checking for any missing value
anyNA(heart_data) # there is no missing observation

# CHANGING SOME OF THE VARIABLE TO THE RIGHT DATA FORMAT
# "Sex" variable is encoded with female as 0 and male as 1
heart_data$Sex <- factor(heart_data$Sex, # changing the sex variable to factor
                         levels = c(0,1),
                         labels = c("female","male"))

heart_data$Heart.Disease <- factor(heart_data$Heart.Disease) # changing the response variable into factor


# SPLITING THE DATA SET INTO TRAINING AND TEST DATASETS
# Note: splitting the model will reduce the power of the model by reducing the number of observation
set.seed(300)
sam <- sample(1:nrow(heart_data), 3/4 * nrow(heart_data))
traindata <- heart_data[sam, ] # 75% of the dataset will be used to train the model
testdata <- heart_data[-sam, ] # 25% will be used to test the model  


# FITTING THE MODEL
# the response variable is a binary variable hence I will fit a logistic regression model
# I fit the model using the "step" function in the base package
# I will fit both the forward selection and backward elimination and compare the two
# I will compare the two model and select using the Akaike-infromation-criteria (AIC)


# Fitting with the forward selection
glm.heart1 <- glm(Heart.Disease ~  1, data = traindata, family = "binomial") # I will start fitting with only intercept model

glm.forward <- step(glm.heart1, scope = mpg ~ + Age + Sex + Chest.pain.type + BP + Cholesterol
                  + FBS.over.120 + EKG.results + Max.HR + Exercise.angina +
                    ST.depression + Slope.of.ST + Number.of.vessels.fluro +
                    Thallium, direction = "forward", trace = FALSE)

summary(glm.forward)

AIC(glm.forward) # checking the AIC number

# Fitting with backward elimination
glm.heart2 <- glm(Heart.Disease ~  + Age + Sex + Chest.pain.type + BP + Cholesterol
                 + FBS.over.120 + EKG.results + Max.HR + Exercise.angina +
                   ST.depression + Slope.of.ST + Number.of.vessels.fluro +
                   Thallium, data = traindata, family = "binomial")

glm.backward <- step(glm.heart2, direction = "backward", trace = FALSE)

summary(glm.backward)

AIC(glm.backward)
# both the forward selction and backward elimination arrived at the same model


# CHECKING THE VALIDITY OF THE MODELS

# 1. checking the model fit using Hosmer and Lemeshow test goodness-of-feet
hoslem.test(as.numeric(glm.backward$y), fitted(glm.backward)) # the test failed to reject the null hypothesis, indicating a good fit

# 2. checking for mulitcollinearity using varience inflation factor
vif(glm.backward) # all the variables have a VIF of less than 2, there is no multicollinearity 

# 3 checking for possible outlier using Cook's distance
plot(glm.backward, which = 4) # Cook's distances are less than 1, hence there is no outlier



# TESTING THE MODEL PERFORMANCE
# removing the response variable from the test dataset
testdata_now <- testdata[, !names(testdata) %in% c( "Heart.Disease")]

# predicting using the predict function 
pred <- predict(glm.forward, newdata = testdata_now, type = "response")

# categorising the prediction
pred1 <- ifelse(pred <= .5, 0, 1) # values above .5 are deemed diseased otherwise, non-diseased 
pred1 <- factor(pred1, labels = c("Absence", "Presence")) # naming the variable

# building a contegency table from the model prediction and the raw vector
test <- table(pred1, testdata$Heart.Disease)

# checking the accuracy
acccuracy <- sum(diag(test))/sum(test) * 100
# the accuracy of the model is 80, therefore 86 percent of the time 
# --the model is predicting the correct value whether there is the presence or absence of disease

# False Negative is important 
FN <- test[1,2]/sum(test) * 100
# roughly 10 percent of the time the model is predicting there is no heart disease while there is one