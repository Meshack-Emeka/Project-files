# Using Linear regression to identify the variables that are contributing to high Air-Quality-Index in the city of DOHA

#install and load the tidyverse package
install.packages("tidyverse")
library(tidyverse)

# import the the pollution csv into Rstudio 
pollution <- read_csv(choose.files())

# removing the variables that are not relevant to the analysis
pollution1 <- pollution %>% 
  filter(city == "Doha") %>%
  select(where(is.numeric), -latitude, -longitude, -wind_speed, -humidity, -temperature)

# fitting the data to a linear model
# aqi is the response variable while the other variable is the predictor variables
model <- lm(aqi ~., data = pollution1)


# summarizing the model 
summary(model)
# the model indicates that pm2.5, pm10 and NO2 are highly significance 
# therefore the atmospheric air in Doha contains high level of nitrogen pollutant
# this pollutant is mainly from combustion of fossil fuel, probably from the heavy chemical industries

