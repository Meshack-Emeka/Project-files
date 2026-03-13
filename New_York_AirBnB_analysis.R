# ANALYSIS OF AIRBNB SHORTLET APARTMENT LISTING IN NEW YORK CITY

# the data used for the analysis is gotten from kaggle using this URL
# https://www.kaggle.com/datasets/godofoutcasts/new-york-city-airbnb-2023-public-data/data

# INFROMATIONS TO BE DERIVED FROM THE DATASET
# 1. neighbourhood group with the total number of house listed
# 2. neighbourhood in each neighbour_group with the highest number of listing
# 3. the median price of apartment in every neighbourhood group
# 4. the most expensive apartment in each neighbouhood and their names
# 5. number of apartment in each neighbourhood group above $5000 per night
# 6. number of apartment in each neighbourhood group above $50 per night
# 7. top 5 host with highest number of house listing
# 8. the number of apartments listing in each neighbouhood by the top host
# 9. neighbourhood with the top most listing based on room type



# importing the  dataframe into R
house_rent <- readr::read_csv("C:/Users/EMEKA/Downloads/NYC-Airbnb-2023.csv")

# loading the tidyverse package that will be used for the analysis
library(tidyverse) 

glimpse(house_rent)

dim(house_rent) # total of 42931 rows with 18 variables

# removing duplicates from the dataset
house_rent <- house_rent %>% 
  distinct()

# 1. neighbourhood group with the total number of house listed
house_rent %>% 
  group_by(neighbourhood_group) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(fct_reorder(neighbourhood_group, n), n)) +
  geom_bar(stat = "identity", fill = c("blue","gray","gray","gray","red")) +
  ggtitle("Number of Apartment by Neighbouhood") +
  ylab("Count") +
  xlab("Neighbouhood")
# "Manhattan" has the highest with over 17000 listing while "Staten Island" has the list with just 429 lisitn 

# 2. neighbourhood in each neighbourhood_group with the highest number of listing
house_rent %>% 
  group_by(neighbourhood_group, neighbourhood) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(neighbourhood_group) %>% 
  mutate(nn = dense_rank(desc(n))) %>% 
  filter(nn == 1) %>% 
  select(-nn)

# 3. the median price of apartment in every neighbourhood group
house_rent %>% 
  group_by(neighbourhood_group) %>% 
  summarise(P = median(price))

# 4. the most expensive apartment in each neighbouhood and their names  
house_rent %>% 
  group_by(neighbourhood_group) %>% 
  mutate(rank = dense_rank(desc(price))) %>% # ranking the prices in the neigbourhood
  filter(rank == 1) %>% # filtering the most expensive apartment
  arrange(desc(price)) %>% 
  select(neighbourhood_group, name,price)


# 5. number of apartment in each neighbourhoo_ group above $5000 per night
house_rent %>% 
  filter(price > 5000) %>% 
  group_by(neighbourhood_group) %>% 
  count() %>% 
  arrange(desc(n))
# the most expensive apartments are found in Manhattan with prices above $5000

# 6. number of apartment in each neighbourhood group below $50 per night
house_rent %>% 
  filter(price < 50) %>% 
  group_by(neighbourhood_group) %>% 
  count() %>% 
  arrange(desc(n))
# the cheapest apartments are found more in Brooklyn

# 7. top 5 host with highest number of apartment listing
house_rent %>% 
  group_by(host_name) %>% 
  summarise(num_of_apartments = length(name)) %>% 
  arrange(desc(num_of_apartments)) %>% 
  head(5)
# host with the highest number of listing is Blueground

# 8. the number of apartments listing in each neighbouhood by the top host
house_rent %>% 
  filter(host_name == "Blueground") %>% 
  count(neighbourhood_group)
# "Blueground" has most of its houses listed in Manhattan


# 9. neighbourhood with the top most listing based on room type
house_rent %>% 
  group_by(neighbourhood_group, room_type) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(room_type) %>% 
  mutate(rank = dense_rank(desc(n))) %>% 
  filter(rank == 1) %>% 
  select(-rank)
# while Manhattan boost with the highest number of listing in "Entire home/apt", "Hotel room" and "Shared room"
# Brooklyn has the highest number of "Private room" listing.

