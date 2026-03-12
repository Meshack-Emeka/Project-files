--Analysis of Air-quality-index(aqi) of some popular cities in the world--
--Doha petrochemical industries are probabily contributiing immensely toward the high level of air pollution in the city


SELECT *
FROM airquality


--The year the sample was collected--
SELECT DISTINCT(EXTRACT('year' FROM samp_date)) 
FROM airquality
--2025

--The month the sample was collected--
SELECT DISTINCT(EXTRACT('month' FROM samp_date))
FROM airquality
--November


--The days the sample was collected--
SELECT DISTINCT(DATE_PART('day', samp_date)) 
FROM airquality
ORDER BY 1
--sample was collected from 4th of November to 19th November (a total of 15 days)

--Hours of sample collection--
SELECT DISTINCT(DATE_PART('hour', samp_date))
FROM airquality
ORDER BY 1
--sample was collected every hour from 1 early morning to 12 midnight(24 samples per day)


--Number of cities the dataset
SELECT COUNT(DISTINCT(city))
FROM airquality
--A total number of 50 cities


--Number of samples collected for each city
SELECT city, COUNT(*)
FROM airquality
GROUP BY 1
--A total of 360 samples per city (that is 24 samples per day * 15 days in the month)


--filtering the most polluted cities
--The Air-Quality-Index (aqi) measurement is used as a parameter to determine the level of pollution in a city
--typically an aqi of 150 and above is considered unhealthy for the general public and can cause respiratory track infection
SELECT city, count(*)				
FROM airquality
WHERE aqi > 150
GROUP BY city
ORDER BY 2 DESC
--the city of Doha with 28 recorded the most unhealthy air in the time period


--extracting the cities that experienced an unhealthy air atleast once per day in the 15 days interval 
SELECT city, COUNT(*) AS num_of_days
FROM(SELECT EXTRACT('day' FROM samp_date) AS days, city, COUNT(*)
     FROM airquality
     WHERE aqi > 150
     GROUP BY 1, 2)
GROUP BY 1
HAVING COUNT(*) > 12
ORDER BY 2 DESC
--6 cities experinced at least an unhealthy air in a day in 13 of the 15 days the samples where collected
--Doha making the top 6


--Air pollution is caused by several factors such as
--high level of human activites resulting from over population
--toxic gas emission from industries and automobile
--and on a rare occasion by natural phenomenon such as volcanic eruption
--comparing the top 6 most polluted cities with there respective population
SELECT DISTINCT p.city, population, rank
FROM airquality AS p 
INNER JOIN population AS pp -- joining the population data
ON p.city = pp.city
WHERE p.city IN (SELECT city -- filtering for the 6 most populated cities
                 FROM (SELECT city, COUNT(*) AS num_of_days
                       FROM(SELECT EXTRACT('day' FROM samp_date) AS days, city, COUNT(*)
                            FROM airquality
                            WHERE aqi > 150
                            GROUP BY 1, 2)
                       GROUP BY 1
                       HAVING COUNT(*) > 12))
ORDER BY 2 
--Doha is of particular conscern as it always have a high aqi but with the least population among the 50 states
--Therefore since Doha has the least population, probably desert sand and high petrochemical industrial activities are contributing immenseley toward the
---the high pollution level in Doha and the air is generally unhealthy.




