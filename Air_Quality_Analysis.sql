--Analysis of Air-quality-index(aqi) of some popular cities in the world--
SELECT *
FROM pollution

--The year the sample was collected--
SELECT DISTINCT(EXTRACT('year' FROM samp_date)) 
FROM pollution
--2025

--The month the sample was collected--
SELECT DISTINCT(EXTRACT('month' FROM samp_date))
FROM pollution
--November

--The days the sample was collected--
SELECT DISTINCT(DATE_PART('day', samp_date)) 
FROM pollution
ORDER BY 1
--sample was collected from 4th of November to 19th November (a total of 15 days)

--Hours of sample collection--
SELECT DISTINCT(DATE_PART('hour', samp_date))
FROM pollution
ORDER BY 1
--sample was collected every hour from 1 early morning to 12 midnight(24 samples per day)

--A total of 50 cities
SELECT COUNT(DISTINCT(city))
FROM pollution

--That is 360 samples for each city during the time period
SELECT city, COUNT(*)
FROM pollution
GROUP BY 1
--This is 24 samples * 15 days = 360

--filtering the most polluted cities
--typically an aqi of 150 and above is considered unhealthy for the general public
SELECT city, count(*)				
FROM pollution
WHERE aqi > 150
GROUP BY city
ORDER BY 2 DESC
--the city of Doha with 28 recorded the most unhealthy air in the time period

--extracting the cities that experienced an unhealthy air atleast once per day in the 15 days interval 
SELECT city, COUNT(*) AS num_of_days
FROM(SELECT EXTRACT('day' FROM samp_date) AS days, city, COUNT(*)
     FROM pollution
     WHERE aqi > 150
     GROUP BY 1, 2)
GROUP BY 1
HAVING COUNT(*) > 12
ORDER BY 2 DESC
--6 cities experinced at least a bad air in a day in 13 of the 15 days the samples where collected
--Doha making the top 6

--typically air pollution is mostly caused by human activites as a result of over population
--industrial emission
--and on a rare occasion by natural phenomenon
--comparing the top 6 most polluted cities with there respective population
SELECT DISTINCT p.city, pp.popullation
FROM pollution AS p 
INNER JOIN population AS pp -- joining the population data
ON p.city = pp.city
WHERE p.city IN (SELECT city -- filtering for the 6 most populated cities
                 FROM (SELECT city, COUNT(*) AS num_of_days
                       FROM(SELECT EXTRACT('day' FROM samp_date) AS days, city, COUNT(*)
                            FROM pollution
                            WHERE aqi > 150
                            GROUP BY 1, 2)
                       GROUP BY 1
                       HAVING COUNT(*) > 12))
ORDER BY 2 
--Doha is of particular conscern as it always have a high aqi but with the least population
---among the most populated cities
--probably desert sand and high petrochemical industrial activities is contributing immenseley toward the
---the high pollution level in doha and the air is generally unhealthy


--performing a regression analysis to ascertain the pollutant contributing to the high aqi.
---i discovered that NO2 is responsible for the bulk of the high aqi.
---you can dowload the R-programming code for the regression analysis in this repository.
