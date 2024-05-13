install.packages("TTR")
install.packages("forecast")
library ("TTR")
library(forecast)
library(tidyverse)


Lfp<- read.csv("Life_Expectancys.csv", header = TRUE)


Lfp_TS <- Lfp %>% select(Year,Country,Life_expectancy_total)
head(Lfp_TS)



------------------------------------------------------------------------------
  
  library(dplyr)


selected_country <- "United Kingdom"

# Filter the data for the selected country
Lfp_TS_country <- Lfp_TS %>%
  filter(Country == selected_country)

# Aggregate life expectancy by year for the selected country
sum_Lfp_by_year <- Lfp_TS_country %>%
  group_by(Year) %>%
  summarise(total_lfp = sum(Life_expectancy_total))

# Create a time series
Lfp_ts <- ts(sum_Lfp_by_year$total_lfp, start = min(sum_Lfp_by_year$Year), frequency = 1)

# Plot the time series
plot.ts(Lfp_ts, main = paste("Total Life Expectancy for", selected_country, "per Year"), 
        ylab = "Life Expectancy")


------------------------------------------------------------------------------------------
#Time series for the Life Expectancy in the Ten Countries 
  library(dplyr)
library(ggplot2)


selected_countries <- c("United Kingdom", "United States", "Australia","Spain",
                        "Canada","Ireland","Portugal","Sweden","Finland","Italy")

# Filter the data for the selected countries
Lfp_TS_countries <- Lfp_TS %>%
  filter(Country %in% selected_countries)

# Aggregate life expectancy by year for the selected countries
sum_Lfp_by_year <- Lfp_TS_countries %>%
  group_by(Year, Country) %>%
  summarise(total_lfp = sum(Life_expectancy_total))

# Create a line plot using ggplot2
ggplot(sum_Lfp_by_year, aes(x = Year, y = total_lfp, color = Country)) +
  geom_line() +
  labs(title = "Total Life Expectancy Comparison for Selected Countries per Year",
       x = "Year",
       y = "Life Expectancy") +
  scale_color_discrete(name = "Country")

#Comparison Life EXPECTANCY BETWWEN UK AND us
-----------------------------------------------------------------------------------------------
  selected_countries <- c("United Kingdom", "United States")

# Filter the data for the selected countries
Lfp_TS_countries <- Lfp_TS %>%
  filter(Country %in% selected_countries)

# Aggregate life expectancy by year for the selected countries
sum_Lfp_by_year <- Lfp_TS_countries %>%
  group_by(Year, Country) %>%
  summarise(total_lfp = sum(Life_expectancy_total))

# Create a line plot using ggplot2
ggplot(sum_Lfp_by_year, aes(x = Year, y = total_lfp, color = Country)) +
  geom_line() +
  labs(title = "Total Life Expectancy Comparison for Selected Countries per Year",
       x = "Year",
       y = "Life Expectancy") +
  scale_color_discrete(name = "Country") 


------------------------------------------------------------------------------
  
  library(dplyr)


selected_country <- "United Kingdom"

# Filter the data for the selected country
Lfp_TS_country <- Lfp_TS %>%
  filter(Country == selected_country)

# Aggregate life expectancy by year for the selected country
sum_Lfp_by_year <- Lfp_TS_country %>%
  group_by(Year) %>%
  summarise(total_lfp = sum(Life_expectancy_total))

# Create a time series
Lfp_ts <- ts(sum_Lfp_by_year$total_lfp, start = min(sum_Lfp_by_year$Year), frequency = 1)

# Plot the time series
plot.ts(Lfp_ts, main = paste("Total Life Expectancy for", selected_country, "per Year"), ylab = "Life Expect

----------------------------------------------------------------------------------
Decomposing Time SERIES



Lfp_tsSMA3 <- SMA(Lfp_ts, n=3)
plot.ts(Lfp_tsSMA3)

Lfp_tsComponents <- decompose(Lfp_ts)
#Simple Exponetial Smoothing .HoltWINTERS Exponetialsmoothing
------------------------------------

Lifeexpectancy_ukforcast <- HoltWinters(Lfp_ts, beta=FALSE,gamma=FALSE)

Lifeexpectancy_ukforcast

Lifeexpectancy_ukforcast$fitted

plot(Lifeexpectancy_ukforcast)

Lifeexpectancy_ukforcast$SSE


Lifeexpectancy_ukforecast2 <- forecast(Lifeexpectancy_ukforcast, h=10)
Lifeexpectancy_ukforecast2


plot(Lifeexpectancy_ukforecast2)

acf(Lifeexpectancy_ukforecast2$residuals, lag.max=10 , na.action = na.pass)

Box.test(Lifeexpectancy_ukforecast2$residuals, lag=10, type="Ljung-Box")

plot.ts((Lifeexpectancy_ukforecast2$residuals)

Lifeexpectancy_ukforecast2$residuals
Lifeexpectancy_ukforecast2$residuals[!is.na(Lifeexpectancy_ukforecast2$residuals)]


-----------------------------------------------------------------------------------------
ARIMA Model

Formal Test For Stationary

Lifeexpectancy_ukdiff1 <- diff(Lfp_ts, differences=1)

plot.ts(Lifeexpectancy_ukdiff1)

acf(Lifeexpectancy_ukdiff1, lag.max=20)

pacf(Lifeexpectancy_ukdiff1, lag.max=20)
pacf(Lifeexpectancy_ukdiff1, lag.max=20, plot=FALSE) 

auto.arima(Lfp_ts)

Lifeexpectancyarima <- arima(Lfp_ts, order= c(0,1,0))

Lifeexpectancyarima

Lifeexpectancyukforecasts <- forecast(Lifeexpectancyarima, h=8)
Lifeexpectancyukforecasts
