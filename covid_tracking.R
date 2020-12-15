
# ----------
# Libraries
# ----------

library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(grid)
library(gridExtra)
library(usmap)
library(ggsci)

# ----------
# Data import/cleaning
# ----------

# import time series data:

confirmed_data <- read.csv("confirmed_2020_12_15.csv", stringsAsFactors = FALSE)  %>%
    filter(!(Province_State %in% c("American Samoa", "Diamond Princess", "Grand Princess", "Guam", 
                                 "Northern Mariana Islands", "Puerto Rico", "Virgin Islands"))) %>%
    select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Country_Region, -Lat, -Long_, -Combined_Key) %>%
    rename(state = Province_State) %>%
    group_by(state) %>%
    summarise_all(list(sum = sum))

deaths_data <- read.csv("deaths_2020_12_15.csv", stringsAsFactors = FALSE) %>%
    filter(!(Province_State %in% c("American Samoa", "Diamond Princess", "Grand Princess", "Guam", 
                                   "Northern Mariana Islands", "Puerto Rico", "Virgin Islands"))) %>%
    select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Country_Region, -Lat, -Long_, -Combined_Key, -Population) %>%
    rename(state = Province_State) %>%
    group_by(state) %>%
    summarise_all(list(sum = sum))

#convert time series (cumulative) counts into daily counts:

j <- ncol(confirmed_data)

for (i in 1:(j-2)) {
    confirmed_data[,i + j] <- confirmed_data[,i + 2] - confirmed_data[,i + 1]
    deaths_data[,i + j] <- deaths_data[,i + 2] - deaths_data[,i + 1]
}

#convert raw counts to per capita counts:

state_pop <- read.csv("state_populations.csv", stringsAsFactors = FALSE)

confirmed_daily_data <- confirmed_data[,c(1,(j+1):(2*j - 2))] %>%
    left_join(state_pop, ., by = "state")

deaths_daily_data <- deaths_data[,c(1,(j+1):(2*j - 2))] %>%
    left_join(state_pop, ., by = "state")

for (i in 3:j) {
    confirmed_daily_data[,i] <- confirmed_daily_data[,i]/confirmed_daily_data[,2]
    deaths_daily_data[,i] <- deaths_daily_data[,i]/deaths_daily_data[,2]
}











