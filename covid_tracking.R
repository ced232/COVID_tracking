
# ----------
# Libraries
# ----------

library(dplyr)
library(ggplot2)
library(ggsci)
library(grid)
library(gridExtra)
library(tidyr)
library(usmap)
library(viridis)


# ----------
# Data import/cleaning
# ----------

date_title <- "December 15th"
customPal <- c(pal_jco()(4)[c(3,1,2,4)])

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


# ----------
# Principal Component Analysis
# ----------

# run PCA:

set.seed(125)
confirmed_prcomp <- confirmed_daily_data %>%
    select(-state, -pop) %>%
    prcomp()

confirmed_pca <- data.frame(state = confirmed_daily_data$state,
                            PC1 = confirmed_prcomp$x[, 1],
                            PC2 = confirmed_prcomp$x[, 2],
                            PC3 = confirmed_prcomp$x[, 3]) %>%
    mutate(PC1 = scale(PC1)) %>%
    mutate(PC2 = scale(PC2)) %>%
    mutate(PC3 = scale(PC3)) 

# visualize cumulative variance:

pca_var <- data.frame(PC = 0:8,
                      var = c(0, as.vector(summary(confirmed_prcomp)$importance[3,1:8])))

var_plot <- ggplot(pca_var, aes(x = PC, y = var)) +
    ggtitle("\nPrinciple Component Importance",
            subtitle = paste0(date_title, "\n")) +
    geom_line(color = plasma(10)[4], size = .8) +
    scale_x_continuous(name = "\nPC\n",
                       limits = c(0,8),
                       breaks = seq(0,8,2)) +
    scale_y_continuous(name = "cumulative \nproportion \nof variance ",
                       limits = c(0,1),
                       breaks = seq(0,1,.25)) +
    geom_hline(yintercept = 0, color = "white", size = .8) +
    geom_vline(xintercept = 0, color = "white", size = .8) +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_line(color = "gray30"),
          panel.grid.major.y = element_line(color = "gray40"),
          panel.grid.major.x = element_line(color = "gray50"),
          text = element_text(color = "white", family = "Avenir"), 
          strip.text = element_text(color = "white", family = "Avenir Black", size = 12, hjust = 0),
          axis.text = element_text(color = "white"),
          axis.title.y = element_text(size = 10, angle = 0, vjust = .5, hjust = 1),
          plot.title = element_text(family = "Avenir Black", hjust = .5, size = 14), 
          plot.subtitle = element_text(family = "Avenir", hjust = .5, size = 10),
          panel.background = element_rect(fill = "black"),
          #panel.border = element_rect(fill = NA, color = "white", size = 1),
          plot.background = element_rect(fill = "black", color = "black"), 
          legend.title = element_text(size = 10),
          legend.background = element_rect(fill = "black"), 
          legend.key = element_rect(fill = "black"),
          legend.key.size = unit(.6, "cm"),
          legend.text = element_text(size = 8),
          legend.position = "none",
          plot.margin = unit(c(5.5, 33, 5.5, 5.5), "pt"))

var_plot




