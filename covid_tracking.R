
# ----------
# Libraries
# ----------

library(cluster)
library(dplyr)
library(factoextra)
library(ggplot2)
library(ggsci)
library(grid)
library(gridExtra)
library(NbClust)
library(tidyr)
library(usmap)
library(viridis)


# ----------
# Constants
# ----------

date_title <- "April 21st"
end_date <- as.Date("2021-4-20")
customPal <- c(magma(20)[c(20,16,12,9)], "#244999", "#8AAFFF")


# ----------
# Data import/cleaning
# ----------

# import time series data:

confirmed_data <- read.csv("~/git/covid_tracking/confirmed_2021_4_21.csv", stringsAsFactors=FALSE)  %>%
    filter(!(Province_State %in% c("American Samoa", "Diamond Princess", "Grand Princess", "Guam", 
                                   "Northern Mariana Islands", "Puerto Rico", "Virgin Islands"))) %>%
    select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Country_Region, -Lat, -Long_, -Combined_Key) %>%
    rename(state = Province_State) %>%
    group_by(state) %>%
    summarise_all(list(sum = sum))

deaths_data <- read.csv("~/git/covid_tracking/deaths_2021_4_21.csv", stringsAsFactors = FALSE) %>%
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

state_pop <- read.csv("~/git/covid_tracking/state_populations.csv", stringsAsFactors = FALSE)

confirmed_daily_data <- confirmed_data[,c(1,(j+1):(2*j - 2))] %>%
    left_join(state_pop, ., by = "state")

deaths_daily_data <- deaths_data[,c(1,(j+1):(2*j - 2))] %>%
    left_join(state_pop, ., by = "state")

for (i in 3:j) {
    confirmed_daily_data[,i] <- confirmed_daily_data[,i]/confirmed_daily_data[,2]
    deaths_daily_data[,i] <- deaths_daily_data[,i]/deaths_daily_data[,2]
}

#convert from wide to long form, format dates:

cases_by_state <- confirmed_daily_data %>%
    select(-pop) %>%
    group_by(state) %>%
    mutate(cases = 1000000*cases) %>%
    gather(date, cases, -state) %>%
    rowwise() %>%
    mutate(date = gsub("X", "", date)) %>%
    mutate(date = gsub("_sum.1", "", date)) %>%
    mutate(month = strsplit(date, ".", fixed = TRUE)[[1]][1]) %>%
    mutate(day = strsplit(date, ".", fixed = TRUE)[[1]][2]) %>%
    mutate(year = strsplit(date, ".", fixed = TRUE)[[1]][3]) %>%
    mutate(date = as.Date(paste0("20", year, "-", month, "-", day))) %>%
    select(-year, -month, -day) 


# ----------
# Locate Peaks
# ----------

# find standard deviation of smoothed daily confirmed cases among all states on each date:

date_range <- as.Date("2020-1-23"):end_date
state <- c()
date <- c()
cases <- c()
dates <- c(1:length(date_range))

for (i in confirmed_daily_data$state) {
    loess_data <- cases_by_state %>%
        filter(state == i) %>%
        select(date, cases)
    
    loess_data$index <- 1:nrow(loess_data)
    
    loess_fit <- loess(cases ~ index, loess_data, span = 0.1)
    
    loess_fit_df <- data.frame(index = 1:nrow(loess_data), fitted = loess_fit$fitted)
    
    state <-  c(state, rep(i, length(dates)))
    
    for (j in 1:length(dates)) {
        date <- c(date, j)
        cases <- c(cases, loess_fit_df$fitted[loess_fit_df$index == dates[j]])
    }
}

sd_df <- data.frame(state, date = as.Date(date_range[date], origin = "1970-1-1"), cases) %>%
    group_by(date) %>%
    select(date, cases) %>%
    summarise_all(list(sd = sd))

# locate local maxima:

candidates <- c()

for (i in 2:(nrow(sd_df) - 1)) {
    if (sd_df$sd[i - 1] < sd_df$sd[i]) {
        if (sd_df$sd[i + 1] < sd_df$sd[i]) {
            candidates <- c(candidates, i)
        }
    }
}

candidate_dates <- as.Date(date_range[candidates], origin = "1970-1-1")
candidate_dates

# select from candidate peaks graphically:

selected_candidates <- c(80, 178, 296, 351, 440) 

selected_dates <- as.Date(date_range[selected_candidates], origin = "1970-1-1")

selected <- factor(ifelse(candidates %in% selected_candidates, "selected", "not selected"),
                   levels = c("selected", "not selected"))

selected_df <- data.frame(candidates = as.Date(date_range[candidates], origin = "1970-1-1"), selected)

peak_plot <- ggplot() +
    ggtitle("\nSelecting from Candidate Peaks",
            subtitle = paste0(date_title, "\n")) +
    geom_area(data = sd_df, aes(x = date, y = sd), fill = plasma(10)[4]) +
    geom_segment(data = selected_df, aes(x = candidates, xend = candidates, color = selected), y = 0, yend = Inf) +
    geom_hline(yintercept = 0, size = .5, color = "gray45") +
    scale_x_date(name = "\ndate", date_breaks = "2 months", date_labels = "%b") +
    scale_y_continuous(name = "standard  \ndeviation in  \ndaily cases  \nper million  ") +
    scale_color_manual(name = "Candidate\nPeaks", values = c("white", "gray35")) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          text = element_text(color = "white", family = "Avenir"), 
          strip.text = element_text(color = "white", family = "Avenir Black", size = 12, hjust = 0),
          axis.text = element_text(color = "white"),
          axis.title.y = element_text(size = 10, angle = 0, vjust = .5, hjust = 1),
          plot.title = element_text(family = "Avenir Black", hjust = .5, size = 14), 
          plot.subtitle = element_text(family = "Avenir", hjust = .5, size = 10),
          panel.background = element_rect(fill = "gray10"),
          panel.border = element_rect(fill = NA, color = "gray45", size = 1),
          plot.background = element_rect(fill = "black", color = "black"), 
          legend.title = element_text(size = 10),
          legend.background = element_rect(fill = "black"), 
          legend.key = element_rect(fill = "black"),
          legend.key.size = unit(.6, "cm"),
          legend.text = element_text(size = 8))

peak_plot


# ----------
# Clustering
# ----------

# measure each state's daily confirmed cases at each peak:

state <- c()
peak <- c()
cases <- c()

for (i in confirmed_daily_data$state) {
    loess_data <- cases_by_state %>%
        filter(state == i) %>%
        select(date, cases)
    
    loess_data$index <- 1:nrow(loess_data)
    
    loess_fit <- loess(cases ~ index, loess_data, span = 0.1)
    
    loess_fit_df <- data.frame(index = 1:nrow(loess_data), fitted = loess_fit$fitted)
    
    state <-  c(state, rep(i, length(selected_candidates)))
    
    for (j in 1:length(selected_candidates)) {
        peak <- c(peak, paste0("peak_", j))
        cases <- c(cases, loess_fit_df$fitted[loess_fit_df$index == selected_candidates[j]])
    }
}

peaks_df <- data.frame(state, peak, cases) %>%
    spread(peak, cases)

peaks_df <- cbind(state = peaks_df$state, as.data.frame(scale(peaks_df[2:ncol(peaks_df)])))

# select K value:

k_val <- 5

# validate using silhouettes:

set.seed(125)
k <- kmeans(peaks_df[,c(2:(length(selected_candidates) + 1))], k_val)
cluster2 <- k$cluster
cluster <- factor(cluster2)
confirmed_cluster <- cbind(peaks_df, cluster)

sil <- silhouette(cluster2, dist(peaks_df[,c(2:(length(selected_candidates) + 1))]))
sort_sil <- sortSilhouette(sil)

sil_df <- as.data.frame(sort_sil[,1:3]) %>%
    mutate(index = nrow(sort_sil):1) %>%
    mutate(cluster = factor(cluster)) 

average_sil <- mean(sil_df$sil_width)

sil_plot <- sil_df%>%
    ggplot(aes(y = sil_width, x = index)) +
    ggtitle("\nK-Clustering Silhouettes",
            subtitle = paste0(date_title, "\n")) +
    geom_col(aes(fill = cluster)) +
    geom_hline(yintercept = average_sil, color = "white", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "white") +
    scale_y_continuous(name = "\nsilhouette width\n") +
    coord_flip() +
    scale_fill_manual(name = "Cluster", values = customPal) + 
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          text = element_text(color = "white", family = "Avenir"), 
          axis.text = element_text(color = "white"),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(family = "Avenir Black", hjust = .5, size = 14), 
          plot.subtitle = element_text(family = "Avenir", hjust = .5, size = 10),
          panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black", color = "black"),
          legend.background = element_rect(fill = "black"), 
          legend.key = element_rect(fill = "black"),
          legend.key.size = unit(.6, "cm"),
          legend.text = element_text(size = 8))

sil_plot

# ----------
# State Clusters Map
# ----------

state_abbr <- read.csv("state_fips.csv", stringsAsFactors = FALSE)

confirmed_plot <- confirmed_cluster %>%
    rename(NAME = state) %>%
    left_join(., state_abbr, by = "NAME") %>%
    select(state, cluster)

state_data <- usmap::statepop %>%
    rename(state = abbr) %>%
    filter(!(state == "DC")) %>%
    left_join(., confirmed_plot, by = "state")

confirmed_cluster_map <- plot_usmap(data = state_data, values = "cluster", color = "black") + 
    ggtitle("\nStates Clustered by Trends in Daily New Cases",
            subtitle = date_title) +
    scale_fill_manual(name = "Cluster",
                      values = customPal, 
                      na.translate = FALSE, drop = FALSE) +
    theme(legend.position = "right", 
          text = element_text(color = "white", family = "Avenir"), 
          plot.title = element_text(family = "Avenir Black", hjust = .5, size = 14), 
          plot.subtitle = element_text(family = "Avenir", hjust = .5, size = 10),
          plot.background = element_rect(fill = "black", color = "black"), 
          legend.background = element_rect(fill = "black"), 
          legend.key = element_rect(fill = "black"),
          legend.key.size = unit(.5, "cm"),
          legend.text = element_text(size = 8))

confirmed_cluster_map

# ----------
# Trends By State Cluster
# ----------

deaths_cluster_section <- confirmed_cluster %>%
    select(state, cluster) %>%
    left_join(., deaths_daily_data, by = "state") %>%
    select(-state, -pop) %>%
    group_by(cluster) %>%
    summarise_all(list(mean = mean)) %>%
    gather(date, cases, -cluster) %>%
    mutate(cases = 1000000*cases) %>%
    rowwise() %>%
    mutate(date = gsub("X", "", date)) %>%
    mutate(date = gsub("_sum.1_mean", "", date)) %>%
    mutate(month = strsplit(date, ".", fixed = TRUE)[[1]][1]) %>%
    mutate(day = strsplit(date, ".", fixed = TRUE)[[1]][2]) %>%
    mutate(year = strsplit(date, ".", fixed = TRUE)[[1]][3]) %>%
    mutate(date = as.Date(paste0("20", year, "-", month, "-", day))) %>%
    select(-year, -month, -day) %>%
    mutate(metric = "deaths")

confirmed_cluster_section <- confirmed_cluster %>%
    select(state, cluster) %>%
    left_join(., confirmed_daily_data, by = "state") %>%
    select(-state, -pop) %>%
    group_by(cluster) %>%
    summarise_all(list(mean = mean)) %>%
    gather(date, cases, -cluster) %>%
    mutate(cases = 1000000*cases) %>%
    rowwise() %>%
    mutate(date = gsub("X", "", date)) %>%
    mutate(date = gsub("_sum.1_mean", "", date)) %>%
    mutate(month = strsplit(date, ".", fixed = TRUE)[[1]][1]) %>%
    mutate(day = strsplit(date, ".", fixed = TRUE)[[1]][2]) %>%
    mutate(year = strsplit(date, ".", fixed = TRUE)[[1]][3]) %>%
    mutate(date = as.Date(paste0("20", year, "-", month, "-", day))) %>%
    select(-year, -month, -day) %>%
    mutate(metric = "confirmed")

full_plot <- rbind(confirmed_cluster_section, deaths_cluster_section) 

metric_names <- list(
    "confirmed" = "New Confirmed Cases", 
    "deaths" = "Deaths"
)

metric_labeller <- function(variable,value){
    return(metric_names[value])
}

trends_plot <- full_plot %>%
    ggplot(aes(x = date, y = cases)) +
    ggtitle("\nUS COVID-19 Trends by State Cluster",
            subtitle = date_title) +
    geom_hline(yintercept = 0, size = .5, color = "gray45") +
    geom_vline(xintercept = as.Date(selected_dates), size = .8, color = "white") +
    geom_smooth(aes(color = cluster), se = FALSE, span = .1, size = .8) +
    scale_color_manual(name = "Group",
                       values = customPal, 
                       na.translate = FALSE, drop = FALSE) +
    scale_x_date(name = "", date_breaks = "2 months", date_labels = "%b") +
    scale_y_continuous(name = "mean daily \ncases/deaths \nper million ") +
    facet_wrap(~metric, nrow = 2, scales = "free", labeller = metric_labeller) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(color = "gray25"),
          panel.grid.major.x = element_line(color = "gray35"),
          text = element_text(color = "white", family = "Avenir"), 
          strip.text = element_text(color = "white", family = "Avenir Black", size = 12, hjust = 0),
          axis.text = element_text(color = "white"),
          axis.title.y = element_text(size = 10, angle = 0, vjust = .5, hjust = 1),
          plot.title = element_text(family = "Avenir Black", hjust = .5, size = 14), 
          plot.subtitle = element_text(family = "Avenir", hjust = .5, size = 10),
          panel.background = element_rect(fill = "gray10"),
          panel.border = element_rect(fill = NA, color = "gray45", size = 1),
          plot.background = element_rect(fill = "black", color = "black"), 
          legend.title = element_text(size = 10),
          legend.background = element_rect(fill = "black"), 
          legend.key = element_rect(fill = "black"),
          legend.key.size = unit(.6, "cm"),
          legend.text = element_text(size = 8),
          legend.position = "none",
          plot.margin = unit(c(5.5, 33, 5.5, 5.5), "pt"))

trends_plot

# ----------
# Heat Map
# ----------

# measure each cluster's mean daily confirmed cases at each peak:

cluster <- c()
peak <- c()
cases <- c()

for (i in 1:length(selected_candidates)) {
    loess_data <- confirmed_cluster_section %>%
        filter(cluster == i) %>%
        select(date, cases)
    
    loess_data$index <- 1:nrow(loess_data)
    
    loess_fit <- loess(cases ~ index, loess_data, span = 0.1)
    
    loess_fit_df <- data.frame(index = 1:nrow(loess_data), fitted = loess_fit$fitted)
    
    cluster <-  c(cluster, rep(i, length(selected_candidates)))
    
    for (j in 1:length(selected_candidates)) {
        peak <- c(peak, j)
        cases <- c(cases, loess_fit_df$fitted[loess_fit_df$index == selected_candidates[j]])
    }
}

# standardize each peak:

df <- data.frame(cluster, peak, cases)

cluster <- c()
peak <- c()
cases <- c()

for (i in 1:5) {
    subset_df <- df %>%
        filter(peak == i)
    
    max_cases <- max(subset_df$cases)
    min_cases <- 0
    
    standardized_cases <- (subset_df$cases - min_cases)/(max_cases - min_cases)
    
    cluster <- c(cluster, 1:5)
    peak <- c(peak, rep(i, 5))
    cases <- c(cases, standardized_cases)
}

# plot heatmap:

plot_df <- data.frame(cluster, peak, cases) %>%
    mutate(peak = factor(peak))

facet_names <- list(
    "1" = "Spike 1", 
    "2" = "Spike 2",
    "3" = "Spike 3",
    "4" = "Spike 4",
    "5" = "Spike 5"
)

facet_labeller <- function(variable,value){
    return(facet_names[value])
}

plot_df %>%
    ggplot(aes(x = 1, y = cluster)) +
    ggtitle("\nEach Cluster's Contribution to the Five Primary Spikes",
            subtitle = date_title) +
    geom_tile(aes(fill = cases)) +
    scale_y_reverse(name = "", 
                    breaks = 1:5, 
                    labels = c("1: The Northeast",
                               "2: The Midwest\n   and Great Plains",
                               "3: The South\n   and Southwest",
                               "4: Michigan",
                               "5: Pacific Northwest,\n    Northern New England,\n    and Mid-Atlantic")) +
    scale_fill_gradientn(name = "",
                         colours = c("White", "#244999")) +
    facet_wrap(~peak, ncol = length(peaks), labeller = facet_labeller, strip.position = "bottom") +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major = element_blank(),
          text = element_text(color = "white", family = "Avenir"), 
          axis.text.y = element_text(color = "white", hjust = 0),
          axis.text.x = element_blank(),
          axis.title.y = element_text(angle = 0, vjust = .5, hjust = 1, size = 12),
          axis.title.x = element_blank(),
          plot.title = element_text(family = "Avenir Black", hjust = .5, size = 14), 
          plot.subtitle = element_text(family = "Avenir", hjust = .5, size = 10),
          plot.background = element_rect(fill = "black", color = "black"), 
          legend.position = "none",
          strip.text = element_text(size = 12, color = "white"))






