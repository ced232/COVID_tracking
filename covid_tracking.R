



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

date_title <- "October 8th"
end_date <- as.Date("2021-10-9")
customPal <- magma(8)[3:8]
redsPal <- c("White", "#ffd2d2", "#ffb4b4", "#ff8c8c", "#ff6464", "#ff4040", "#ff0000", "#dc0000", "#b90000")
bluesPal <- c("#8cc6ff", "#40a0ff", "#006edc", "#004C96")


# ----------
# Data import/cleaning
# ----------

# import time series data:

confirmed_data <- read.csv("~/git/covid_tracking/confirmed_2021_10_8.csv", stringsAsFactors=FALSE)  %>%
    filter(!(Province_State %in% c("American Samoa", "Diamond Princess", "Grand Princess", "Guam", 
                                   "Northern Mariana Islands", "Puerto Rico", "Virgin Islands"))) %>%
    select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Country_Region, -Lat, -Long_, -Combined_Key) %>%
    rename(state = Province_State) %>%
    group_by(state) %>%
    summarise_all(list(sum = sum))

deaths_data <- read.csv("~/git/covid_tracking/deaths_2021_10_8.csv", stringsAsFactors = FALSE) %>%
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
    gather(date, cases, -state) %>%
    mutate(cases = 1000000*cases) %>%
    rowwise() %>%
    mutate(date = gsub("X", "", date)) %>%
    mutate(date = gsub("_sum", "", date)) %>%
    mutate(month = strsplit(date, ".", fixed = TRUE)[[1]][1]) %>%
    mutate(day = strsplit(date, ".", fixed = TRUE)[[1]][2]) %>%
    mutate(year = strsplit(date, ".", fixed = TRUE)[[1]][3]) %>%
    mutate(date = as.Date(paste0("20", year, "-", month, "-", day))) %>%
    select(-year, -month, -day) 

#convert to weekly:

master_df <- data.frame(state = c(), dates = c(), cases = c())

weekly_cases <- cases_by_state %>%
    filter(date >= as.Date("2020-1-27"))

for (state_name in names(table(weekly_cases$state))) {
    state_weekly <- weekly_cases %>%
        filter(state == state_name)
    
    l <- length(state_weekly$date) - length(state_weekly$date)%%7
    
    dates <- c()
    cases <- c()
    
    for (i in 1:l) {
        if ((i-1)%%7 == 0) {
            dates <- c(dates, state_weekly$date[i])
            count <- 0
            
            for (j in 0:6){
                count <- count + state_weekly$cases[i+j]
            }
            
            cases <- c(cases, count)
        }
    }
    
    new_df <- data.frame(state = rep(state_name, length(dates)), dates = as.Date(dates, origin = "1970-1-1"), cases)
    master_df <- rbind(master_df, new_df)
}


# ----------
# Locate Spikes
# ----------

# find standard deviation of weekly confirmed cases among all states on each date:

sd_df <- master_df %>%
    rename(date = dates) %>%
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

dates <- master_df %>% 
    filter(state == "Alabama") %>%
    select(dates)

dates <- as.vector(dates)[[1]]

candidates <- c(candidates, length(dates)) #include the most recent date to catch "ongoing" spikes

candidate_dates <- dates[candidates] 

candidate_dates

# select from candidate spikes graphically:

# input:
selected_candidates <- candidates[c(1,3,5,7,8,10)] 
# -----

selected_dates <- as.Date(dates[selected_candidates], origin = "1970-1-1")

selected <- factor(ifelse(candidates %in% selected_candidates, "selected", "not selected"),
                   levels = c("selected", "not selected"))

selected_df <- data.frame(candidates = as.Date(dates[candidates], origin = "1970-1-1"), selected)

peak_plot <- ggplot() +
    ggtitle("\nSelecting from Candidate Peaks",
            subtitle = paste0(date_title, "\n")) +
    geom_area(data = sd_df, aes(x = date, y = sd), fill = bluesPal[3]) +
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

# measure each state's daily confirmed cases at each spike:

state <- c()
peak <- c()
cases <- c()

for (state_name in names(table(weekly_cases$state))) {
    state <-  c(state, rep(state_name, length(selected_candidates)))
    
    for (j in 1:length(selected_candidates)) {
        peak <- c(peak, paste0("peak_", j))
        cases <- c(cases, master_df$cases[(master_df$state == state_name) & (master_df$dates == dates[selected_candidates[j]])])
    }
}

peaks_df <- data.frame(state, peak, cases) %>%
    spread(peak, cases)

peaks_df <- cbind(state = peaks_df$state, as.data.frame(scale(peaks_df[2:ncol(peaks_df)])))

# select K value:

k_val <- 6

# validate using silhouettes:

set.seed(125)
k <- kmeans(peaks_df[,c(2:(length(selected_candidates) + 1))], k_val)
cluster2 <- case_when(k$cluster == 1 ~ 6,
                      k$cluster == 2 ~ 2,
                      k$cluster == 3 ~ 4,
                      k$cluster == 4 ~ 1,
                      k$cluster == 5 ~ 3,
                      k$cluster == 6 ~ 5,
                      k$cluster == 7 ~ 7,
                      k$cluster == 8 ~ 8,
                      k$cluster == 9 ~ 9,
                      k$cluster == 10 ~ 10)
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
    mutate(date = gsub("_sum_mean", "", date)) %>%
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
    mutate(date = gsub("_sum_mean", "", date)) %>%
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
    scale_color_manual(name = "Cluster",
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
          legend.text = element_text(size = 8)
    )


# ----------
# Export data to Tableau
# ----------

cc <- confirmed_cluster %>%
    select(state, cluster)

cluster_df <- master_df %>%
    left_join(., cc, by = "state")

write.csv(cluster_df, "~/git/covid_tracking/tableau_data.csv")


# ----------
# Heat Map
# ----------

#reformat data to get standardized cases by cluster per spike

heat_df <- peaks_df %>%
    left_join(., cc, by = "state") %>%
    select(-state) %>%
    group_by(cluster) %>%
    summarize_all(mean) %>%
    gather(peak, value, -cluster) %>%
    rowwise() %>%
    mutate(peak = gsub("peak_", "", peak)) %>%
    mutate(peak = as.numeric(peak))

cluster <- c()
peak <- c()
cases <- c()

for (i in 1:peak_count) {
    subset_df <- heat_df %>%
        filter(peak == i)
    
    max_cases <- max(subset_df$value)
    min_cases <- min(subset_df$value)
    
    standardized_cases <- (subset_df$value - min_cases)/(max_cases - min_cases)
    
    cluster <- c(cluster, 1:k_val)
    peak <- c(peak, rep(i, k_val))
    cases <- c(cases, standardized_cases)
}

plot_df <- data.frame(cluster, peak, cases) %>%
    mutate(peak = factor(peak))

#make the plot:

facet_names <- list(
    "1" = "Spike 1", 
    "2" = "Spike 2",
    "3" = "Spike 3",
    "4" = "Spike 4",
    "5" = "Spike 5",
    "6" = "Spike 6"
)

facet_labeller <- function(variable,value){
    return(facet_names[value])
}

contribution_plot <- plot_df %>%
    ggplot(aes(x = 1, y = cluster)) +
    ggtitle("\nEach Cluster's Contribution to the Six Primary Spikes",
            subtitle = date_title) +
    geom_tile(aes(fill = cases)) +
    scale_y_reverse(name = "", breaks = 1:peak_count, labels = c("1: Misc.",
                                                                 "2: The Midwest\n    and Great Plains",
                                                                 "3: Mid-Atlantic, Cascadia,\n    and North New England",
                                                                 "4: The South",
                                                                 "5: Michigan",
                                                                 "6: The Northeast")) +
    scale_fill_gradientn(name = "Share of\nTotal Cases", 
                         colours = redsPal) +
    facet_wrap(~peak, ncol = peak_count, labeller = facet_labeller, strip.position = "top") +
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
          legend.text = element_blank(),
          strip.text = element_text(size = 10, color = "white")
    )

contribution_plot

