
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

date_title <- "April 14th"
end_date <- as.Date("2021-4-13")
customPal <- c(magma(20)[c(20,17,14,11,8)], "white", "gray60", "gray30", "red")


# ----------
# Data import/cleaning
# ----------

# import time series data:

confirmed_data <- read.csv("~/git/covid_tracking/confirmed_2021_4_14.csv", stringsAsFactors=FALSE)  %>%
    filter(!(Province_State %in% c("American Samoa", "Diamond Princess", "Grand Princess", "Guam", 
                                   "Northern Mariana Islands", "Puerto Rico", "Virgin Islands"))) %>%
    select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Country_Region, -Lat, -Long_, -Combined_Key) %>%
    rename(state = Province_State) %>%
    group_by(state) %>%
    summarise_all(list(sum = sum))

deaths_data <- read.csv("~/git/covid_tracking/deaths_2021_4_14.csv", stringsAsFactors = FALSE) %>%
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


# ----------
# Outliers
# ----------

# determine number of outlier dates by size of sliding window:

window <- c()
outliers <- c()

for (i in 5:100) {
    ws2 <- c()
    
    for (j in confirmed_cluster$state) {
        c <- confirmed_daily_data %>%
            filter(state == j) %>%
            select(-state, -pop) %>%
            gather(date, value) %>%
            mutate(date = as.Date("2020-1-23"):end_date) %>%
            mutate(date = as.Date(date, origin = "1970-1-1"))
        
        ws <- c()
        
        for (k in 1:(nrow(c) - i)) {
            c_subset <- c$value[k:(k + i - 1)]
            q3 <- fivenum(c_subset)[4]
            q1 <- fivenum(c_subset)[2]
            IQR <- q3 - q1
            
            w <- c(which(c_subset > q3 + 1.5*IQR), which(c_subset < q1 - 1.5*IQR))
            ws <- c(ws, w + k)
        }
    }
    
    window <- c(window, i)
    ws2 <- c(ws2, unique(ws))
    outliers <- c(outliers, length(ws2))
    
    print(i)
    
}

outlier_df <- data.frame(window, outliers)

# visualize relationship between window size and number of outlier dates:

outlier_plot <- outlier_df %>%
    ggplot(aes(x = window, y = outliers)) +
    ggtitle("\nWindow Size for Outlier Detection",
            subtitle = paste0(date_title, "\n")) +
    geom_line(color = plasma(10)[4]) +
    geom_smooth(color = "white", size = .8, span = .1, se = FALSE) +
    scale_x_continuous(name = "\nwindow size\n",
                       limits = c(0, 100),
                       breaks = seq(0, 100, 20)) +
    scale_y_continuous(name = "dates \nwith \noutliers ",
                       limits = c(0, 100),
                       breaks = seq(0, 100, 25)) +
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

outlier_plot

# selected threshold, determine outlier dates with that threshold

i <- 38

for (j in confirmed_cluster$state) {
    c <- confirmed_daily_data %>%
        filter(state == j) %>%
        select(-state, -pop) %>%
        gather(date, value) %>%
        mutate(date = as.Date("2020-1-23"):end_date)) %>%
        mutate(date = as.Date(date, origin = "1970-1-1"))
    ws <- c()
    
    for (k in 1:(nrow(c) - i)) {
        c_subset <- c$value[k:(k + i - 1)]
        q3 <- fivenum(c_subset)[4]
        q1 <- fivenum(c_subset)[2]
        IQR <- q3 - q1
        
        w <- c(which(c_subset > q3 + 1.5*IQR), which(c_subset < q1 - 1.5*IQR))
        ws <- c(ws, w + k)
    }
}

ws2 <- unique(ws)

outlier_dates <- c$date[ws2]


# ----------
# Principal Component Analysis
# ----------

# run PCA:

set.seed(125)
confirmed_prcomp <- confirmed_daily_data[,-(ws2 + 2)] %>%
    select(-state, -pop) %>%
    prcomp()

confirmed_pca <- data.frame(state = confirmed_daily_data$state,
                            PC1 = confirmed_prcomp$x[, 1],
                            PC2 = confirmed_prcomp$x[, 2],
                            PC3 = confirmed_prcomp$x[, 3],
                            PC4 = confirmed_prcomp$x[, 4],
                            PC5 = confirmed_prcomp$x[, 5],
                            PC6 = confirmed_prcomp$x[, 6],
                            PC7 = confirmed_prcomp$x[, 7],
                            PC8 = confirmed_prcomp$x[, 8]) %>%
    mutate(PC1 = scale(PC1)) %>%
    mutate(PC2 = scale(PC2)) %>%
    mutate(PC3 = scale(PC3)) %>%
    mutate(PC4 = scale(PC4)) %>%
    mutate(PC5 = scale(PC5)) %>%
    mutate(PC6 = scale(PC6)) %>%
    mutate(PC7 = scale(PC7)) %>%
    mutate(PC8 = scale(PC8))

# visualize cumulative variance:

pca_var <- data.frame(PC = 0:8,
                      var = c(0, as.vector(summary(confirmed_prcomp)$importance[3,1:8])))

var_plot <- ggplot(pca_var, aes(x = PC, y = var)) +
    ggtitle("\nPrinciple Component Importance",
            subtitle = paste0(date_title, "\n")) +
    geom_line(color = plasma(10)[4], size = .8) +
    scale_x_continuous(name = "\nPC\n",
                       limits = c(0, 8),
                       breaks = seq(0, 8, 2)) +
    scale_y_continuous(name = "cumulative \nproportion \nof variance ",
                       limits = c(0, 1),
                       breaks = seq(0, 1, .25)) +
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

# visualize PC rotations:

included_dates <- as.Date("2020-1-23"):end_date
included_dates <- included_dates[!(included_dates %in% outlier_dates)]

pc_rotations <- as.data.frame(confirmed_prcomp$rotation[,1:8]) %>%
    mutate(date = included_dates) %>%
    mutate(date = as.Date(date, origin = "1970-1-1")) %>%
    gather(pc, value, -date)

r <- range(pc_rotations$value)
rlim <- max(abs(r))

pc_rotations_plot <- pc_rotations %>%
    ggplot(aes(x = date, y = pc)) +
    ggtitle("\nPrinciple Component Rotations",
            subtitle = paste0(date_title, "\n")) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradientn(name = "Rotation", limits = c(-rlim, rlim), 
                         colours = c("#B2182B","White","#244999")) +
    scale_y_discrete(name = "principal \ncomponent ", 
                     limits = rev(names(as.data.frame(confirmed_prcomp$rotation[,1:8])))) +
    scale_x_date(name = "\n\n", date_breaks = "2 months", date_labels = "%b") +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          text = element_text(color = "white", family = "Avenir"), 
          axis.text = element_text(color = "white"),
          axis.title.y = element_text(angle = 0, vjust = .5, hjust = 1),
          plot.title = element_text(family = "Avenir Black", hjust = .5, size = 14), 
          plot.subtitle = element_text(family = "Avenir", hjust = .5, size = 10),
          plot.background = element_rect(fill = "black", color = "black"), 
          legend.background = element_rect(fill = "black"), 
          legend.key = element_rect(fill = "black"),
          legend.key.size = unit(.6, "cm"),
          legend.text = element_text(size = 8))

pc_rotations_plot


# ----------
# Clustering
# ----------

# grid search for ideal k and number of PC's to include:

pcs_col <- c()
k_val_col <- c()
error <- c()

for (pcs in 2:8) {
    for (k_val in 2:6) {
        set.seed(125)
        k <- kmeans(confirmed_pca[,c(2:(pcs+1))], k_val)
        cluster <- factor(k$cluster)
        confirmed_cluster <- cbind(confirmed_pca, cluster)
        
        cluster_sd <- confirmed_cluster %>%
            select(state, cluster) %>%
            left_join(., confirmed_daily_data, by = "state") %>%
            select(-state, -pop) %>%
            group_by(cluster) %>%
            summarise_all(list(sd = sd)) %>%
            gather(date, cases, -cluster) %>%
            mutate(cases = 1000000*cases) %>%
            rowwise() %>%
            mutate(date = gsub("X", "", date)) %>%
            mutate(date = gsub("_sum.1_sd", "", date)) %>%
            mutate(month = strsplit(date, ".", fixed = TRUE)[[1]][1]) %>%
            mutate(day = strsplit(date, ".", fixed = TRUE)[[1]][2]) %>%
            mutate(year = strsplit(date, ".", fixed = TRUE)[[1]][3]) %>%
            mutate(date = as.Date(paste0("20", year, "-", month, "-", day))) %>%
            select(-year, -month, -day) %>%
            filter(!(is.na(cases))) %>%
            group_by(date) %>%
            mutate(cluster = NA) %>%
            summarise_all(list(mean = mean)) %>%
            select(date, cases_mean) %>%
            summarise(sum(cases_mean))
        
        pcs_col <- c(pcs_col, pcs)
        k_val_col <- c(k_val_col, k_val)
        error <- c(error, as.numeric(cluster_sd))
    }
}

grid_df <- data.frame(pcs = pcs_col, k_val = k_val_col, error)

# (selected values)
selected <- c(4, 6)

grid_plot <- grid_df %>%
    mutate(error = round(error)) %>%
    ggplot(aes(x = pcs, y = k_val)) +
    ggtitle("\nGrid Search Clustering Optimization",
            subtitle = paste0(date_title, "\n")) +
    geom_tile(aes(fill = error)) +
    geom_text(aes(label = error)) +
    geom_segment(x = selected[1] - .5, xend = selected[1] + .5, 
                 y = selected[2] + .5, yend = selected[2] + .5,
                 color = "white", size = .8) +
    geom_segment(x = selected[1] - .5, xend = selected[1] + .5, 
                 y = selected[2] - .5, yend = selected[2] - .5,
                 color = "white", size = .8) +
    geom_segment(x = selected[1] + .5, xend = selected[1] + .5, 
                 y = selected[2] - .5, yend = selected[2] + .5,
                 color = "white", size = .8) +
    geom_segment(x = selected[1] - .5, xend = selected[1] - .5, 
                 y = selected[2] - .5, yend = selected[2] + .5,
                 color = "white", size = .8) +
    scale_y_continuous(name = "number \nof \nclusters ",
                       breaks = seq(2,6,2)) +
    scale_x_continuous(name = "\nnumber of PC's included\n") +
    scale_fill_gradientn(name = "Mean\nIntra-cluster\nDeviance",
                         colours = c(plasma(10)[4], "White")) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          text = element_text(color = "white", family = "Avenir"), 
          axis.text = element_text(color = "white"),
          axis.title.y = element_text(angle = 0, vjust = .5, hjust = 1),
          plot.title = element_text(family = "Avenir Black", hjust = .5, size = 14), 
          plot.subtitle = element_text(family = "Avenir", hjust = .5, size = 10),
          plot.background = element_rect(fill = "black", color = "black"), 
          legend.background = element_rect(fill = "black"), 
          legend.key = element_rect(fill = "black"),
          legend.key.size = unit(.6, "cm"),
          legend.text = element_text(size = 8))

grid_plot

# validate using silhouettes:

set.seed(125)
k <- kmeans(confirmed_pca[,c(2:(selected[1]+1))], selected[2])

# (reassign cluster numbers for consistency over time)
cluster2 <- case_when(k$cluster == 1 ~ 1,
                      k$cluster == 2 ~ 5,
                      k$cluster == 3 ~ 4,
                      k$cluster == 4 ~ 3,
                      k$cluster == 5 ~ 6,
                      k$cluster == 6 ~ 2)
cluster <- factor(cluster2)
confirmed_cluster <- cbind(confirmed_pca, cluster)

sil <- silhouette(cluster2, dist(confirmed_pca[,c(2:(selected[1]+1))]))
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

# visualize state clusters map:

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

# visualize trends by cluster:

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
    geom_vline(xintercept = as.Date("2021-1-1"), size = .8, color = "gray45") +
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








