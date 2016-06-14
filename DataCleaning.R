# Sara Williams
# 11/18/2015
# Detection probability analysis - data cleaning.
################################################################################

#  Load packages
library(Distance)
library(dplyr)
library(ggplot2)
################################################################################

#  Read in raw data file 
temp <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale-Detection-Probability-Analysis/data/Whales_0615_general_clean.csv")

#  First sighting only and remove erroneous locations.
temp2 <- temp %>%
           filter(whale_dist_shore_m > 0) %>%
           filter(whale_depth_m <= 0) %>%
           filter(ob_order_time == 1) %>%
           filter(year > 2007) %>%
           as.data.frame(.)

temp3 <- temp2 %>%
           filter(whale_behavior=="BL-Blowing" |whale_behavior=="DF-Dive-fluke-up" | 
                 whale_behavior=="DN-Dive-no-fluke" | whale_behavior=="LF-Lunge-feed" | 
                 whale_behavior=="RE-Resting" | whale_behavior=="SA-Surface-active") %>%
          filter (visibility == "excellent" |  visibility == "good" |visibility == "poor" | visibility == "poor-fog" | 
                 visibility == "excellent") %>%
          filter(observer != "Scott") %>%
          filter(waves == "1'" | waves == "2'" | waves == "3'" |waves == "4'" | waves == "calm") %>%
          as.data.frame(.)
dat <- droplevels(temp3)

#  Change name of column with distances
names(dat)[41] <- "distance"
################################################################################
              
#  Generate smaller dataframe
myvars <- c("unique_event_ID", "same_whale_ID", "ship", "observer_location", "count", "ship_speed", "distance", "whale_behavior", "visibility", 
            "waves", "X_whale_UTM", "Y_whale_UTM", "wind", "ship_whale_bearing", "approx", "year", "cruise_event_ID", "TimeTxt")
final_dat <- dat[myvars]

#  Create a new column for scaled ship speed data (mean = 0, sd =1)
final_dat$ship_speed_scaled <- as.numeric(scale(final_dat$ship_speed, T))

#   Add column where count is a factor.
final_dat <- final_dat %>%
            mutate(count_factor = count)
final_dat$count_factor[final_dat$count > 3] <- "4+"
final_dat$count_factor[final_dat$count == 3] <- "2-3"
final_dat$count_factor[final_dat$count == 2] <- "2-3"
final_dat$count_factor <- as.factor(final_dat$count_factor)
final_dat <- as.data.frame(final_dat)

#  Combine behaviors "dive-no-fluke" and "blow" 
final_dat$whale_behavior[final_dat$whale_behavior == "DN-Dive-no-fluke"] <- "BL-Blowing"
#   Drop now unused factor level
final_dat <- droplevels(final_dat)

#  Write and save csv file.
# write.csv(final_dat, file = "final_dat_2015.csv")

#  Find 85th percentile  and truncate sightings to this distance
percentile85 <- quantile(final_dat$distance, probs=0.85, na.rm=TRUE)
percentile85
final_dat_truncated85 <- final_dat[final_dat$distance < 4565,]

#  Write and save csv file.
# write.csv(final_dat_truncated85, file = "final_dat_truncated85.csv")
################################################################################

#  View raw data and final_dat_truncated85 histogram

plot_dist <- ggplot(dat, aes(x=distance)) + 
                    geom_histogram(fill="grey", 
                    colour = "grey" ,
                    binwidth=200) +
                    #geom_density(alpha=.2, fill="#FF6666") +
                    xlim(0, 12000) +
                    ylim(0,400) +
                    xlab("Distances of first sightings (m)") +
                    ylab("Count") +
                    theme_bw()
plot_dist

#  Summary statistics and outliers
summary(dat$distance)
boxplot(dat$distance)

plot_dist <- ggplot(final_dat_truncated85, aes(x=distance)) + 
                    geom_histogram(fill= "#46ACC8", 
                    colour ="black", binwidth=500) +
                    xlim(0,5000) +
                    ylim(0, 800) +
                    xlab("
                    Distance from ship (m)") +
                    ylab("Frequency
                    ") +
                    theme_bw() +
                    theme(panel.grid.major.x=element_blank(),
                    panel.grid.minor=element_blank()) +
                    theme(axis.title=element_text(size=15), 
                    axis.text=element_text(size=12)) +
                    ggtitle("Detection as a function of distance") +
                    theme(plot.title = element_text(lineheight=1, face="bold", size =20, vjust =2))
plot_dist
################################################################################
