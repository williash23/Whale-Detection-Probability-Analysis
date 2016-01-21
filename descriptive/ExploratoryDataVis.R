# Sara Williams
# 11/18/2015
# Detection probability analysis - exploratoty data visualization.
################################################################################

#  Load packages
library(Distance)
library(dplyr)
library(ggplot2)
library(car)

#Load and truncate data to 85th percentile
final_dat <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale-Detection-Probability-Analysis/data/final_dat_2015.csv")

percentile85 <- quantile(final_dat$distance, probs=0.85, na.rm=TRUE)
percentile85
final_dat_truncated85 <- final_dat[final_dat$distance < 4565,]

#Plot number of observations by each level of covariate
group.counts <- table(final_dat_truncated85$count_factor)
group.counts
group.plot <- barplot(group.counts, main="Number of Observations by Group Size", 
                    xlab="Group Size", ylab="Frequency", 
                    ylim=c(0, 2500), cex.main=0.75)

vis.counts <- table(final_dat_truncated85$visibility)
vis.counts
vis.plot <- barplot(vis.counts, main="Number of Observations by Visibility", 
                    xlab="Visibility", ylab="Frequency", 
                    ylim=c(0, 2500), cex.main=0.75)

beh.counts <- table(final_dat_truncated85$whale_behavior)
beh.counts
beh.plot <- barplot(beh.counts, main="Number of Observations by Behavior", xlab="Behavior", ylab="Frequency",
                    ylim=c(0, 2500), cex.main=0.75)

waves.counts <- table(final_dat_truncated85$waves)
waves.counts
sea.plot <- barplot(sea.counts, main="Number of Observations by Sea State", xlab="Sea State", ylab="Frequency",
                   ylim=c(0, 2500))

chisq.test(group.counts, vis.counts)
				   
				   
# subarea.counts <- table(Subarea)
# subarea.counts
# subarea.plot <- barplot(subarea.counts, main="Number of Observations by Subarea", xlab="Subarea", ylab="Frequency",
                   # ylim=c(0, 2000))

# obs.counts <- table(Observer)
# obs.counts
# obs.plot <- barplot(obs.counts, main="Number of Observations by Observer", xlab="Observer", ylab="Frequency",
                    # ylim=c(0, 2000), )


####################################################################################################
#Plot distance as a function of each level of covariate

#Ship Speed
plot(final_dat_truncated85$ship_speed_scaled, final_dat_truncated85$distance, main="First Sighting Distance by Ship Speed", 
     xlab="Ship Speed", ylab="Distance (m)")
abline(lm(final_dat_truncated85$distance~final_dat_truncated85$ship_speed_scaled), col="red")
summary(lm(final_dat_truncated85$distance~final_dat_truncated85$ship_speed_scaled))

#Visibility
boxplot(distance~visibility,data=final_dat_truncated85, main="First Sighting Distance by Visibility", 
        xlab="Visbility", ylab="Distance (m)")

#Behavior
boxplot(distance~whale_behavior,data=final_dat_truncated85, main="First Sighting Distance by Behavior", 
        xlab="Behavior", ylab="Distance (m)")

#Sea State
boxplot(distance~waves,data=final_dat_truncated85, main="First Sighting Distance by Sea State", 
        xlab="Wave height", ylab="Distance (m)")

#Group Size
boxplot(distance~count_factor,data=final_dat_truncated85, main="First Sighting Distance by Group Size", 
        xlab="Group Size", ylab="Distance (m)")


		
		
		
		
		
		
# #Azimuth
# plot(BB_MM_Azi, distance, main="First Sighting Distance by Azimuth to Whale", 
     # xlab="Azimuth from Bulbous Bow to  Whale", ylab="Distance (m)")
# abline(lm(distance~BB_MM_Azi), col="red")

# #Vessel Depth ###### ????????????
# new_depth <- abs(final_data$VslDepth_m)
# depth <- cbind(distance, new_depth)
# depth <- subset(depth, new_depth < 1000)
# depth <- as.data.frame(depth)
# summary(depth)

# plot(depth$new_depth, depth$distance, main="First Sighting Distance by Vessel Depth", 
     # xlab="Vessel Depth (m)", ylab="Distance (m)")
# abline(lm(depth$distance~depth$new_depth), col="red")
# summary(lm(depth$distance~depth$new_depth))

# #Observer
# boxplot(distance~Observer,data=final_data, main="First Sighting Distance by Observer", 
        # xlab="Observer", ylab="Distance (m)")
		
# #Subarea
# boxplot(distance~Subarea,data=final_data, main="First Sighting Distance by Subarea", 
        # xlab="Subarea", ylab="Distance (m)")

