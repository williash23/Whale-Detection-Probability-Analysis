#Load and attach data
setwd("C:/Users/sara.williams/Documents/GitHub/Whale_DetectionProbability")
final_data <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale_DetectionProbability/data/final_data.csv")		

#################
#Not binned data
attach(final_data)
#################
#Binned Data
attach(bin_data)
####################################################################################################
#Exploratory Analysis

######################################################################################################
#Plot number of observations by each level of covariate
group.counts <- table(Count_)
group.counts
group.plot <- barplot(group.counts, main="Number of Observations by Group Size", 
                    xlab="Group Size", ylab="Frequency", 
                    ylim=c(0, 2500), cex.main=0.75)

vis.counts <- table(Visibility)
vis.counts
vis.plot <- barplot(vis.counts, main="Number of Observations by Visibility", 
                    xlab="Visibility", ylab="Frequency", 
                    ylim=c(0, 2500), cex.main=0.75)

beh.counts <- table(Behavior)
beh.counts
beh.plot <- barplot(beh.counts, main="Number of Observations by Behavior", xlab="Behavior", ylab="Frequency",
                    ylim=c(0, 2500), cex.main=0.75)

sea.counts <- table(SeaState)
sea.counts
sea.plot <- barplot(sea.counts, main="Number of Observations by Sea State", xlab="Sea State", ylab="Frequency",
                   ylim=c(0, 2500))

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
plot(Vessel_Kts, distance, main="First Sighting Distance by Ship Speed", 
     xlab="Ship Speed", ylab="Distance (m)")
abline(lm(distance~Vessel_Kts), col="red")
summary(lm(distance~Vessel_Kts))

#Visibility
boxplot(distance~Visibility,data=final_data, main="First Sighting Distance by Visibility", 
        xlab="Visbility", ylab="Distance (m)")

#Behavior
boxplot(distance~Behavior,data=final_data, main="First Sighting Distance by Behavior", 
        xlab="Behavior", ylab="Distance (m)")

#Sea State
boxplot(distance~SeaState,data=final_data, main="First Sighting Distance by Sea State", 
        xlab="Sea State", ylab="Distance (m)")

#Group Size
plot(Count_, distance, main="First Sighting Distance by Group Size", 
     xlab="Group Size", ylab="Distance (m)")
abline(lm(distance~Count_), col="red")
summary(lm(distance~Count_))

boxplot(distance~Count_,data=final_data, main="First Sighting Distance by Group Size", 
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




####################################################################################################
