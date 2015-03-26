#Read in raw data file 
data <- read.csv("C:/Users/sara.williams/Documents/UM/Analyses/DetectionProbability/UpdatedData_Mar_2015/Whales_0613_FirstSightings.csv")

######################################################################
###First sighting records only already subsetted in Access
#######################################################################

#View raw data file histogram
bins <- seq(0,10000,by=400)
hist(data$distance, breaks=bins, main="First Sighting Distance - All data", 
	 xlim=c(0,10000), ylim=c(0,800),
     xlab="Distance (m)", ylab="Frequency")

#Summary statistics and outliers
summary(data$distance)
boxplot(data$distance)
#Make quantiles and intervals
Q3 <- quantile(data$distance,0.75)
Q1 <- quantile(data$distance,0.25)
IQR <- Q3-Q1
I1 <- Q1-1.5*IQR
I2 <- Q3+1.5*IQR

#Which values are low outliers
data$distance[which(data$distance<I1)]

#Which values are high outliers
high.out <- data$distance[which(data$distance>I2)]
min(high.out)

##############################################################################
#DID NOT USE THIS METHOD
#Truncate data by REMOVING HIGH OUTLIERS 
data_trunc_outliers <- subset(data,data$distance < 4154.526)
summary(data_trunc_outliers$distance)

#View truncated data file histogram
bins_trunc <- seq(0,4200,by=200)
hist(data_trunc_outliers$distance, breaks=bins_trunc, 
     main="First Sighting Distance - Outliers removed", 
     xlim=c(0,4200), ylim=c(0,500), xlab="Distance (m)", cex.main=0.75)

######OR######################################################################

#Truncate data according to rule of thumb (10% of largest observations)
percentile90 <- quantile(data$distance, probs=0.90, na.rm=TRUE)
percentile90
percentile95 <- quantile(data$distance, probs=0.95, na.rm=TRUE)
percentile95

#Truncate data at 90th percentile
data_trunc_90 <- subset(data,data$distance < percentile90)
boxplot(data_trunc_90$distance)
summary(data_trunc_90$distance)

#View truncated data file histogram
#View(data_trunc)
bins_trunc <- seq(0,5000,by=200)
hist(data_trunc_90$distance, breaks=bins_trunc, 
     main="First Sighting Distance - Data truncated at 90th Percentile", 
     xlim=c(0,5000), ylim=c(0,400), xlab="Distance (m)", cex.main=0.75)

#Subset data
#Determine which categorical covariates have very few observations and at which levels
attach(data_trunc_90)
levels(Visibility)
table(Visibility)

levels(Behavior)
table(Behavior)

levels(Observer)
table(Observer)

levels(SeaState)
table(SeaState)


#Remove via subset covariate factor levels where there were very few or oddly categorized observations
#Subset data to remove observations that are weird...
data_trunc_90b <- data_trunc_90 [! data_trunc_90$Visibility %in% c("unknown", "moderate"), ]
data_trunc_90c <- data_trunc_90b [! data_trunc_90b$Behavior %in% c("FL-Floating", "SW-Swimming", 
                                                                   "UN-Unsure", "DO-Dove"), ]
data_trunc_90d <- data_trunc_90c [! data_trunc_90c$Observer %in% c("Scott"), ]
data_trunc_90e <- data_trunc_90d [! data_trunc_90d$SeaState %in% c("unknown", "1-2'", "2-3'", "3-4'", "flat calm", 
                                                                   "glassy calm", "0"), ]

#Reassign covariates as factors to get rid of labels that now have no observations
data_trunc_90e$Visibility <- factor(data_trunc_90e$Visibility)
data_trunc_90e$Behavior <- factor(data_trunc_90e$Behavior)
data_trunc_90e$Observer <- factor(data_trunc_90e$Observer)
data_trunc_90e$SeaState <- factor(data_trunc_90e$SeaState)

#Attach column names and check covariate levels
detach(data_trunc)
attach(data_trunc_90e)
levels(Visibility)
table(Visibility)

levels(Behavior)
table(Behavior)

levels(Observer)
table(Observer)

levels(SeaState)
table(SeaState)

#Create data frame from cleaned data set
#### first_obs is data set made from observations of ObType Sinlge and ObType MultiOb but only using ObOrder 1 (First Sighting)
first_obs <- as.data.frame(data_trunc_90e)

#Save Rdata file and CSV file
setwd("C:/Users/sara.williams/Documents/UM/Analyses/DetectionProbability/UpdatedData_Mar_2015")
save(first_obs, file="new_first_obs.RData")
write.csv(first_obs, file = "new_first_obs.csv")


###############################################################
#To combine behaviors dive-no-fluke and blow and create a smaller dataset - fewer variables
first_obs$Behavior[first_obs$Behavior == 'DN-Dive-no-fluke'] <- 'BL-Blowing'
#Drop now unused factor level
first_obs$Behavior <- factor(first_obs$Behavior)

#Smaller dataframe
myvars <- c("Ship", "Count_", "Vessel_Kts", "distance", "Behavior", "Visibility", "SeaState", "Year_")
final_data <- first_obs[myvars]
save(final_data, file="final_data.RData")
write.csv(final_data, file = "final_data.csv")

