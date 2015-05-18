#Read in raw data file
library(Distance)
setwd("C:/Users/sara.williams/Documents/GitHub/Whale_DetectionProbability")

#################DETECTION FUNCTION MODELS#############################################################
#######################################################################################################

#no covariates code example
#ds(simple, 
   #transect = c("point"), 
   #formula = ~1, 
   #key = c("hr"),
   #adjustment = c("cos"), 
   #order = 3,
   #scale = c("width", "scale"), 
   #monotonicity = ifelse(formula == ~1, "strict", "none"),
   #initial.values = NULL)

########################################################################################################
#Using binned data
final_data <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale_DetectionProbability/data/final_data_14_CountPooled.csv")
cutpoints<-c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000)
bin_data<-create.bins(final_data, cutpoints)
bin_data$Count <- factor(bin_data$Count, levels = c("1", "2 to 3", "large"))

#no covariate --- binned data
ds<-ds(data = bin_data,
              formula= ~1,
              transect="point",
              key="hr",
              adjustment=NULL)
summary(ds)
#plot(ds, main="Detection Function using Binned Data")

# #ship speed
# ds.speed<-ds(data = bin_data, 
					# formula= ~1+Kts_scaled,
					# transect="point",
					# key="hr",
					# adjustment=NULL)
# summary(ds.speed)
# #plot(ds.speed, main="Detection as Function of Ship Speed")

#Visibility
ds.vis<-ds(data = bin_data, 
           formula= ~1+Visibility,
           transect="point",
           key="hr",
           adjustment=NULL)
summary(ds.vis)
#plot(ds.vis, main="Detection as Function of Visibility")

#Behavior
ds.beh<-ds(data = bin_data, 
           formula= ~1+Behavior,
           transect="point",
           key="hr",
           adjustment=NULL)
summary(ds.beh)
#plot(ds.beh, main="Detection as Function of Behavior")

#Sea State
ds.sea<-ds(data = bin_data, 
           formula= ~1+SeaState,
           transect="point",
           key="hr",
           adjustment=NULL)
summary(ds.sea)
#plot(ds.sea, main="Detection as Function of Sea State")

#Group size - Count is factor; greater than 4 -> 4
ds.count<-ds(data = bin_data, 
             formula= ~1+Count,
             transect="point",
             key="hr",
             adjustment=NULL)
summary(ds.count)
#plot(ds.count, main="Detection as Function of Group Size")

#All covariates with binned data and scaled ship speed
ds.all<-ds(data = bin_data, 
             formula= ~1+Count+Behavior+Visibility,
			 transect="point",
             key="hr",
             adjustment=NULL)
summary(ds.all)

########################################################################################################
########################################################################################################
########################################################################################################
###Using not binned data
final_data <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale_DetectionProbability/data/final_data.csv")		
	
#no covariate
ds<-ds(data = final_data,
              formula= ~1,
			  transect="point",
              key="hr",
              adjustment=NULL)
summary(ds)
#plot(ds, main="Detection Function")

#ship speed
ds.speed<-ds(data = final_data, 
             formula= ~1+Kts_scaled,
             transect="point",
             key="hr",
             adjustment=NULL)
summary(ds.speed)
#plot(ds.speed, main="Detection as Function of Ship Speed")

#Visibility
ds.vis<-ds(data = final_data, 
           formula= ~1+Visibility,
           transect="point",
           key="hr",
           adjustment=NULL)
summary(ds.vis)
#plot(ds.vis, main="Detection as Function of Visibility")

#Behavior
ds.beh<-ds(data = final_data, 
           formula= ~1+Behavior,
           transect="point",
           key="hr",
           adjustment=NULL)
summary(ds.beh)
#plot(ds.beh, main="Detection as Function of Behavior")

#sea State
ds.sea<-ds(data = final_data, 
           formula= ~1+SeaState,
           transect="point",
           key="hr",
           adjustment=NULL)
summary(ds.sea)
#plot(ds.sea, main="Detection as Function of Sea State")

#Group size - Count is integer
ds.count<-ds(data = final_data, 
             formula= ~1+Count,
             transect="point",
             key="hr",
             adjustment=NULL)
summary(ds.count)
#plot(ds.count, main="Detection as Function of Group Size")

# #ship 
# ds.ship<-ds(data = final_data, 
            # formula=~1+Ship,
            # transect="point",
            # key="hr",
            # adjustment=NULL)
# summary(ds.ship)
# #plot(ds.ship, main="Detection as Function of Ship")

