#Read in raw data file
library(Distance)

setwd("C:/Users/sara.williams/Documents/GitHub/Whale_DetectionProbability")
final_data <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale_DetectionProbability/data/final_data.csv")		
	
#################DETECTION FUNCTION MODELS###############################
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

#####################################################
#Using binned data
#Create bins
cutpoints <- c(0,500, 1500, 2500, 3500, 4500)
bin_data <- create.bins(final_data, cutpoints)
#no covariate --- binned data
ds<-ds(data = bin_data,
              formula= ~1,
              transect="point",
              key="hr",
              adjustment=NULL)
summary(ds)
plot(ds, main="Detection Function using Binned Data")
#######################################################

#no covariate
ds<-ds(data = final_data,
              formula= ~1,
              transect="point",
              key="hr",
              adjustment=NULL)
summary(ds)
plot(ds, main="Detection Function")


#ship speed
ds.speed<-ds(data = final_data, 
             formula= ~1+Vessel_Kts,
			 truncation=list(left="0%",right="0%"),
             transect="point",
             key="hr",
             adjustment=NULL)
summary(ds.speed)
#plot(ds.speed, main="Detection as Function of Ship Speed")

#Visibility
ds.vis<-ds(data = final_data, 
           formula= ~1+Visibility,
           truncation=list(left="0%",right="0%"),
           transect="point",
           key="hr",
           adjustment=NULL)
summary(ds.vis)
#plot(ds.vis, main="Detection as Function of Visibility", 
     cex.lab = 0.75, cex.main = 1, cex.axis=0.75, font.lab=2)

#Behavior
ds.beh<-ds(data = final_data, 
           formula= ~1+Behavior,
           truncation=list(left="0%",right="0%"),
           transect="point",
           key="hr",
           adjustment=NULL)
summary(ds.beh)
#plot(ds.beh, main="Detection as Function of Behavior")

#sea State
ds.sea<-ds(data = final_data, 
           formula= ~1+SeaState,
           truncation=list(left="0%",right="0%"),
           transect="point",
           key="hr",
           adjustment=NULL)
summary(ds.sea)
#plot(ds.sea, main="Detection as Function of Sea State")

#Group size - Count is integer
ds.count<-ds(data = final_data, 
             formula= ~1+Count_,
             truncation=list(left="0%",right="0%"),
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
            # adjustment=NULL,
            # monotonicity=FALSE)
# summary(ds.ship)
# #plot(ds.ship, main="Detection as Function of Ship")

#######################################################################################
#Interaction models

#All
ds.2<-ds(data = final_data, 
         formula= ~1+SeaState + Visibility,
         transect="point",
         key="hr",
         adjustment=NULL,
         monotonicity=FALSE)
summary(ds.2)

ds.3<-ds(data = final_data, 
               formula= ~1+SeaState + Visibility + Behavior,
               transect="point",
               key="hr",
               adjustment=NULL,
               monotonicity=FALSE)
summary(ds.3)

ds.4<-ds(data = final_data, 
         formula= ~1+SeaState + Visibility + Behavior + Count_,
         transect="point",
         key="hr",
         adjustment=NULL,
         monotonicity=FALSE)
summary(ds.4)

plot(ds.4, main="Detection as Function of All Covariates")

#Visibility*Behavior
ds.vis.beh<-ds(data = data, 
               formula= ~1+Behavior*Visibility,
               truncation=list(left="0%",right="0%"),
               transect="point",
               key="hr",
               adjustment="cos")
summary(ds.vis.beh)
plot(ds.vis.beh, main="Detection as Function of Visibility-Behavior Interaction")


#Visibility*Group Size
ds.vis.count<-ds(data = data, 
                 formula= ~1+Count*Visibility,
                 truncation=list(left="0%",right="0%"),
                 transect="point",
                 key="hr",
                 adjustment="cos")
summary(ds.vis.count)
plot(ds.vis.count, main="Detection as Function of Visibility-Group size Interaction")

#Group size*Behavior
ds.count.beh<-ds(data = data, 
                 formula= ~1+Count*Behavior,
                 truncation=list(left="0%",right="0%"),
                 transect="point",
                 key="hr",
                 adjustment="cos")
summary(ds.count.beh)
plot(ds.count.beh, main="Detection as Function of Group size-Behavior Interaction")