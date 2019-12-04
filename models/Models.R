#  12/23/2015
#  Detection probability analysis - estimating detection probability and 
#   running various models with covariates.
################################################################################

#  Load packages.
library(Distance)
library(dplyr)

#  Read in final data set. NOTE that 85th percentil trucation is done WITHIN the ds() function, 
#   so we do NOT use final_dat_truncated85
final_dat <- read.csv("C:/Users/saraw/Documents/Whales/final_dat_2015.csv")

#  Example model call
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

#  No covariates - hazard rate
ds<-ds(data = final_dat,
        formula= ~1,
        truncation="15%",
        transect="point",
        key="hr",
        adjustment=NULL)
summary(ds)

#  Must download mrds package v. 2.1.15 to get radial distance PDF
plot(ds, pdf = TRUE, pl.den = 0,  breaks = c(0, 250, 750, 1250, 1750, 2250, 2750, 3250, 3750, 4250, 4564.46), pch = 16)
#title(main = NULL, xlab = "Radial first sighting distance (m)")
#  Changing breaks and number of distance classes has no effect on p-values of GOF tests
ddf.gof(ds$ddf, breaks = seq(0, 4564.46, 200))

# radial distance with equal distance bins, but don't create break at the heaps. On y-axis is g(x) = radial distance detection function, derivative of detection probability function === probability density function!!!
# radial detection plot
#  really big sample size and heaping are causing low p-values and lack of fit
#  standard chi-square test with bins - pooling data. 



 # CvM test statistic =  1.0671  P =  0.0017144
# # # AIC: 54288.47
# # # Scale Coefficients:  
# # #                          estimate           se
# # # (Intercept)                6.726756       0.04511843
# # # Shape parameters:  
 # # #                           estimate           se
# # # (Intercept)             0.7371793   0.02403831


#  Visibility
ds.vis<-ds(data = final_dat, 
            formula= ~1+visibility,
            truncation="15%",
            transect="point",
            key="hr",
            adjustment=NULL)
summary(ds.vis)
ddf.gof(ds.vis$ddf, asp=1)

# CvM test statistic =  0.84976  P =  0.0055666  
# # # AIC: 54221.6 
# # # Scale Coefficients:  
# # #                          estimate           se
# # # (Intercept)                6.85951497   0.04457347
# # # visibilitygood          0.04977762   0.06111292
# # # visibilitypoor            -0.61123947   0.10312669
# # # visibilitypoor-fog       -1.20921171   0.29163640
# # # Shape parameters:  
# # #                           estimate           se
# # # (Intercept)             0.7798358     0.02438796


#  Behavior
ds.beh<-ds(data = final_dat, 
            formula= ~1+whale_behavior,
            truncation="15%",
            transect="point",
            key="hr",
            adjustment=NULL)
summary(ds.beh)
ddf.gof(ds.beh$ddf, asp=1)

# CvM test statistic =  0.81504  P =  0.0067347
# # # AIC: 54277.5
# # # Scale Coefficients:  
# # #                                        estimate           se
# # # (Intercept)                              6.7315668     0.04828056
# # # whale_behaviorDF-Dive-fluke-up   -0.0352727     0.06373799
# # # whale_behaviorLF-Lunge-feed          0.3444508    0.69140831
# # # whale_behaviorRE-Resting              -0.1046447     0.18424319
# # # whale_behaviorSA-Surface-active    0.5233363     0.11744317
# # # Shape parameters:  
# # #                           estimate           se
# # # (Intercept)             0.7472793     0.0241749


#  Waves
ds.waves<-ds(data = final_dat, 
               formula= ~1+waves,
               truncation="15%",
               transect="point",
               key="hr",
               adjustment=NULL)
summary(ds.waves)
ddf.gof(ds.waves$ddf, asp=1)

# CvM test statistic =  0.83485  P =  0.0060405 
# # # AIC: 54290.68
# # # Scale Coefficients:  
# # #                          estimate           se
# # # (Intercept)                6.71945692   0.06166379
# # # waves2'                  0.04229350   0.09909780
# # # waves3'                  0.30627212   0.20179164
# # # waves4'                 0.58712044   0.57729616
# # # wavescalm             -0.01670241   0.06206140
# # # Shape parameters:  
# # #                           estimate           se
# # # (Intercept)             0.736489       0.02405752


#  Group size - Count is factor
ds.count<-ds(data = final_dat, 
              formula= ~1+count_factor,
              truncation="15%",
              transect="point",
              key="hr",
              adjustment=NULL)
summary(ds.count)
ddf.gof(ds.count$ddf, asp=1)

# CvM test statistic =  0.78199  P =  0.0080796 
# # # AIC: 54222.21
# # # Scale Coefficients:  
# # #                          estimate           se
# # # (Intercept)                  6.6727570     0.04581586
# # # count2-3                    0.5614018     0.06964726
# # # count4+                    0.8416419     0.17293295
# # # Shape parameters:  
# # #                           estimate           se
# # # (Intercept)              0.7661301    0.02390104

#  All cov
ds.all<-ds(data = final_dat, 
              formula= ~1+count_factor+visibility+whale_behavior,
              truncation="15%",
              transect="point",
              key="hr",
              adjustment=NULL)
summary(ds.all)
ddf.gof(ds.all$ddf, asp=1)

# CvM test statistic =  0.47986  P =  0.044828
# # # AIC: 54133.25
# # # Scale Coefficients:  
# # #                                    estimate               se
# # #(Intercept)                                    6.83327011     0.04637927
# # #count_factor2-3                            0.53301231      0.06400286
# # #count_factor4+                                0.79389379     0.16764059
# # #visibilitygood                               0.05091515     0.05719176
# # #visibilitypoor                                 -0.62263143     0.09454118
# # #visibilitypoor-fog                         -1.27844963    0.25941451
# # #whale_behaviorDF-Dive-fluke-up  -0.07586427     0.05595046
# # #whale_behaviorLF-Lunge-feed        0.05273601     0.52122796
# # #whale_behaviorRE-Resting            -0.14716361     0.15277078
# # #whale_behaviorSA-Surface-active  0.54018195     0.10436279

# # # Shape parameters:  
# # #                           estimate           se
# # # (Intercept)              0.824327      0.02421066
##############################################################################

#  Other models run in exploratory phase

#  Ship speed
ds.speed<-ds(data =final_dat, 
               formula= ~1+ship_speed_scaled,
               truncation="15%",
               transect="point",
               key="hr",
               adjustment=NULL)
summary(ds.speed)
ddf.gof(ds.speed$ddf, asp=1)

# CvM test statistic =   0.45874  P =  0.050782 
# # # AIC: 54277.86 
# # # Scale Coefficients:  
# # #                          estimate           se
# # # (Intercept)                6.72404534   0.04519963
# # # ship_speed_scaled     0.09714449   0.02750598
# # # Shape parameters:  
 # # #                           estimate           se
# # # (Intercept)             0.7371793   0.02403831


#  No covariates - hazard rate with cos adjustments
ds.adj<-ds(data = final_dat,
              formula= ~1,
        truncation="15%",
              transect="point",
              key="hr",
              #adjustment=NULL
        )
summary(ds.adj)
# # # AIC: 54290.473 for cos(2) - higher AIC than with no adjustments

#  Half normal models
# ds.hn<-ds(data = final_dat,
           # formula= ~1,
           # truncation="10%",
           # transect="point",
           # #key="hn",
           # adjustment=NULL
           # )
# summary(ds.hn)
# # # All variations of half normal model fail to fit.

# ds.speed.hn<-ds(data =final_dat, 
               # formula= ~1+ship_speed_scaled,
               # truncation="15%",
               # transect="point",
               # key="hn",
                # adjustment=NULL)
# summary(ds.speed.hn)
# # # # Half normal model fail to fit.

# ds.vis.hn<-ds(data = final_dat, 
            # formula= ~1+visibility,
            # truncation="15%",
            # transect="point",
            # key="hn",
            # adjustment=NULL)
# summary(ds.vis.hn)
# # # Half normal model fail to fit.

# ds.beh.hn<-ds(data = final_dat, 
            # formula= ~1+whale_behavior,
            # truncation="15%",
            # transect="point",
            # key="hn",
            # adjustment=NULL)
# summary(ds.beh.hn)
# # # Half normal model fail to fit.

# ds.waves.hn<-ds(data = final_dat, 
               # formula= ~1+waves,
               # truncation="15%",
               # transect="point",
               # key="hn",
               # adjustment=NULL)
# summary(ds.waves.hn)
# # # Half normal model fail to fit.

# ds.count.hn<-ds(data = final_dat, 
              # formula= ~1+count_factor,
              # truncation="15%",
              # transect="point",
              # key="hn",
              # adjustment=NULL)
# summary(ds.count.hn)
# # # Half normal model fail to fit.
#plot(ds.count, main="Detection as Function of Group Size")

# # Wind
# ds.wind<-ds(data = final_dat, 
              # formula= ~1+wind,
              # truncation="15%",
              # transect="point",
              # key="hr",
              # adjustment=NULL)
# summary(ds.wind)

# ds.wind.hn<-ds(data = final_dat, 
              # formula= ~1+wind,
              # truncation="15%",
              # transect="point",
              # key="hn",
              # adjustment=NULL)
# summary(ds.wind.hn)
# # # # Half normal model fail to fit.

# # # AIC: 54290.3
# # # Scale Coefficients:  
# # #                          estimate           se
# # # (Intercept)                6.67893702   0.05989229
# # # windmoderate         0.04261289   0.06122996
# # # windnone               0.29290806   0.15013547
# # # windstrong            0.11967308   0.10194874
# # # Shape parameters:  
# # #                           estimate           se
# # # (Intercept)             0.7361125     0.02412308


ds.vis.count <-ds(data = final_dat, 
            formula= ~1+visibility+count_factor,
            truncation="15%",
            transect="point",
            key="hr",
            adjustment=NULL)
summary(ds.vis.count)


# # Location
# ds.loc<-ds(data = final_dat, 
            # formula= ~1+X_whale_UTM+Y_whale_UTM,
            # truncation="15%",
            # transect="point",
            # key="hr",
            # adjustment=NULL)
# summary(ds.loc)
# #plot(ds.loc, main="Detection as Function of Spatial Location")
# # # # AIC: 
# # # # Scale Coefficients:  
# # # #                          estimate           se
# # # # (Intercept)                
# # # # 
# # # # 
# # # # 
# # # # Shape parameters:  
# # # #                           estimate           se
# # # # (Intercept)           


# #All covariates with binned data and scaled ship speed
# ds.all<-ds(data = final_dat, 
           # formula= ~1+count+whale_behavior+visibility,
           # transect="point",
           # truncation="15%",
           # key="hr",
           # adjustment=NULL)
# summary(ds.all)
#plot(ds.beh, main="Detection as Function of Behavior")

# # # AIC: 
# # # Scale Coefficients:  
# # #                          estimate           se
# # # (Intercept)                
# # # 
# # # 
# # # 
# # # Shape parameters:  
# # #                           estimate           se
# # # (Intercept)           