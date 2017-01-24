#  Sara Williams
#  12/23/2015; updated 11/6/2016
#  Detection probability analysis - assesing bias in estimation of distances by observers 
#   vs. actual distances from range finder binoculars.
################################################################################

#  Load packages
library(dplyr)
library(ggplot2)
################################################################################

#  Read in raw data file 
tmp1 <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale-Detection-Probability-Analysis/data/Distance_estimation.csv")

#  Add column of differences between estimate and actual
tmp2 <- tmp1 %>%
         mutate(abs_diff = abs(Exact - Estimate)) %>%
         mutate(diff = (Exact - Estimate))
tmp3<- tmp2 %>%
        mutate(percent_diff = diff/Exact) %>%
        mutate(percent_abs_diff = abs_diff/Exact)
        
          #  Investigate strange data points
          check <- filter(tmp2, abs_diff > 2000)
          #   Based on the data in this filter there are 2 obvious data entry errors. Removed from data.
          #   Event_Date    Ship           Observer   Object     Estimate     Exact   abs_diff
          #   8/7/2013     Zuiderdam        Sara         Boat       5000         372       4628
          #   6/27/2014     Oosterdam      Heather     Boat      23000        2802     20198

dat_err <- tmp3 %>%
                  filter(abs_diff < 4627) %>%
                  filter(Exact > 0) %>%
                  filter(percent_diff > -1)

#  Plot histogram and density plots of difference frequency
dens_diff <- ggplot(dat, aes(x=diff)) + 
              geom_density(alpha=.6, fill="black") +
              xlim(-2000, 2000) +
              ylim(0, 0.0065) +
              xlab("Exact - Estimated (m)") +
              ylab("Density") +
              theme_bw()
dens_diff

hist_diff <- ggplot(dat, aes(x=diff)) + 
            geom_histogram(fill="grey", 
                    colour = "grey" ,
                    binwidth=100) +
            xlim(-2000, 2000) +
            ylim(0, 2500) +
            xlab("Exact - Estimated (m)") +
            ylab("Frequency") +
            theme_bw()
hist_diff

#  Assess how many data points are within 100, 200 and 300 m difference from exact
under_100m <- dat %>%
         filter(diff < 100 & diff > -100)
under_200m <- dat %>%
         filter(diff < 200 & diff > -200)  
under_300m <- dat %>%
         filter(diff < 300 & diff > -300)         
         

################################################################################
#  Generate distribution of possible observed/estimated differences using error
#   Example of one multiple observation group:
library(aspace) #  Allows sin() cos() of degree units
library(rgdal)

#   Read in standard data
dat_raw <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale-Movement-Analysis/data/Whales_0615_locations_clean.csv")
dat_all <- dat_raw %>%
            group_by(same_whale_ID) %>%
            filter(n() >1) %>%
            ungroup() %>%
            filter(count == 1) %>%
            #mutate(new_bear = vessel_azimuth - ship_whale_bearing) %>%
            dplyr::select(unique_event_ID, same_whale_ID, X_ship_UTM, Y_ship_UTM, X_whale_UTM, Y_whale_UTM, ship_whale_dist, 
                       ship_whale_azimuth, Y_PROJ, X_PROJ,  ob_order_time) %>%
             as.data.frame()
dat_whale <- dat_all %>%
                       dplyr::select(X_whale_UTM, Y_whale_UTM)

#  Get whale locations in lat/long
whale_sp <- SpatialPoints(dat_whale, proj4string=CRS("+proj=utm +zone=8N +datum=WGS84"))  
whale_latlong <- spTransform(whale_sp, CRS("+proj=longlat +datum=WGS84"))
whale_latlong_df <- as.data.frame(whale_latlong)
dat <- cbind(dat_all, whale_latlong_df)
names(dat)[12] <- "X_whale_long"
names(dat)[13] <- "X_whale_lat"

dat_rad <- dat %>%
                  mutate(X_ship_rad = X_PROJ*pi/180) %>%
                  mutate(Y_ship_rad = Y_PROJ*pi/180) %>%
                  mutate(bearing_rad = ship_whale_azimuth*pi/180)

#   Generate data frame for each observed location with the 100 possible distances estimated with error
#   Earth Radious in KM at 58.7657 lat
r <- 6362.531

#  Example 1
#   Get error for each observed ditsance in multi ob group
dists <- c(dat[1400,7], dat[1401,7], dat[1402,7])
n <- length(dists)
err <- vector("numeric", 100)
pulls <- matrix(,100, n)
#  Using percent difference between Exact - Estimate mean and sd
for(i in 1:100){
  for(t in 1:n){
  err[i] <- rnorm(1, 0.0014, 0.175)
  pulls[i,t] <- dists[t] + (dists[t]*err[i]) 
  }
}

pulls1_red <- as.data.frame(pulls) %>%
                       dplyr::select(V1) %>%
                       filter(V1 < quantile(V1, probs = 0.76) & V1 > quantile(V1, probs = 0.24))

pulls2_red <- as.data.frame(pulls) %>%
                       dplyr::select(V2) %>%
                       filter(V2 < quantile(V2, probs = 0.76) & V2 > quantile(V2, probs = 0.24))

pulls3_red <- as.data.frame(pulls) %>%
                       dplyr::select(V3) %>%
                       filter(V3 < quantile(V3, probs = 0.76) & V3 > quantile(V3, probs = 0.24))

obs_dat1_1_tmp <-  dat_rad[1400,]
obs_dat1_1_tmp2 <- obs_dat1_1_tmp[rep(seq_len(nrow(obs_dat1_1_tmp)), each = nrow(pulls1_red)),]
obs_dat1_1_tmp3 <- as.data.frame(cbind(obs_dat1_1_tmp2, pulls1_red[,1]))
names(obs_dat1_1_tmp3)[17] <- "ship_whale_dist_w_err"
obs_dat1_1 <- obs_dat1_1_tmp3 %>%
                         mutate(new_lat_whale_rad = asin(sin(Y_ship_rad)*cos((ship_whale_dist_w_err/1000)/r) + cos(Y_ship_rad)*sin((ship_whale_dist_w_err/1000)/r)*cos(bearing_rad))) %>%
                         mutate(new_long_whale_rad = X_ship_rad + atan2(sin(bearing_rad)*sin(ship_whale_dist_w_err/1000/r)*cos(Y_ship_rad),cos(ship_whale_dist_w_err/1000/r)-sin(Y_ship_rad)*sin(new_lat_whale_rad))) %>%
                         mutate(new_whale_lat = new_lat_whale_rad*(180/pi)) %>%
                         mutate(new_whale_long = new_long_whale_rad*(180/pi))
dat_whale_new1 <- obs_dat1_1 %>%
                                 dplyr::select(new_whale_long, new_whale_lat)
dat_whale_old1 <- obs_dat1_1 %>%
                                 dplyr::select(X_whale_long, X_whale_lat)
new_whale_sp1 <- SpatialPoints(dat_whale_new1, proj4string=CRS("+proj=longlat +datum=WGS84"))  
new_whale_UTM1 <- spTransform(new_whale_sp1, CRS("+proj=utm +zone=8N +datum=WGS84"))
new_whale_UTM_df1 <- as.data.frame(new_whale_UTM1)
old_whale_sp1 <- SpatialPoints(dat_whale_old1, proj4string=CRS("+proj=longlat +datum=WGS84"))  
old_whale_UTM1 <- spTransform(old_whale_sp1, CRS("+proj=utm +zone=8N +datum=WGS84"))
old_whale_UTM_df1 <- as.data.frame(old_whale_UTM1)
write.csv(new_whale_UTM_df1, file = "C:/Users/sara.williams/Documents/GitHub/Whale-Movement-Analysis/data/new_whale_loc_ex1_1.csv", row.names=FALSE)
write.csv(old_whale_UTM_df1, file = "C:/Users/sara.williams/Documents/GitHub/Whale-Movement-Analysis/data/old_whale_loc_ex1_1.csv", row.names=FALSE)
#write.csv(obs_dat1_1, file = "C:/Users/sara.williams/Documents/GitHub/Whale-Movement-Analysis/data/estimated_dist_err_ex1.csv", row.names=FALSE)

obs_dat1_2_tmp <-  dat_rad[1401,]
obs_dat1_2_tmp2 <- obs_dat1_2_tmp[rep(seq_len(nrow(obs_dat1_2_tmp)), each = nrow(pulls2_red)),]
obs_dat1_2_tmp3 <- as.data.frame(cbind(obs_dat1_2_tmp2, pulls2_red[,1]))
names(obs_dat1_2_tmp3)[17] <- "ship_whale_dist_w_err"
obs_dat1_2 <- obs_dat1_2_tmp3 %>%
                        mutate(new_lat_whale_rad = asin(sin(Y_ship_rad)*cos((ship_whale_dist_w_err/1000)/r) + cos(Y_ship_rad)*sin((ship_whale_dist_w_err/1000)/r)*cos(bearing_rad))) %>%
                        mutate(new_long_whale_rad = X_ship_rad + atan2(sin(bearing_rad)*sin(ship_whale_dist_w_err/1000/r)*cos(Y_ship_rad),cos(ship_whale_dist_w_err/1000/r)-sin(Y_ship_rad)*sin(new_lat_whale_rad))) %>%
                        mutate(new_whale_lat = new_lat_whale_rad*(180/pi)) %>%
                        mutate(new_whale_long = new_long_whale_rad*(180/pi))
dat_whale_new2 <- obs_dat1_2 %>%
                                 dplyr::select(new_whale_long, new_whale_lat)
new_whale_sp2 <- SpatialPoints(dat_whale_new2, proj4string=CRS("+proj=longlat +datum=WGS84"))  
new_whale_UTM2 <- spTransform(new_whale_sp2, CRS("+proj=utm +zone=8N +datum=WGS84"))
new_whale_UTM_df2 <- as.data.frame(new_whale_UTM2)
write.csv(new_whale_UTM_df2, file = "C:/Users/sara.williams/Documents/GitHub/Whale-Movement-Analysis/data/new_whale_loc_ex1_2.csv", row.names=FALSE)
#write.csv(obs_dat1_2, file = "C:/Users/sara.williams/Documents/GitHub/Whale-Movement-Analysis/data/estimated_dist_err_ex2.csv", row.names=FALSE)

obs_dat1_3_tmp <-  dat_rad[1402,]
obs_dat1_3_tmp2 <- obs_dat1_3_tmp[rep(seq_len(nrow(obs_dat1_3_tmp)), each = nrow(pulls3_red)),]
obs_dat1_3_tmp3 <- as.data.frame(cbind(obs_dat1_3_tmp2, pulls3_red[,1]))
names(obs_dat1_3_tmp3)[17] <- "ship_whale_dist_w_err"
obs_dat1_3 <- obs_dat1_3_tmp3 %>%
                        mutate(new_lat_whale_rad = asin(sin(Y_ship_rad)*cos((ship_whale_dist_w_err/1000)/r) + cos(Y_ship_rad)*sin((ship_whale_dist_w_err/1000)/r)*cos(bearing_rad))) %>%
                        mutate(new_long_whale_rad = X_ship_rad + atan2(sin(bearing_rad)*sin(ship_whale_dist_w_err/1000/r)*cos(Y_ship_rad),cos(ship_whale_dist_w_err/1000/r)-sin(Y_ship_rad)*sin(new_lat_whale_rad))) %>%
                        mutate(new_whale_lat = new_lat_whale_rad*(180/pi)) %>%
                        mutate(new_whale_long = new_long_whale_rad*(180/pi))
dat_whale_new3 <- obs_dat1_3 %>%
                                 dplyr::select(new_whale_long, new_whale_lat)
new_whale_sp3 <- SpatialPoints(dat_whale_new3, proj4string=CRS("+proj=longlat +datum=WGS84"))  
new_whale_UTM3 <- spTransform(new_whale_sp3, CRS("+proj=utm +zone=8N +datum=WGS84"))
new_whale_UTM_df3 <- as.data.frame(new_whale_UTM3)
write.csv(new_whale_UTM_df3, file = "C:/Users/sara.williams/Documents/GitHub/Whale-Movement-Analysis/data/new_whale_loc_ex1_3.csv", row.names=FALSE)
#write.csv(obs_dat1_3, file = "C:/Users/sara.williams/Documents/GitHub/Whale-Movement-Analysis/data/estimated_dist_err_ex3.csv", row.names=FALSE)


#  Example 2
#   Get error for each observed ditsance in multi ob group
dists <- c(dat[709,7], dat[710,7], dat[711,7])
n <- length(dists)
err <- vector("numeric", 100)
pulls <- matrix(,100, n)
#  Using percent difference between Exact - Estimate mean and sd
for(i in 1:100){
  for(t in 1:n){
  err[i] <- rnorm(1, 0.0014, 0.175)
  pulls[i,t] <- dists[t] + (dists[t]*err[i]) 
  }
}

pulls1_red <- as.data.frame(pulls) %>%
                       dplyr::select(V1) %>%
                       filter(V1 < quantile(V1, probs = 0.76) & V1 > quantile(V1, probs = 0.24))

pulls2_red <- as.data.frame(pulls) %>%
                       dplyr::select(V2) %>%
                       filter(V2 < quantile(V2, probs = 0.76) & V2 > quantile(V2, probs = 0.24))

pulls3_red <- as.data.frame(pulls) %>%
                       dplyr::select(V3) %>%
                       filter(V3 < quantile(V3, probs = 0.76) & V3 > quantile(V3, probs = 0.24))

obs_dat2_1_tmp <-  dat_rad[709,]
obs_dat2_1_tmp2 <- obs_dat2_1_tmp[rep(seq_len(nrow(obs_dat2_1_tmp)), each = nrow(pulls1_red)),]
obs_dat2_1_tmp3 <- as.data.frame(cbind(obs_dat2_1_tmp2, pulls1_red[,1]))
names(obs_dat2_1_tmp3)[17] <- "ship_whale_dist_w_err"
obs_dat2_1 <- obs_dat2_1_tmp3 %>%
                         mutate(new_lat_whale_rad = asin(sin(Y_ship_rad)*cos((ship_whale_dist_w_err/1000)/r) + cos(Y_ship_rad)*sin((ship_whale_dist_w_err/1000)/r)*cos(bearing_rad))) %>%
                         mutate(new_long_whale_rad = X_ship_rad + atan2(sin(bearing_rad)*sin(ship_whale_dist_w_err/1000/r)*cos(Y_ship_rad),cos(ship_whale_dist_w_err/1000/r)-sin(Y_ship_rad)*sin(new_lat_whale_rad))) %>%
                         mutate(new_whale_lat = new_lat_whale_rad*(180/pi)) %>%
                         mutate(new_whale_long = new_long_whale_rad*(180/pi))
dat_whale_new1 <- obs_dat2_1 %>%
                                 dplyr::select(new_whale_long, new_whale_lat)
new_whale_sp1 <- SpatialPoints(dat_whale_new1, proj4string=CRS("+proj=longlat +datum=WGS84"))  
new_whale_UTM1 <- spTransform(new_whale_sp1, CRS("+proj=utm +zone=8N +datum=WGS84"))
new_whale_UTM_df1 <- as.data.frame(new_whale_UTM1)
write.csv(new_whale_UTM_df1, file = "C:/Users/sara.williams/Documents/GitHub/Whale-Movement-Analysis/data/new_whale_loc_ex2_1.csv", row.names=FALSE)
write.csv(obs_dat2_1, file = "C:/Users/sara.williams/Documents/GitHub/Whale-Movement-Analysis/data/estimated_dist_err_ex2_1.csv", row.names=FALSE)

obs_dat2_2_tmp <-  dat_rad[710,]
obs_dat2_2_tmp2 <- obs_dat2_2_tmp[rep(seq_len(nrow(obs_dat2_2_tmp)), each = nrow(pulls2_red)),]
obs_dat2_2_tmp3 <- as.data.frame(cbind(obs_dat2_2_tmp2, pulls2_red[,1]))
names(obs_dat2_2_tmp3)[17] <- "ship_whale_dist_w_err"
obs_dat2_2 <- obs_dat2_2_tmp3 %>%
                        mutate(new_lat_whale_rad = asin(sin(Y_ship_rad)*cos((ship_whale_dist_w_err/1000)/r) + cos(Y_ship_rad)*sin((ship_whale_dist_w_err/1000)/r)*cos(bearing_rad))) %>%
                        mutate(new_long_whale_rad = X_ship_rad + atan2(sin(bearing_rad)*sin(ship_whale_dist_w_err/1000/r)*cos(Y_ship_rad),cos(ship_whale_dist_w_err/1000/r)-sin(Y_ship_rad)*sin(new_lat_whale_rad))) %>%
                        mutate(new_whale_lat = new_lat_whale_rad*(180/pi)) %>%
                        mutate(new_whale_long = new_long_whale_rad*(180/pi))
dat_whale_new2 <- obs_dat2_2 %>%
                                 dplyr::select(new_whale_long, new_whale_lat)
new_whale_sp2 <- SpatialPoints(dat_whale_new2, proj4string=CRS("+proj=longlat +datum=WGS84"))  
new_whale_UTM2 <- spTransform(new_whale_sp2, CRS("+proj=utm +zone=8N +datum=WGS84"))
new_whale_UTM_df2 <- as.data.frame(new_whale_UTM2)
write.csv(new_whale_UTM_df2, file = "C:/Users/sara.williams/Documents/GitHub/Whale-Movement-Analysis/data/new_whale_loc_ex2_2.csv", row.names=FALSE)
write.csv(obs_dat2_2, file = "C:/Users/sara.williams/Documents/GitHub/Whale-Movement-Analysis/data/estimated_dist_err_ex2_2.csv", row.names=FALSE)


obs_dat2_3_tmp <-  dat_rad[711,]
obs_dat2_3_tmp2 <- obs_dat2_3_tmp[rep(seq_len(nrow(obs_dat2_3_tmp)), each = nrow(pulls1_red)),]
obs_dat2_3_tmp3 <- as.data.frame(cbind(obs_dat2_3_tmp2, pulls3_red[,1]))
names(obs_dat2_3_tmp3)[17] <- "ship_whale_dist_w_err"
obs_dat2_3 <- obs_dat2_3_tmp3 %>%
                        mutate(new_lat_whale_rad = asin(sin(Y_ship_rad)*cos((ship_whale_dist_w_err/1000)/r) + cos(Y_ship_rad)*sin((ship_whale_dist_w_err/1000)/r)*cos(bearing_rad))) %>%
                        mutate(new_long_whale_rad = X_ship_rad + atan2(sin(bearing_rad)*sin(ship_whale_dist_w_err/1000/r)*cos(Y_ship_rad),cos(ship_whale_dist_w_err/1000/r)-sin(Y_ship_rad)*sin(new_lat_whale_rad))) %>%
                        mutate(new_whale_lat = new_lat_whale_rad*(180/pi)) %>%
                        mutate(new_whale_long = new_long_whale_rad*(180/pi))
dat_whale_new3 <- obs_dat2_3 %>%
                                 dplyr::select(new_whale_long, new_whale_lat)
new_whale_sp3 <- SpatialPoints(dat_whale_new3, proj4string=CRS("+proj=longlat +datum=WGS84"))  
new_whale_UTM3 <- spTransform(new_whale_sp3, CRS("+proj=utm +zone=8N +datum=WGS84"))
new_whale_UTM_df3 <- as.data.frame(new_whale_UTM3)
write.csv(new_whale_UTM_df3, file = "C:/Users/sara.williams/Documents/GitHub/Whale-Movement-Analysis/data/new_whale_loc_ex2_3.csv", row.names=FALSE)
write.csv(obs_dat2_3, file = "C:/Users/sara.williams/Documents/GitHub/Whale-Movement-Analysis/data/estimated_dist_err_ex2_3.csv", row.names=FALSE)





p <- ggplot() +
        geom_point(data = dat_whale_new3, aes(new_whale_lat, new_whale_long), size = 14, colour = "#009999", alpha = 0.01) +
        geom_point(data = obs_dat2_3, aes(X_whale_lat, X_whale_long), size = 10, colour = "#EBCC2A", fill = "#EBCC2A")
p






















for(i in 1:100){
  for(t in 1:n){
  err[i] <- rnorm(1, 13.4, 229.5)
  pulls[i,t] <- dists[t] + err[i] 
  }
}
hist(pulls[,1])


#  Linear regression: difference as a function of exact distance
mod <- lm(dat$Estimate ~ dat$Exact)
summary(mod)
anova(mod)

mod1 <- lm(dat$diff ~ dat$Exact)
summary(mod1)
anova(mod1)



lm_plot <- ggplot(dat, aes(x=Exact, y=Estimate)) +
             geom_point(shape = 3) +    
             geom_smooth(method=lm) +
             xlab("Exact (m)") +
             ylab("Estimated (m)") +
             theme_bw() # +
            #geom_text(x = 25, y = 300, label = lm_eqn(dat), parse = TRUE)
lm_plot          

lm_plot1 <- ggplot(dat, aes(x=Exact, y=diff)) +
             geom_point(shape = 3) +    
             geom_smooth(method=lm) +
             xlab("Exact (m)") +
             ylab("Exact - Estimated (m)") +
             theme_bw() #+
            #geom_text(x = 25, y = 300, label = lm_eqn(dat), parse = TRUE)
lm_plot1

