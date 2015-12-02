#Read in raw data file
library(Distance)
library(dplyr)
setwd("C:/Users/sara.williams/Documents/GitHub/Whale-Detection-Probability-Analysis/")

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
#  Use either final_dat or bin_dat from DataCleaning.R script.
final_dat <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale-Detection-Probability-Analysis/data/final_dat_2015.csv")

#  No covariates - hazard rate
ds<-ds(data = final_dat,
              formula= ~1,
			  truncation="15%",
              transect="point",
              key="hr",
              adjustment=NULL)
summary(ds)
#plot(ds, main="Detection Function")

# # # AIC: 54288.47
# # # Scale Coefficients:  
# # #													estimate        	 se
# # # (Intercept)      					6.726756 			0.04511843
# # # Shape parameters:  
 # # # 													estimate        	 se
# # # (Intercept) 						0.7371793 	0.02403831

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

#  No covariates - half normal
ds.hn<-ds(data = final_dat,
					 formula= ~1,
					 truncation="10%",
					 transect="point",
					 #key="hn",
					 adjustment=NULL
					 )
summary(ds.hn)
# # # All variations of half normal model fail to fit.

#  Ship speed
ds.speed<-ds(data =final_dat, 
							 formula= ~1+ship_speed_scaled,
							 truncation="15%",
							 transect="point",
							 key="hr",
							  adjustment=NULL)
summary(ds.speed)
#plot(ds.speed, main="Detection as Function of Ship Speed")

# # # AIC: 54277.86 
# # # Scale Coefficients:  
# # #													estimate        	 se
# # # (Intercept)      					6.72404534 	0.04519963
# # # ship_speed_scaled 		0.09714449 	0.02750598
# # # Shape parameters:  
 # # # 													estimate        	 se
# # # (Intercept) 						0.7371793 	0.02403831

#  Visibility
ds.vis<-ds(data = final_dat, 
					  formula= ~1+visibility,
					  truncation="15%",
					  transect="point",
					  key="hr",
					  adjustment=NULL)
summary(ds.vis)
#plot(ds.vis, main="Detection as Function of Visibility")

# # # AIC: 54221.6 
# # # Scale Coefficients:  
# # #													estimate        	 se
# # # (Intercept)      					6.85951497 	0.04457347
# # # visibilitygood					0.04977762 	0.06111292
# # # visibilitypoor    				-0.61123947 	0.10312669
# # # visibilitypoor-fog 			-1.20921171 	0.29163640
# # # Shape parameters:  
# # # 													estimate        	 se
# # # (Intercept) 						0.7798358 		0.02438796

#  Behavior
ds.beh<-ds(data = final_dat, 
						formula= ~1+whale_behavior,
						truncation="15%",
						transect="point",
						key="hr",
						adjustment=NULL)
summary(ds.beh)
#plot(ds.beh, main="Detection as Function of Behavior")

# # # AIC: 54277.5
# # # Scale Coefficients:  
# # #																				estimate        	 se
# # # (Intercept)      												6.7315668 		0.04828056
# # # whale_behaviorDF-Dive-fluke-up 	-0.0352727 		0.06373799
# # # whale_behaviorLF-Lunge-feed      		0.3444508		0.69140831
# # # whale_behaviorRE-Resting        			-0.1046447 		0.18424319
# # # whale_behaviorSA-Surface-active  	0.5233363 		0.11744317
# # # Shape parameters:  
# # # 													estimate        	 se
# # # (Intercept) 						0.7472793 		0.0241749

#  Waves
ds.waves<-ds(data = final_dat, 
							 formula= ~1+waves,
							 truncation="15%",
							 transect="point",
							 key="hr",
							 adjustment=NULL)
summary(ds.waves)

# # # AIC: 54290.68
# # # Scale Coefficients:  
# # #													estimate        	 se
# # # (Intercept)      					6.71945692 	0.06166379
# # # waves2'      						0.04229350 	0.09909780
# # # waves3'      						0.30627212 	0.20179164
# # # waves4'     						0.58712044 	0.57729616
# # # wavescalm   					-0.01670241 	0.06206140
# # # Shape parameters:  
# # # 													estimate        	 se
# # # (Intercept) 						0.736489 			0.02405752

# Group size - Count is factor; greater than 4 -> 4
ds.count<-ds(data = final_dat, 
							formula= ~1+count,
							truncation="15%",
							transect="point",
							key="hr",
							adjustment=NULL)
summary(ds.count)
#plot(ds.count, main="Detection as Function of Group Size")

# # # AIC: 54222.21
# # # Scale Coefficients:  
# # #													estimate        	 se
# # # (Intercept)      						6.6727570 		0.04581586
# # # count2-3      							0.5614018 		0.06964726
# # # count4+      							0.8416419 		0.17293295
# # # Shape parameters:  
# # # 													estimate        	 se
# # # (Intercept) 						 0.7661301		0.02390104

# Wind
ds.wind<-ds(data = final_dat, 
							formula= ~1+wind,
							truncation="15%",
							transect="point",
							key="hr",
							adjustment=NULL)
summary(ds.wind)
#plot(ds.wind, main="Detection as Function of Wind")
# # # AIC: 54290.3
# # # Scale Coefficients:  
# # #													estimate        	 se
# # # (Intercept)      					6.67893702 	0.05989229
# # # windmoderate 				0.04261289 	0.06122996
# # # windnone     					0.29290806 	0.15013547
# # # windstrong  					0.11967308 	0.10194874
# # # Shape parameters:  
# # # 													estimate        	 se
# # # (Intercept) 						0.7361125 		0.02412308

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
# # # #													estimate        	 se
# # # # (Intercept)      					
# # # # 
# # # # 
# # # # 
# # # # Shape parameters:  
# # # # 													estimate        	 se
# # # # (Intercept) 					


#All covariates with binned data and scaled ship speed
ds.all<-ds(data = final_dat, 
					 formula= ~1+count+whale_behavior+visibility,
					 transect="point",
					 truncation="15%",
					 key="hr",
					 adjustment=NULL)
summary(ds.all)
#plot(ds.beh, main="Detection as Function of Behavior")

# # # AIC: 
# # # Scale Coefficients:  
# # #													estimate        	 se
# # # (Intercept)      					
# # # 
# # # 
# # # 
# # # Shape parameters:  
# # # 													estimate        	 se
# # # (Intercept) 					
########################################################################################################

########################################################
#Detection model function plots
x <- seq(min(final_dat_truncated85$distance), 
				max(final_dat_truncated85$distance))

#  No Covariates
se_no <- 0.04511843
ci_no <- 1.96*se_no

hr.no_cov <- function(x, scale = exp(6.727), shape = exp(0.737)){
							 1 - exp(-(x/scale)^(-shape))
			}
			hr.no_cov_l <- function(x, scale = exp(6.727-ci_no), shape = exp(0.737)){
										1 - exp(-(x/scale)^(-shape))
						}
			hr.no_cov_u <- function(x, scale = exp(6.727+ci_no), shape = exp(0.737)){
											1 - exp(-(x/scale)^(-shape))
						}
			
#  Speed
#   Speed scaled = 0 (or 14.6 knots), which is the mean
#   Scaled and centered speed: 
#   10.002 kts = -1.388
#   20.009 kts =	1.618
hr <- function(x, scale = exp(6.724), shape = exp(0.737)){
							 1 - exp(-(x/scale)^(-shape))
		   }
hr.cov.10 <- function(x, scale = exp(6.590), shape = exp(0.737)){
										   1 - exp(-(x/scale)^(-shape)) #increased by 10 kts
						}
hr.cov.20 <- function(x, scale = exp(6.881), shape = exp(0.737)){
										   1 - exp(-(x/scale)^(-shape)) #increased by 20 kts
						}

#  Group size
se_count_1 <- 0.04581586
ci_count_1 <- 1.96*se_count_1

se_count_2 <- 0.06964726
ci_count_2 <- 1.96*se_count_2

se_count_lg <-0.17293295
ci_count_lg <- 1.96*se_count_lg

hr.count <- function(x, scale = exp(6.673), shape = exp(0.766)){
							 1 - exp(-(x/scale)^(-shape))
		   }
		   hr.count_l <- function(x, scale = exp(6.673-ci_count_1), shape = exp(0.766)){
							 1 - exp(-(x/scale)^(-shape))
		   }
		   hr.count_u <- function(x, scale = exp(6.673+ci_count_1), shape = exp(0.766)){
							 1 - exp(-(x/scale)^(-shape))
		   }
hr.count.2 <- function(x, scale = exp(7.234), shape = exp(0.766)){
										 1 - exp(-(x/scale)^(-shape))
						}
						hr.count.2_l <- function(x, scale = exp(7.234-ci_count_2), shape = exp(0.766)){
										 1 - exp(-(x/scale)^(-shape))
						}
						hr.count.2_u <- function(x, scale = exp(7.234+ci_count_2), shape = exp(0.766)){
										 1 - exp(-(x/scale)^(-shape))
						}
hr.count.lg <- function(x, scale = exp(7.514), shape = exp(0.766)){
										 1 - exp(-(x/scale)^(-shape))
					   }
					   hr.count.lg_l <- function(x, scale = exp(7.514-ci_count_lg), shape = exp(0.766)){
										 1 - exp(-(x/scale)^(-shape))
					   }
					   hr.count.lg_u <- function(x, scale = exp(7.514+ci_count_lg), shape = exp(0.766)){
										 1 - exp(-(x/scale)^(-shape))
					   }

#  Visibility
se_vis_no <- 0.04457347
ci_vis_no <- 1.96*se_vis_no

se_vis_p <- 0.10312669
ci_vis_p <- 1.96*se_vis_p

se_vis_pf <- 0.29163640
ci_vis_pf <- 1.96*se_vis_pf

hr.vis <- function(x, scale = exp(6.860), shape = exp(0.780)){
							 1 - exp(-(x/scale)^(-shape))
			} # excellent - baseline
			hr.vis_l <- function(x, scale = exp(6.860-ci_vis_no), shape = exp(0.780)){
							 1 - exp(-(x/scale)^(-shape))
			}
			hr.vis_u <- function(x, scale = exp(6.860+ci_vis_no), shape = exp(0.780)){
							 1 - exp(-(x/scale)^(-shape))
			}
hr.vis.p <- function(x, scale = exp(6.249), shape = exp(0.780)){
												1 - exp(-(x/scale)^(-shape))
							  } # poor
							  hr.vis.p_l <- function(x, scale = exp(6.249- ci_vis_p), shape = exp(0.780)){
												1 - exp(-(x/scale)^(-shape))
							  } 
							  hr.vis.p_u <- function(x, scale = exp(6.249+ci_vis_p), shape = exp(0.780)){
												1 - exp(-(x/scale)^(-shape))
							  } 
hr.vis.pf <- function(x, scale = exp(5.651), shape = exp(0.780)){
													   1 - exp(-(x/scale)^(-shape))
									} # poor-fog
									hr.vis.pf_l <- function(x, scale = exp(5.651-ci_vis_pf), shape = exp(0.780)){
													   1 - exp(-(x/scale)^(-shape))
									}
									hr.vis.pf_u <- function(x, scale = exp(5.651+ci_vis_pf), shape = exp(0.780)){
													   1 - exp(-(x/scale)^(-shape))
									}

# Behavior
se_beh_no <- 0.04828056
ci_beh_no <- 1.96*se_beh_no

se_beh_sa <- 0.11744317
ci_beh_sa <- 1.96*se_beh_sa

hr.beh <- function(x, scale = exp(6.73157), shape = exp(0.747)){
							 1 - exp(-(x/scale)^(-shape))
		   }
		   hr.beh_l <- function(x, scale = exp(6.73157-ci_beh_no), shape = exp(0.747)){
							 1 - exp(-(x/scale)^(-shape))
		   }
		   hr.beh_u <- function(x, scale = exp(6.73157+ci_beh_no), shape = exp(0.747)){
							 1 - exp(-(x/scale)^(-shape))
		   }
hr.beh.sa <- function(x, scale = exp(7.225), shape = exp(0.747)){
										   1 - exp(-(x/scale)^(-shape))
						} # surface-active
						hr.beh.sa_l <- function(x, scale = exp(7.225-ci_beh_sa), shape = exp(0.747)){
										   1 - exp(-(x/scale)^(-shape))
						}
						hr.beh.sa_u <- function(x, scale = exp(7.225+ci_beh_sa), shape = exp(0.747)){
										   1 - exp(-(x/scale)^(-shape))
						}

# Waves
hr <- function(x, scale = exp(6.831), shape = exp(0.737)){
							 1 - exp(-(x/scale)^(-shape))
		   }
hr.cov <- function(x, scale = exp(), shape = exp(0.737)){
									 1 - exp(-(x/scale)^(-shape))
                   }

# Wind
hr <- function(x, scale = exp(6.831), shape = exp(0.736)){
							 1 - exp(-(x/scale)^(-shape))
		   }
hr.cov <- function(x, scale = exp(), shape = exp(0.736)){
									 1 - exp(-(x/scale)^(-shape))
                   }
				   
				   
				   
				   
				   
				   
				   
				   
#All cov
hr <- function(x, scale = exp(6.870), shape = exp(0.737)){
  1 - exp(-(x/scale)^(-shape))
}# Most common scenario: 1 whale, blow, excellent
hr.cov <- function(x, scale = exp(8.026), shape = exp(0.737)){
  1 - exp(-(x/scale)^(-shape)) 
}# Best case scenario: 4+ whales, surface active, excellent
hr.cov.2 <- function(x, scale = exp(5.701), shape = exp(0.737)){
  1 - exp(-(x/scale)^(-shape)) 
}# Worst case scenario: 1 whale, blow, poor-fog

##############################################################################
#  Plots
#   No covariates
no_cov_dat <- as.data.frame(cbind(x, hr.no_cov(x), hr.no_cov_l(x), hr.no_cov_u(x)))
no_cov_plot <- ggplot(no_cov_dat, aes(x=x, y=V2)) +
								   geom_line(size = 1.25, colour = "#46ACC8") +
								   geom_ribbon(data=no_cov_dat, aes(x=x, ymin=V3,ymax=V4),
								   alpha=0.4,  fill="#46ACC8") +
					               xlim(0, 4500) +
								   ylim(0, 1) +
								   xlab("
								   Distance from ship (m)") +
								   ylab("Detection probability
								   ") +
								   theme_bw() +
								   theme(panel.grid.major.x=element_blank(),
								   panel.grid.minor=element_blank()) +
								   theme(axis.title=element_text(size=15), 
								  axis.text=element_text(size=12)) + 
								  ggtitle("Detection as a function of distance") +
								  theme(plot.title = element_text(lineheight=1, face="bold", size =20, vjust =2))
no_cov_plot

#   Group size
count_dat <- as.data.frame(cbind(x, hr.count(x), hr.count_l(x), hr.count_u(x)))
count_dat_2 <-as.data.frame(cbind(x, hr.count.2(x), hr.count.2_l(x), hr.count.2_u(x)))
count_dat_lg <-as.data.frame(cbind(x, hr.count.lg(x), hr.count.lg_l(x), hr.count.lg_u(x)))

count_plot <- ggplot(count_dat, aes(x=x, y=V2)) +
								   geom_line(size = 1.25, colour = "#46ACC8") +
								   geom_ribbon(data=count_dat, aes(x=x, ymin=V3,ymax=V4),
								   alpha=0.4,  fill="#46ACC8") +
								   geom_line(data=count_dat_2, aes(x=x, y=V2), size = 1.25, colour = "#E58601") +
								   geom_ribbon(data=count_dat_2, aes(x=x, ymin=V3,ymax=V4),
								   alpha=0.4,  fill="#E58601") +
								   geom_line(data=count_dat_lg, aes(x=x, y=V2), size = 1.25, colour = "#B40F20") +
								   geom_ribbon(data=count_dat_lg, aes(x=x, ymin=V3,ymax=V4),
								   alpha=0.3,  fill="#B40F20") +
					               xlim(0, 4500) +
								   ylim(0, 1) +
								   xlab("
								   Distance from ship (m)") +
								   ylab("Detection probability
								   ") +
								   theme_bw() +
								   theme(panel.grid.major.x=element_blank(),
								   panel.grid.minor=element_blank()) +
								   theme(axis.title=element_text(size=15), 
								  axis.text=element_text(size=12)) + 
								  ggtitle("Detection as a function of distance and group size") +
								  theme(plot.title = element_text(lineheight=1, face="bold", size =20, vjust =2))
count_plot

#   Visibility
vis_dat <- as.data.frame(cbind(x, hr.vis(x), hr.vis_l(x), hr.vis_u(x)))
vis_dat_p <-as.data.frame(cbind(x, hr.vis.p(x), hr.vis.p_l(x), hr.vis.p_u(x)))
vis_dat_pf <-as.data.frame(cbind(x, hr.vis.pf(x), hr.vis.pf_l(x), hr.vis.pf_u(x)))

vis_plot <- ggplot(vis_dat, aes(x=x, y=V2)) +
								   geom_line(size = 1.25, colour = "#46ACC8") +
								   geom_ribbon(data=vis_dat, aes(x=x, ymin=V3,ymax=V4),
								   alpha=0.4,  fill="#46ACC8") +
								   geom_line(data=vis_dat_p, aes(x=x, y=V2), size = 1.25, colour = "#E58601") +
								   geom_ribbon(data=vis_dat_p, aes(x=x, ymin=V3,ymax=V4),
								   alpha=0.4,  fill="#E58601") +
								   geom_line(data=vis_dat_pf, aes(x=x, y=V2), size = 1.25, colour = "#B40F20") +
								   geom_ribbon(data=vis_dat_pf, aes(x=x, ymin=V3,ymax=V4),
								   alpha=0.3,  fill="#B40F20") +
					               xlim(0, 4500) +
								   ylim(0, 1) +
								   xlab("
								   Distance from ship (m)") +
								   ylab("Detection probability
								   ") +
								   theme_bw() +
								   theme(panel.grid.major.x=element_blank(),
								   panel.grid.minor=element_blank()) +
								   theme(axis.title=element_text(size=15), 
								  axis.text=element_text(size=12)) + 
								  ggtitle("Detection as a function of distance and visibility") +
								  theme(plot.title = element_text(lineheight=1, face="bold", size =20, vjust =2))
vis_plot

#   Behavior
beh_dat <- as.data.frame(cbind(x, hr.beh(x), hr.beh_l(x), hr.beh_u(x)))
beh_dat_sa <-as.data.frame(cbind(x, hr.beh.sa(x), hr.beh.sa_l(x), hr.beh.sa_u(x)))

beh_plot <- ggplot(beh_dat, aes(x=x, y=V2)) +
								   geom_line(size = 1.25, colour = "#46ACC8") +
								   geom_ribbon(data=beh_dat, aes(x=x, ymin=V3,ymax=V4),
								   alpha=0.4,  fill="#46ACC8") +
								   geom_line(data=beh_dat_sa, aes(x=x, y=V2), size = 1.25, colour = "#E58601") +
								   geom_ribbon(data=beh_dat_sa, aes(x=x, ymin=V3,ymax=V4),
								   alpha=0.4,  fill="#E58601") +
								   xlim(0, 4500) +
								   ylim(0, 1) +
								   xlab("
								   Distance from ship (m)") +
								   ylab("Detection probability
								   ") +
								   theme_bw() +
								   theme(panel.grid.major.x=element_blank(),
								   panel.grid.minor=element_blank()) +
								   theme(axis.title=element_text(size=15), 
								  axis.text=element_text(size=12)) + 
								  ggtitle("Detection as a function of distance and whale behavior") +
								  theme(plot.title = element_text(lineheight=1, face="bold", size =20, vjust =2))
beh_plot


##############################################################################



















#Plot detection function
plot(sort(x), hr(x), xlim=c(-10,5000),
	 axes=F,
     main="Detection as  a function of distance and ship speed", cex.main = 1, 
     cex.lab = 1, cex.axis=1, xlab="Distance (m)", ylab="P(detection)", 
     ylim = c(0, 1.0), col = "black", lwd = 2, type = "l", 
     frame.plot = F)
axis(1, pos=0)
axis(2, pos=0)

#Adding 1000m lines for null
a <- hr(1000)
segments(1000, a, 1000, 0, col= "black" ,lty=1, lwd = 1.5)
segments(0, a, 1000, a, col= "black" ,lty=1, lwd = 1.5)	 
	
#Add in line for covariate
lines(x, hr.cov.10(x), col = "black", lwd = 2, lty = 2)
lines(x, hr.cov.20(x), col = "black", lwd = 2, lty = 3)

#Adding 1000m lines for covariate
b <- hr.cov.10(1000)
segments(1000, b, 1000, 0, col= "black" ,lty=1, lwd = 1.5)
segments(0, b, 1000, b, col= "black" ,lty=1, lwd = 1.5)	 

c <- hr.cov.20(1000)
segments(1000, c, 1000, 0, col= "black" ,lty=1, lwd = 1.5)
segments(0, c, 1000, c, col= "black" ,lty=1, lwd = 1.5)	 
	 
	 
# Plot 60% detection comparison of scenarios
plot(sort(x), hr(x), xlim=c(-10,5000),
	 axes=F,
     main=NULL, cex.main = 1, 
     cex.lab = 1, cex.axis=1, xlab="Distance (m)", ylab="P(detection)", 
     ylim = c(0, 1.0), col = "black", lwd = 2, type = "l", 
     frame.plot = F)
axis(1, pos=0)
axis(2, pos=0)

#Adding 1000m lines for null
a <- hr(1000)
segments(1000, a, 1000, 0, col= "black" ,lty=1, lwd = 1.5)
#segments(0, a, 1000, a, col= "black" ,lty=1, lwd = 1.5)	 
	
#Add in line for covariate
lines(x, hr.cov(x), col = "black", lwd = 2, lty = 2)
lines(x, hr.cov.2(x), col = "black", lwd = 2, lty = 3)

#Adding 1000m lines for covariate
b <- hr.cov(3170)
segments(3170, b, 3170, 0, col= "black" ,lty=1, lwd = 1.5)
segments(0, b, 3170, b, col= "black" ,lty=1, lwd = 1.5)	 

c <- hr.cov.2(310)
segments(310, c, 310, 0, col= "black" ,lty=1, lwd = 1.5)
#segments(0, c, 310, c, col= "black" ,lty=1, lwd = 1.5)	 