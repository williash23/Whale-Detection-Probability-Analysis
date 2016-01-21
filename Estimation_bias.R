#  Sara Williams
#  12/23/2015
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
				mutate(percent_diff = (diff/Exact)*100) %>%
				mutate(percent_abs_diff = (abs_diff/Exact)*100)
				
					#  Investigate strange data points
					check <- filter(tmp2, abs_diff > 2000)
					#   Based on the data in this filter there are 2 obvious data entry errors. Removed from data.
					#   Event_Date    Ship 					Observer 	Object 		Estimate 		Exact 	abs_diff
					#   8/7/2013 		Zuiderdam    		Sara   			Boat     	5000   			372     	4628
					#   6/27/2014 		Oosterdam  		Heather   	Boat    	23000  			2802   	20198

dat <- tmp3 %>%
			 filter(abs_diff < 4627) %>%
			 filter(Exact > 0)

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
					   theme_bw() +
					  geom_text(x = 25, y = 300, label = lm_eqn(dat), parse = TRUE)
lm_plot					

lm_plot1 <- ggplot(dat, aes(x=Exact, y=diff)) +
					   geom_point(shape = 3) +    
					   geom_smooth(method=lm) +
					   xlab("Exact (m)") +
					   ylab("Exact - Estimated (m)") +
					   theme_bw() +
					  geom_text(x = 25, y = 300, label = lm_eqn(dat), parse = TRUE)
lm_plot1