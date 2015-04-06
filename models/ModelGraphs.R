###READ IN DATA
######################################################
loadfonts(device="win")
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
  ggtitle("Title text goes here") +
  theme(plot.title = element_text(size = 16, family="Georgia", face="italic"))
##load(file="first_obs_NoBlank.RData")
#########################################################

bins <- seq(0,5500,by=500)

hist(bin_data$distance, breaks=bins, main=NULL,
     xlab="Distance (m)", ylab="Frequency", ylim = c(0, 800), 
     xlim = c(0, 5500),
     cex.lab = 1, cex.main = 1, cex.axis=1,
     border = "black", col = "gray", freq = T)
lines(density(final_data$distance))
axis(2, at=c(0, 750), cex.axis = 1)

sea.counts <- as.data.frame(table(SeaState))
barplot(sea.counts, main="Number of Observations by Sea State", 
        xlab="Sea State", ylab="Frequency", cex.lab = 0.75, 
        cex.main = 1, cex.axis=0.75,
        border = "gray", col = "gray", cex.names = 0.75,
        ylim=c(0, 1600))

sea.counts.plot <- 
  ggplot(data=sea.counts, aes(x=SeaState, y=Freq), family="B") + geom_bar(stat="identity")
sea.counts.plot <- sea.counts.plot +
  labs(x = "Sea State", y = "Frequency")
sea.counts.plot <- sea.counts.plot + theme_minimal(base_size = 12)
sea.counts.plot


########################################################
#Detection model functions - binned data
x <- seq(min(bin_data$distance), max(bin_data$distance))

#all cov
hr <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}

hr.cov <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape)) 
}

#No Cov
hr <- function(x, scale = exp(6.851883), shape = exp(0.9563675){
  1 - exp(-(x/scale)^(-shape))
}

#speed
hr <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}
hr.cov.10 <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape)) #increased by 10 kts
}
hr.cov.20 <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape)) #increased by 20 kts
}

#visibility
hr <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}
hr.cov.poor <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
} #poor
hr.cov.poorfog <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
} #poor-fog

#behavior
hr <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}
hr.cov <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}

#sea state
hr <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}
hr.cov <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}

#group size
hr <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}
hr.cov <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}

########################################################
#Detection model functions - NOT binned data
x <- seq(min(final_data$distance), max(final_data$distance))


#all cov
hr <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}

hr.cov <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape)) 
}

#No Cov
hr <- function(x, scale = exp(6.749879), shape = exp(0.7907694)){
  1 - exp(-(x/scale)^(-shape))
}

#speed
hr <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}
hr.cov.10 <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape)) #increased by 10 kts
}
hr.cov.20 <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape)) #increased by 20 kts
}

#visibility
hr <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}
hr.cov.poor <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
} #poor
hr.cov.poorfog <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
} #poor-fog

#behavior
hr <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}
hr.cov <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}

#sea state
hr <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}
hr.cov <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}

#group size
hr <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}
hr.cov <- function(x, scale = exp(), shape = exp()){
  1 - exp(-(x/scale)^(-shape))
}

###################################################################
#Plots

#Plot detection function
plot(sort(x), hr(x), xlim=c(0,5000), 
     main=NULL, cex.main = 1, 
     cex.lab = 1, cex.axis=1, xlab="Distance (m)", ylab="P(detection)", 
     ylim = c(0, 1.0), col = "black", lwd = 2, type = "l", 
     frame.plot = F)
#Add in line for covariate
	 lines(x, hr.cov(x), col = "black", lwd = 2, lty = 2)

#50% lines
abline(v=1000, h=0.17,col= "black" ,lty=1, lwd = 1.75)
abline(v=1000, h=0.08,col= "black" ,lty=1, lwd = 1.75)

text(1700, 0.56, "50% Detection", cex = .75)

segments(514, 0.5, 514, 0, col= "black" ,lty=1, lwd = 1.75)
segments(613, 0.5, 613, 0, col= "black" ,lty=1, lwd = 1.75)




legend("topright", legend = c("Group Size = 2", "Group Size = 1"),
       col = c("black", "black"), lty = c(1,2), cex = .8, 
       lwd = 2, xjust = 0.4)
##############

hist(dd$distance, breaks=bins, border = "gray", col = "gray")
axis(2, at=c(0, 0.001), cex.axis = 0.75)

points(620, 0.5, pch = 19, cex = 1, col = 1)
points(445, 0.5, pch = 19, cex = 1, col = 1)