###READ IN DATA
loadfonts(device="win")

ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
  ggtitle("Title text goes here") +
  theme(plot.title = element_text(size = 16, family="Georgia", face="italic"))


##load(file="first_obs_NoBlank.RData")
bins <- seq(0,3600,by=200)

hist(newdata$distance, breaks=bins, main=NULL,
     xlab="Distance (m)", ylab="Frequency", ylim = c(0, 400), 
     xlim = c(0, 4000),
     cex.lab = 1, cex.main = 1, cex.axis=1,
     border = "gray", col = "gray", freq = T,
     family = "Times New Roman")
lines(density(newdata$distance))
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
#Detection model funtions
x <- seq(min(newdata$distance), max(newdata$distance))

#all cov
hr <- function(x, scale = exp(6.22), shape = exp(0.8827)){
  1 - exp(-(x/scale)^(-shape))
}

hr.cov <- function(x, scale = exp(5.87203), shape = exp(0.8827)){
  1 - exp(-(x/scale)^(-shape)) 
}

#No Cov
hr <- function(x, scale = exp(6.29148), shape = exp(0.8300175)){
  1 - exp(-(x/scale)^(-shape))
}

#speed
hr <- function(x, scale = exp(5.9667227), shape = exp(0.8307049)){
  1 - exp(-(x/scale)^(-shape))
}

hr.cov <- function(x, scale = exp(6.187), shape = exp(0.8307049)){
  1 - exp(-(x/scale)^(-shape)) #increased by 10 kts
}

hr.cov <- function(x, scale = exp(6.41), shape = exp(0.8307049)){
  1 - exp(-(x/scale)^(-shape)) #increased by 20 kts
}


#visibility
hr <- function(x, scale = exp(6.34926080), shape = exp(0.8516195)){
  1 - exp(-(x/scale)^(-shape))
}

hr.cov.poor <- function(x, scale = exp(6.052), shape = exp(0.8516195)){
  1 - exp(-(x/scale)^(-shape))
} #poor

hr.cov.poorfog <- function(x, scale = exp(5.359), shape = exp(0.8516195)){
  1 - exp(-(x/scale)^(-shape))
} #poor-fog

#behavior
hr <- function(x, scale = exp(6.392), shape = exp(0.8490944)){
  1 - exp(-(x/scale)^(-shape))
}

hr.cov <- function(x, scale = exp(5.956), shape = exp(0.8490944)){
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
hr <- function(x, scale = exp(6.0851060), shape = exp(0.84355)){
  1 - exp(-(x/scale)^(-shape))
}

hr.cov <- function(x, scale = exp(6.26), shape = exp(0.84355)){
  1 - exp(-(x/scale)^(-shape))
}

###################################################################
#Plots

####ggplot?###########################################################
d <- qplot(x=sort(x), y=hr(x), newdata) +
  theme(text = element_text(size=20, family="Droid_Sans")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
  ylab("Distance (m)") +
  xlab("Probability of Detection")
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title.x = element_blank())
  
  ggplot(newdata, aes(x=sort(x), y=hr(x)), size = 5) + 
         theme(text = element_text(size=30, family="Droid_Sans") +
         theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
######################################################################

plot(sort(x), hr(x), xlim=c(0,3500), 
     main=NULL, cex.main = 1, 
     cex.lab = 1, cex.axis=1, xlab="Distance (m)", ylab="P(detection)", 
     ylim = c(0, 1.0), col = "black", lwd = 2, type = "l", 
     frame.plot = F, family = "Times New Roman")
lines(x, hr.cov(x), col = "black", lwd = 2, lty = 2)


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