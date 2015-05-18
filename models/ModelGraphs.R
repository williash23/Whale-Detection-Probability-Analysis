#Read in raw data file
library(Distance)
setwd("C:/Users/sara.williams/Documents/GitHub/Whale_DetectionProbability")

final_data <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale_DetectionProbability/data/final_data_14.csv")

#Histogram
bins <- seq(0,5000,by=500)

windowsFonts(A=windowsFont("Open Sans"))

hist(final_data$distance, breaks=bins, main=NULL,
     xlab="Distance (m)", ylab="Frequency", ylim = c(0, 800), 
     xlim = c(0, 5000),
     cex.lab = 1, cex.main = 1, cex.axis=1,
     border = "black", col = "grey75",  freq = T,
	 axes=F, family = "A")
axis(1, pos=0)
axis(2, pos=0)

segments(1000, 800, 1000, 0, col= "black" ,lty=3, lwd = 3.5)


lines(density(bin_data$distance))
axis(2, at=c(0, 750), cex.axis = 1)

########################################################
#Detection model functions - binned data
x <- seq(min(bin_data$distance), max(bin_data$distance))

#No Cov
hr <- function(x, scale = exp(6.829), shape = exp(0.898)){
  1 - exp(-(x/scale)^(-shape))
}

#Speed
#Speed scaled = 0 (or 14.5 knots), which is the mean
#Scaled and centered speed: 
#10.00209406 kts = -1.316359313 
#20.00890564 kts =	1.592541345
# hr <- function(x, scale = exp(6.849), shape = exp(.955)){
  # 1 - exp(-(x/scale)^(-shape))
# }
# hr.cov.10 <- function(x, scale = exp(6.783), shape = exp(.955)){
  # 1 - exp(-(x/scale)^(-shape)) #increased by 10 kts
# }
# hr.cov.20 <- function(x, scale = exp(6.929), shape = exp(.955)){
  # 1 - exp(-(x/scale)^(-shape)) #increased by 20 kts
# }

#Group size
hr <- function(x, scale = exp(6.777), shape = exp(0.926)){
  1 - exp(-(x/scale)^(-shape))
}
hr.cov.2 <- function(x, scale = exp(7.237), shape = exp(0.926)){
 1 - exp(-(x/scale)^(-shape))
 }
hr.cov.3 <- function(x, scale = exp(7.591), shape = exp(0.926)){
  1 - exp(-(x/scale)^(-shape))
}

#Visibility
hr <- function(x, scale = exp(6.907), shape = exp(0.928)){
  1 - exp(-(x/scale)^(-shape))
}#excellent - baseline
hr.cov.poor <- function(x, scale = exp(6.388), shape = exp(0.928)){
  1 - exp(-(x/scale)^(-shape))
} #poor
hr.cov.poorfog <- function(x, scale = exp(5.779), shape = exp(0.928)){
  1 - exp(-(x/scale)^(-shape))
} #poor-fog

#Behavior
hr <- function(x, scale = exp(6.836), shape = exp(0.907)){
  1 - exp(-(x/scale)^(-shape))
}
hr.cov.sa <- function(x, scale = exp(7.225), shape = exp(0.907)){
  1 - exp(-(x/scale)^(-shape))
}

# #Sea state
# hr <- function(x, scale = exp(6.831), shape = exp(0.897)){
  # 1 - exp(-(x/scale)^(-shape))
# }
# hr.cov <- function(x, scale = exp(), shape = exp(0.897)){
  # 1 - exp(-(x/scale)^(-shape))
# }

#All cov
hr <- function(x, scale = exp(6.870), shape = exp(0.964)){
  1 - exp(-(x/scale)^(-shape))
}
hr.cov <- function(x, scale = exp(8.026), shape = exp(0.965)){
  1 - exp(-(x/scale)^(-shape)) 
}

##############################################################################
#Plots

#Plot detection function
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
segments(0, a, 1000, a, col= "black" ,lty=1, lwd = 1.5)	 
	
#Add in line for covariate
lines(x, hr.cov.2(x), col = "black", lwd = 2, lty = 2)
lines(x, hr.cov.3(x), col = "black", lwd = 2, lty = 3)

#Adding 1000m lines for covariate
b <- hr.cov.2(1000)
segments(1000, b, 1000, 0, col= "black" ,lty=1, lwd = 1.5)
segments(0, b, 1000, b, col= "black" ,lty=1, lwd = 1.5)	 

c <- hr.cov.3(1000)
segments(1000, c, 1000, 0, col= "black" ,lty=1, lwd = 1.5)
segments(0, c, 1000, c, col= "black" ,lty=1, lwd = 1.5)	 
	 
	 
	 
	 
	 
# lines(x, hr.cov.20(x), col = "black", lwd = 2, lty = 3)	 
	

# abline(v=1000, h=, col= "black" ,lty=1, lwd = 1.75)
# abline(v=1000, h=0.5147578,col= "black" ,lty=1, lwd = 1.75)
# abline(v=1000, h=0.65242,col= "black" ,lty=1, lwd = 1.75)

# #For adding text or other line segments
# text(1700, 0.56, "50% Detection", cex = .75)



# legend("topright", legend = c("Group Size = 2", "Group Size = 1"),
       # col = c("black", "black"), lty = c(1,2), cex = .8, 
       # lwd = 2, xjust = 0.4)

# #Alternative histogram
# hist(bin_data$distance, breaks=bins, border = "gray", col = "gray")
# axis(2, at=c(0, 0.001), cex.axis = 0.75)

# points(620, 0.5, pch = 19, cex = 1, col = 1)
# points(445, 0.5, pch = 19, cex = 1, col = 1)