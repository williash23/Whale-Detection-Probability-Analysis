#  1/13/2015
#  Detection probability analysis - generating detection funcations with actual values and 
#   creating detection function plots.
################################################################################

#  Load packages.
library(ggplot2)
library(dplyr)
library(grid)

#  Read in final data set.
final_dat <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale-Detection-Probability-Analysis/data/final_dat_2015.csv")

#  Truncate to the 85th percentile so functions only go to the range of those estimated
#   through detection function models.
percentile85 <- quantile(final_dat$distance, probs=0.85, na.rm=TRUE)
percentile85
final_dat_truncated85 <- final_dat[final_dat$distance < 4565,]


#  Generate values from each model to plot across distance range (0 to 4565)

#  Distance values (m)
x <- seq(min(final_dat_truncated85$distance), 
         max(final_dat_truncated85$distance))

#  Detection probability values creation.

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
hr.beh.sa <- function(x, scale = exp(7.255), shape = exp(0.747)){
                       1 - exp(-(x/scale)^(-shape))
            } # surface-active
            hr.beh.sa_l <- function(x, scale = exp(7.255-ci_beh_sa), shape = exp(0.747)){
                       1 - exp(-(x/scale)^(-shape))
            }
            hr.beh.sa_u <- function(x, scale = exp(7.255+ci_beh_sa), shape = exp(0.747)){
                       1 - exp(-(x/scale)^(-shape))
            }

            
# Waves
hr <- function(x, scale = exp(6.831), shape = exp(0.737)){
               1 - exp(-(x/scale)^(-shape))
       }
hr.cov <- function(x, scale = exp(), shape = exp(0.737)){
                   1 - exp(-(x/scale)^(-shape))
                   }

           
#All cov
se_common <- 0.04637927 
ci_common <- 1.96*se_common

se_best <- 0.16764059 + 0.10436279
ci_best <- 1.96*se_best

se_worst <- 0.25941451
ci_worst <- 1.96*se_worst

hr.common <- function(x, scale = exp(6.833), shape = exp(0.824)){
  1 - exp(-(x/scale)^(-shape))
}# Most common scenario: 1 whale, blow, excellent
      hr.common_l <- function(x, scale = exp(6.833-ci_common), shape = exp(0.824)){
               1 - exp(-(x/scale)^(-shape))
       }
       hr.common_u <- function(x, scale = exp(6.833+ci_common), shape = exp(0.824)){
               1 - exp(-(x/scale)^(-shape))
       }
hr.best <- function(x, scale = exp(8.167), shape = exp(0.824)){
  1 - exp(-(x/scale)^(-shape)) 
}# Best case scenario: 4+ whales, surface active, excellent
      hr.best_l <- function(x, scale = exp(8.167-ci_best), shape = exp(0.824)){
               1 - exp(-(x/scale)^(-shape))
       }
       hr.best_u <- function(x, scale = exp(8.167+ci_best), shape = exp(0.824)){
               1 - exp(-(x/scale)^(-shape))
       }
hr.worst <- function(x, scale = exp(5.555), shape = exp(0.824)){
  1 - exp(-(x/scale)^(-shape)) 
}# Worst case scenario: 1 whale, blow, poor-fog
      hr.worst_l <- function(x, scale = exp(5.555-ci_worst), shape = exp(0.824)){
               1 - exp(-(x/scale)^(-shape))
       }
       hr.worst_u <- function(x, scale = exp(5.555+ci_worst), shape = exp(0.824)){
               1 - exp(-(x/scale)^(-shape))
       }

##############################################################################
#  Plots

#  Histogram - equal area bins

#  Get N equal area bins with maximum radius R
#   Function to generate all cutpoints (= all radii lengths)
 calc_rad <- function(R = R,  N = N){
  all_r <- vector(mode = "numeric", length = 0)
  for(i in 1:N){
    r <- R*(sqrt(i/N))
    all_r <- c(all_r, r)
  }
  return(all_r)
}
R <- 5000 # maxiumum radius
N <- 10 # number of equal area bins desired
cutpts <- calc_rad(R, N)

#  Function to generate areas of all half circles at these cutpoints to check 
half_circ_area <- function(r){
                  area <- (pi*(r^2))/2
                  print(area)
                  }
areas <- half_circ_area(cutpts)  
#   If cutpoints are correct, areas[10] - areas[9], areas[9] - areas[8],
#   etc., should all equal areas[1]

#  Generate histogram
#   Use cutpts as breaks: 
#   cutpts
#   1581.139 2236.068 2738.613 3162.278 3535.534 3872.983 4183.300 4472.136 4743.416 5000.000

plot_dist <- ggplot(final_dat_truncated85, aes(x=distance)) + 
                     geom_histogram(fill="grey", colour = "black", 
                     breaks= c(0, 1581.139, 2236.068, 2738.613, 3162.278, 3535.534, 3872.983, 4183.300, 4472.136, 4743.416, 5000.000)) +
                     xlab("\nDistances of first sightings (m)") +
                     ylab("Frequency              \n") +
                     theme_bw() +
                     theme(panel.grid.major.x=element_blank(), panel.grid.minor=element_blank()) +
                     theme(axis.title=element_text(size=15), axis.text=element_text(size=12)) +
                     xlim(0, 5000)
plot_dist


#   No covariates
no_cov_dat <- as.data.frame(cbind(x, hr.no_cov(x), hr.no_cov_l(x), hr.no_cov_u(x)))
a <- hr.no_cov(1000)
no_cov_plot <- ggplot(no_cov_dat, aes(x=x, y=V2)) +
                            geom_line(size = 1) +
                            geom_ribbon(data=no_cov_dat, aes(x=x, ymin=V3,ymax=V4),
                            alpha=0.4,  fill="grey70") +
                            xlab("\nDistance from ship (m)") +
                            ylab("Detection probability\n") +
                            theme_bw() +
                            theme(panel.grid.major.x=element_blank(), panel.grid.minor=element_blank()) +
                            theme(axis.title=element_text(size=15), axis.text=element_text(size=12)) +
                            geom_segment(aes(x = 1000, y = a, xend = 1000, yend = 0), arrow = arrow(angle = 15, ends = "first", length = unit(0.4, "cm"), type = "closed")) +
                            geom_segment(aes(x = 0, y = a, xend = 1000, yend = a), arrow = arrow(angle = 15, ends = "last", length = unit(0.4, "cm"), type = "closed"))  +
                            scale_x_continuous(expand = c(0, 0)) +
                            scale_y_continuous(expand = c(0, 0.01))
no_cov_plot

#  Attempts to plot PDF of detection function on top of histogram
hist(final_dat_truncated85$distance, axes=F, xlab=NA, ylab=NA, breaks= c(0, 1581.139, 2236.068, 2738.613, 3162.278, 3535.534, 3872.983, 4183.300, 4472.136, 4743.416, 5000.000), border = "black")
par(new = T)
plot(no_cov_dat$x, no_cov_dat$V2, xlab="Distance from ship (m)", ylab="Detection probability", lwd = 0.75) 

no_cov_plot <- ggplot(no_cov_dat, aes(x=x, y=V2)) +
                            geom_line(size = 1) +
                            ylab("Detection probability\n") +
                            theme_bw() +
                            theme(panel.grid.major.x=element_blank(), panel.grid.minor=element_blank()) +
                            theme(axis.title=element_text(size=15), axis.text=element_text(size=12))
                                                                       
library(gtable)
library(grid)

grid.newpage()

# two plots
p2 <- no_cov_plot
p1 <- plot_dist

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
    pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)

#   Group size
count_dat <- as.data.frame(cbind(x, hr.count(x), hr.count_l(x), hr.count_u(x)))
count_dat_2 <-as.data.frame(cbind(x, hr.count.2(x), hr.count.2_l(x), hr.count.2_u(x)))
count_dat_lg <-as.data.frame(cbind(x, hr.count.lg(x), hr.count.lg_l(x), hr.count.lg_u(x)))
a <- hr.count(1000)
b <- hr.count.2(1000)
c <- hr.count.lg(1000)
count_plot <- ggplot(count_dat, aes(x=x, y=V2)) +
                   geom_line(size = 1) +
                   geom_ribbon(data=count_dat, aes(x=x, ymin=V3,ymax=V4),
                   alpha=0.4,  fill="grey70") +
                   geom_line(data=count_dat_2, aes(x=x, y=V2), size = 1, linetype= 5) +
                   geom_ribbon(data=count_dat_2, aes(x=x, ymin=V3,ymax=V4),
                   alpha=0.4,  fill="grey70") +
                   geom_line(data=count_dat_lg, aes(x=x, y=V2), size = 1,  linetype= 3) +
                   geom_ribbon(data=count_dat_lg, aes(x=x, ymin=V3,ymax=V4),
                   alpha=0.4,  fill="grey70") +
                   xlab("\nDistance from ship (m)") +
                   ylab("Detection probability\n") +
                   theme_bw() +
                   theme(panel.grid.major.x=element_blank(),
                   panel.grid.minor=element_blank()) +
                   theme(axis.title=element_text(size=15), 
                  axis.text=element_text(size=12)) +
                  geom_segment(aes(x = 1000, y = a, xend = 1000, yend = 0), arrow = arrow(angle = 15, ends = "first", length = unit(0.4, "cm"), type = "closed")) +
                  geom_segment(aes(x = 0, y = a, xend = 1000, yend = a), arrow = arrow(angle = 15, ends = "last", length = unit(0.43, "cm"), type = "closed")) +
                  geom_segment(aes(x = 1000, y = b, xend = 1000, yend = 0), arrow = arrow(angle = 15, ends = "first", length = unit(0.4, "cm"), type = "closed")) +
                  geom_segment(aes(x = 0, y = b, xend = 1000, yend = b), arrow = arrow(angle = 15, ends = "last", length = unit(0.4, "cm"), type = "closed")) +
                  geom_segment(aes(x = 1000, y = c, xend = 1000, yend = 0), arrow = arrow(angle = 15, ends = "first", length = unit(0.4, "cm"), type = "closed")) +
                  geom_segment(aes(x = 0, y = c, xend = 1000, yend = c), arrow = arrow(angle = 15, ends = "last", length = unit(0.4, "cm"), type = "closed"))  +
                  scale_x_continuous(expand = c(0, 0)) +
                  scale_y_continuous(expand = c(0, 0.01))
count_plot


#   Visibility
vis_dat <- as.data.frame(cbind(x, hr.vis(x), hr.vis_l(x), hr.vis_u(x)))
vis_dat_p <-as.data.frame(cbind(x, hr.vis.p(x), hr.vis.p_l(x), hr.vis.p_u(x)))
vis_dat_pf <-as.data.frame(cbind(x, hr.vis.pf(x), hr.vis.pf_l(x), hr.vis.pf_u(x)))
a <- hr.vis(1000)
b <- hr.vis.p(1000)
c <- hr.vis.pf(1000)
vis_plot <- ggplot(vis_dat, aes(x=x, y=V2)) +
                   geom_line(size = 1) +
                   geom_ribbon(data=vis_dat, aes(x=x, ymin=V3,ymax=V4),
                   alpha=0.4,  fill="grey70") +
                   geom_line(data=vis_dat_p, aes(x=x, y=V2), size = 1, linetype= 5) +
                   geom_ribbon(data=vis_dat_p, aes(x=x, ymin=V3,ymax=V4),
                   alpha=0.4,  fill="grey70") +
                   geom_line(data=vis_dat_pf, aes(x=x, y=V2), size = 1, linetype= 3) +
                   geom_ribbon(data=vis_dat_pf, aes(x=x, ymin=V3,ymax=V4),
                   alpha=0.4,  fill="grey70") +
                   xlab("\nDistance from ship (m)") +
                   ylab("Detection probability\n") +
                   theme_bw() +
                   theme(panel.grid.major.x=element_blank(),
                   panel.grid.minor=element_blank()) +
                   theme(axis.title=element_text(size=15), 
                  axis.text=element_text(size=12)) +
                  geom_segment(aes(x = 1000, y = a, xend = 1000, yend = 0), arrow = arrow(angle = 15, ends = "first", length = unit(0.4, "cm"), type = "closed")) +
                  geom_segment(aes(x = 0, y = a, xend = 1000, yend = a), arrow = arrow(angle = 15, ends = "last", length = unit(0.43, "cm"), type = "closed")) +
                  geom_segment(aes(x = 1000, y = b, xend = 1000, yend = 0), arrow = arrow(angle = 15, ends = "first", length = unit(0.4, "cm"), type = "closed")) +
                  geom_segment(aes(x = 0, y = b, xend = 1000, yend = b), arrow = arrow(angle = 15, ends = "last", length = unit(0.4, "cm"), type = "closed")) +
                  geom_segment(aes(x = 1000, y = c, xend = 1000, yend = 0), arrow = arrow(angle = 15, ends = "first", length = unit(0.4, "cm"), type = "closed")) +
                  geom_segment(aes(x = 0, y = c, xend = 1000, yend = c), arrow = arrow(angle = 15, ends = "last", length = unit(0.4, "cm"), type = "closed"))  +
                  scale_x_continuous(expand = c(0, 0)) +
                  scale_y_continuous(expand = c(0, 0.01))
vis_plot


#   Behavior
beh_dat <- as.data.frame(cbind(x, hr.beh(x), hr.beh_l(x), hr.beh_u(x)))
beh_dat_sa <-as.data.frame(cbind(x, hr.beh.sa(x), hr.beh.sa_l(x), hr.beh.sa_u(x)))
a <- hr.beh(1000)
b <- hr.beh.sa(1000)
beh_plot <- ggplot(beh_dat, aes(x=x, y=V2)) +
                   geom_line(size = 1) +
                   geom_ribbon(data=beh_dat, aes(x=x, ymin=V3,ymax=V4),
                   alpha=0.4,  fill="grey70") +
                   geom_line(data=beh_dat_sa, aes(x=x, y=V2), size = 1, linetype= 5) +
                   geom_ribbon(data=beh_dat_sa, aes(x=x, ymin=V3,ymax=V4),
                   alpha=0.4,  fill="grey70") +
                    xlab("\nDistance from ship (m)") +
                   ylab("Detection probability\n") +
                   theme_bw() +
                   theme(panel.grid.major.x=element_blank(),
                   panel.grid.minor=element_blank()) +
                   theme(axis.title=element_text(size=15), 
                  axis.text=element_text(size=12)) +
                  geom_segment(aes(x = 1000, y = a, xend = 1000, yend = 0), arrow = arrow(angle = 15, ends = "first", length = unit(0.4, "cm"), type = "closed")) +
                  geom_segment(aes(x = 0, y = a, xend = 1000, yend = a), arrow = arrow(angle = 15, ends = "last", length = unit(0.43, "cm"), type = "closed")) +
                  geom_segment(aes(x = 1000, y = b, xend = 1000, yend = 0), arrow = arrow(angle = 15, ends = "first", length = unit(0.4, "cm"), type = "closed")) +
                  geom_segment(aes(x = 0, y = b, xend = 1000, yend = b), arrow = arrow(angle = 15, ends = "last", length = unit(0.4, "cm"), type = "closed"))  +
                  scale_x_continuous(expand = c(0, 0)) +
                  scale_y_continuous(expand = c(0, 0.01))
beh_plot


#   Most common, best and worst case scenarios
common_dat <- as.data.frame(cbind(x, hr.common(x)))
best_dat <-as.data.frame(cbind(x, hr.best(x)))
worst_dat <-as.data.frame(cbind(x, hr.worst(x)))
a <- hr.common(1000)
b <- hr.best(1000)
c <- hr.worst(1000)
scenario_plot <- ggplot(common_dat, aes(x=x, y=V2)) +
                  geom_line(size = 1) +
                  geom_line(data=best_dat, aes(x=x, y=V2), size = 1, linetype= 5) +
                  geom_line(data=worst_dat, aes(x=x, y=V2), size = 1, linetype= 3) +
                  xlab("\nDistance from ship (m)") +
                  ylab("Detection probability\n") +
                  theme_bw() +
                  theme(panel.grid.major.x=element_blank(),
                  panel.grid.minor=element_blank()) +
                  theme(axis.title=element_text(size=15), 
                 axis.text=element_text(size=12)) +
                  geom_segment(aes(x = 1000, y = a, xend = 1000, yend = 0), arrow = arrow(angle = 15, ends = "first", length = unit(0.4, "cm"), type = "closed")) +
                  geom_segment(aes(x = 0, y = a, xend = 1000, yend = a), arrow = arrow(angle = 15, ends = "last", length = unit(0.43, "cm"), type = "closed")) +
                  geom_segment(aes(x = 1000, y = b, xend = 1000, yend = 0), arrow = arrow(angle = 15, ends = "first", length = unit(0.4, "cm"), type = "closed")) +
                  geom_segment(aes(x = 0, y = b, xend = 1000, yend = b), arrow = arrow(angle = 15, ends = "last", length = unit(0.4, "cm"), type = "closed")) +
                  geom_segment(aes(x = 1000, y = c, xend = 1000, yend = 0), arrow = arrow(angle = 15, ends = "first", length = unit(0.4, "cm"), type = "closed")) +
                  geom_segment(aes(x = 0, y = c, xend = 1000, yend = c), arrow = arrow(angle = 15, ends = "last", length = unit(0.4, "cm"), type = "closed")) +
                  scale_x_continuous(expand = c(0, 0)) +
                  scale_y_continuous(expand = c(0, 0.01))
scenario_plot

# #   Most common, best and worst case scenarios with CIs.
# common_dat <- as.data.frame(cbind(x, hr.common(x), hr.common_l(x), hr.common_u(x)))
# best_dat <-as.data.frame(cbind(x, hr.best(x), hr.best_l(x), hr.best_u(x)))
# worst_dat <-as.data.frame(cbind(x, hr.worst(x), hr.worst_l(x), hr.worst_u(x)))
# scenario_plot_wCI <- ggplot(common_dat, aes(x=x, y=V2)) +
                  # geom_line(size = 1) +
                  # geom_ribbon(data=common_dat, aes(x=x, ymin=V3,ymax=V4),
                  # alpha=0.4,  fill="grey70") +
                  # geom_line(data=best_dat, aes(x=x, y=V2), size = 1, linetype= 5) +
                   # geom_ribbon(data=best_dat, aes(x=x, ymin=V3,ymax=V4),
                  # alpha=0.4,  fill="grey70") +
                  # geom_line(data=worst_dat, aes(x=x, y=V2), size = 1, linetype= 3) +
                   # geom_ribbon(data=worst_dat, aes(x=x, ymin=V3,ymax=V4),
                  # alpha=0.4,  fill="grey70") +
                  # xlim(0, 5000) +
                  # ylim(0, 1) +
                  # xlab("\nDistance from ship (m)") +
                  # ylab("Detection probability\n") +
                  # theme_bw() +
                  # theme(panel.grid.major.x=element_blank(),
                  # panel.grid.minor=element_blank()) +
                  # theme(axis.title=element_text(size=15), 
                 # axis.text=element_text(size=12))
# scenario_plot_wCI
##############################################################################

#  Plots for SMM 2015 conference poster
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

#  Speed
#   Speed scaled = 0 (or 14.6 knots), which is the mean
#   Scaled and centered speed: 
#   10.002 kts = -1.388
#   20.009 kts =  1.618

# se_int <- 0.04519963
# ci_int <- 1.96*se_int

# se_cov <- 0.02750598
# ci_cov <- 1.96*se_cov

# hr.int <- function(x, scale = exp(6.724), shape = exp(0.737)){
               # 1 - exp(-(x/scale)^(-shape))
        # }
        # hr.int_l <- function(x, scale = exp(6.724-ci_int), shape = exp(0.737)){
                 # 1 - exp(-(x/scale)^(-shape))
         # }
         # hr.int_u <- function(x, scale = exp(6.724+ci_int), shape = exp(0.737)){
                 # 1 - exp(-(x/scale)^(-shape))
         # }
# hr.cov <- function(x, scale = exp(6.821), shape = exp(0.737)){
                       # 1 - exp(-(x/scale)^(-shape)) #increased by 10 kts
        # }
        # hr.cov_l <- function(x, scale = exp(6.821-ci_cov), shape = exp(0.737)){
               # 1 - exp(-(x/scale)^(-shape))
        # }
        # hr.cov_u <- function(x, scale = exp(6.821+ci_cov), shape = exp(0.737)){
               # 1 - exp(-(x/scale)^(-shape))
        # }    


# #   Ship speed
# speed_dat <- as.data.frame(cbind(x, hr.int(x), hr.int_l(x), hr.int_u(x)))
# speed_dat_cov <-as.data.frame(cbind(x, hr.cov(x), hr.cov_l(x), hr.cov_u(x)))
# speed_plot <- ggplot(speed_dat, aes(x=x, y=V2)) +
                   # geom_line(size = 1) +
                   # geom_ribbon(data=speed_dat, aes(x=x, ymin=V3,ymax=V4),
                   # alpha=0.4,  fill="grey70") +
                   # geom_line(data=speed_dat_cov, aes(x=x, y=V2), size = 1, linetype= 5) +
                   # geom_ribbon(data=speed_dat_cov, aes(x=x, ymin=V3,ymax=V4),
                   # alpha=0.4,  fill="grey70") +
                   # xlim(0, 5000) +
                   # ylim(0, 1) +
                    # xlab("\nDistance from ship (m)") +
                   # ylab("Detection probability\n") +
                   # theme_bw() +
                   # theme(panel.grid.major.x=element_blank(),
                   # panel.grid.minor=element_blank()) +
                   # theme(axis.title=element_text(size=15), 
                  # axis.text=element_text(size=12))
# speed_plot        
##############################################################################

# # Old format - Plot detection function
# plot(sort(x), hr(x), xlim=c(-10,5000),
   # axes=F,
     # main="Detection as  a function of distance and ship speed", cex.main = 1, 
     # cex.lab = 1, cex.axis=1, xlab="Distance (m)", ylab="P(detection)", 
     # ylim = c(0, 1.0), col = "black", lwd = 2, type = "l", 
     # frame.plot = F)
# axis(1, pos=0)
# axis(2, pos=0)

# #Adding 1000m lines for null
# a <- hr(1000)
# segments(1000, a, 1000, 0, col= "black" ,lty=1, lwd = 1.5)
# segments(0, a, 1000, a, col= "black" ,lty=1, lwd = 1.5)   
  
# #Add in line for covariate
# lines(x, hr.cov.10(x), col = "black", lwd = 2, lty = 2)
# lines(x, hr.cov.20(x), col = "black", lwd = 2, lty = 3)

# #Adding 1000m lines for covariate
# b <- hr.cov.10(1000)
# segments(1000, b, 1000, 0, col= "black" ,lty=1, lwd = 1.5)
# segments(0, b, 1000, b, col= "black" ,lty=1, lwd = 1.5)   

# c <- hr.cov.20(1000)
# segments(1000, c, 1000, 0, col= "black" ,lty=1, lwd = 1.5)
# segments(0, c, 1000, c, col= "black" ,lty=1, lwd = 1.5)   
   
   
# # Plot 60% detection comparison of scenarios
# plot(sort(x), hr(x), xlim=c(-10,5000),
   # axes=F,
     # main=NULL, cex.main = 1, 
     # cex.lab = 1, cex.axis=1, xlab="Distance (m)", ylab="P(detection)", 
     # ylim = c(0, 1.0), col = "black", lwd = 2, type = "l", 
     # frame.plot = F)
# axis(1, pos=0)
# axis(2, pos=0)

# #Adding 1000m lines for null
# a <- hr(1000)
# segments(1000, a, 1000, 0, col= "black" ,lty=1, lwd = 1.5)
# #segments(0, a, 1000, a, col= "black" ,lty=1, lwd = 1.5)   
  
# #Add in line for covariate
# lines(x, hr.cov(x), col = "black", lwd = 2, lty = 2)
# lines(x, hr.cov.2(x), col = "black", lwd = 2, lty = 3)

# #Adding 1000m lines for covariate
# b <- hr.cov(3170)
# segments(3170, b, 3170, 0, col= "black" ,lty=1, lwd = 1.5)
# segments(0, b, 3170, b, col= "black" ,lty=1, lwd = 1.5)   

# c <- hr.cov.2(310)
# segments(310, c, 310, 0, col= "black" ,lty=1, lwd = 1.5)
# #segments(0, c, 310, c, col= "black" ,lty=1, lwd = 1.5)   