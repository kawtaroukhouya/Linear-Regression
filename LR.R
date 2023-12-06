library(tidyverse)
library(broom)
library(ggplot2)
library(dplyr)
library(raster)
library(scatterplot3d)
#In this exercise we want to explore a climate data set from France. First, we download the data and get an overview.
clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")
str(clim)

clim$altitude <- as.numeric(gsub(",", "", clim$altitude))
clim$p_mean <- as.numeric(gsub(",", "", clim$p_mean))
str(clim)

G1 <- raster::getData(country = "France", level = 1)
library(ggplot2)
ggplot() +
  geom_polygon(
    data = G1,
    aes(x = long, y = lat, group = group),
    colour = "grey10", fill = "#fff7bc"
  ) +
  geom_point(
    data = clim,
    aes(x = lon, y = lat),
    alpha = .5,
    size = 2
  ) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_map()

climfrar<- clim[1:34, ]

mdl <- lm(t_mean ~ altitude+ lat +lon, data = climfrar)
summary(mdl)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) 37.2650364  2.6220099  14.212 7.29e-15
# altitude    -0.0064139  0.0008688  -7.383 3.17e-08
# lat         -0.5339603  0.0557546  -9.577 1.24e-10
# lon          0.0321010  0.0395728   0.811    0.424

clim$station
mdl2 <- lm(t_mean ~ altitude+lat, data = climfrar)
summary(mdl2)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) 37.9147567  2.4828724   15.27 5.68e-16
# altitude    -0.0062643  0.0008443   -7.42 2.34e-08
# lat         -0.5465325  0.0532610  -10.26 1.72e-11

newdata<-filter(clim,station%in% c('Mont-Ventoux', 'Pic-du-Midi'))
newdata
pred <- predict(mdl2, newdata, interval = "p", level = 0.95)
pred
# fit       lwr      upr
# 1  6.187574  3.814005 8.561143
# 2 -3.463727 -8.364328 1.436874

#Plot:

scatter_3d <- with(climfrar, scatterplot3d(altitude, lat, t_mean,
                                           pch = 16, highlight.3d = TRUE,
                                           angle = 45,
))
scatter_3d$plane3d(mdl2)
