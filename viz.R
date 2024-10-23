install.packages("dplyr")

library(dplyr)

install.packages("pillar")
install.packages("Rcpp")
install.packages("dplyr")

setwd("C:/Users/maxim/Documents/Data")
st <-  read.csv('Strava_clean.csv')
attach(st)

#Subsetting to remove outliers
st2 <-  select(filter(st, st$Mile > 4 & st$Per.Mile.Pace < 9 ), -st$Average.Temperature,-st$Max.Temperature)
st2 <-  filter(st, st$Mile > 4 & st$Per.Mile.Pace < 9 )


summary(lm(st2$Per.Mile.Pace~st2$Mile+st2$Average.Heart.Rate+st2$Average.Grade+st2$Elevation.Gain))

summary(lm(st2$Per.Mile.Pace~st2$Activity.ID))

#test
