library(gtrendsR)
library(reshape2)
library(dplyr)
library(stargazer)
library(tidyr)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

#Set Language to Ukrainian. !
Sys.setlocale("LC_CTYPE", "ukrainian")

#Function to create cleaned data
trends_dat <- function(data,s1,s2,loc,ti,time,region,cityб){
  dat  <<- gtrends(c(s1,s2), gprop = "web", geo = c(loc), time = ti)  
  #Basic plot over time if needed
  # plot(dat) +   geom_line(size = 0.5) 
  
  #Fixing hits so that it's numeric and any <1 or nulls are set to .5 and 0
  t <- dat[[1]]
  t$hits <-  ifelse(t$hits == "<1",.5,t$hits)
  t$hits <-  as.numeric(ifelse(t$hits == "",0,t$hits))
  r <- dat[[3]]
  r$hits <-  ifelse(r$hits == "<1",.5,r$hits)
  r$hits <-  as.numeric(ifelse(r$hits == "",0,r$hits))
  c <- dat[[5]]
  c$hits <-  ifelse(c$hits == "<1",.5,c$hits)
  c$hits <-  as.numeric(ifelse(c$hits == "",0,c$hits))
    
  assign(deparse(substitute(data)), dat, envir=.GlobalEnv)
  assign(deparse(substitute(time)), t, envir=.GlobalEnv)
  assign(deparse(substitute(region)), r, envir=.GlobalEnv)
  assign(deparse(substitute(city)), c, envir=.GlobalEnv)
}

#If needed function to make data wide and reassigning variable names, creating ratio . Make sure Ukrainian first search term
wide_dat <- function(datz,output){
 
#To preserve column order
  cols <- c(colnames(datz)[!(colnames(datz) %in% c("keyword","hits"))], unique(datz$keyword))
  ra <- spread(datz,keyword, hits)
  # Select the columns in the order they are listed in the cols vector
  ra <- ra[,cols]
 n <- ncol(ra)
 names(ra)[(n-1):n] <- c("hits_ua","hits_ru")
 ra$ratio <- ra$hits_ua / ra$hits_ru
 
 assign(deparse(substitute(output)), ra, envir=.GlobalEnv)
}

#Time plot of ratio
time_plot <- function(dat,title) {
  ggplot(dat,aes(x=date, y=ratio)) + 
    geom_line() +
    theme(legend.position="bottom") +
    labs(title =title, x = "Date", y="Ratio") +
    theme(plot.title = element_text(hjust = 0.5)) +
    # geom_smooth(method = "lm", se = FALSE,color="red")
    geom_smooth(method = "loess", se = TRUE,color="red")
  
}

#what is
trends_dat(what,"що таке","что такое","UA","2004-01-01 2025-05-01",what_t,what_r,what_c)
wide_dat(what_t,time_wide)
whap <-  time_plot(time_wide,"що таке/что такое")

#How
trends_dat(how,"Як","Как","UA","2004-01-01 2024-10-23",what_t,what_r,what_c)
wide_dat(what_t,time_wide)
how <-  time_plot(time_wide,"Як/Как")

#news
trends_dat(news,"новини","новости","UA","2010-01-01 2024-10-23",time,region,city)
wide_dat(time,time_wide)
nep <-  time_plot(time_wide,"новини/новости")

#games
trends_dat(games,"ігри","игры","UA","2004-01-01 2024-10-23",time,region,city)
wide_dat(time,time_wide)
ga <-   time_plot(time_wide,"ігри/игры")

#Recipes
trends_dat(recipes,"рецепти","рецепты","UA","2004-01-01 2024-10-23",time,region,city)
wide_dat(time,time_wide)
re <-   time_plot(time_wide,"рецепти/рецепты")
re

#Price
trends_dat(test2,"ціна","цена","UA","2004-01-01 2024-10-23",time,region,city)
wide_dat(time,time_wide)
pr <-   time_plot(time_wide,"ціна/цена")
pr

#Grid
grid.arrange(whap, how,nep,ga,re,pr, nrow = 2,
             top="Graphs of Ukrainian/Russian Ratios over Time")

#Composite
trends_dat(test2,"що таке+новини+ігри+рецепти+Як+ціна","что такое+Как+новости+игры+рецепты+цена","UA","2004-01-01 2024-10-23",time,region,city)
wide_dat(time,time_wide)
com <-   time_plot(time_wide,"Composite")

#Composite since 2010
trends_dat(test2,"що таке+новини+ігри+рецепти+Як+ціна","что такое+Как+новости+игры+рецепты+цена","UA","2010-01-01 2024-10-23",time,region,city)
wide_dat(time,time_wide)
com2 <-   time_plot(time_wide,"Composite")

#unique geo codes
geo.codes = sort(unique(countries[substr(countries$sub_code, 1, 2) == "UA", ]$sub_code))

#unique codes with oblast names
ob <- unique(countries[substr(countries$sub_code, 1, 2) == "UA",])

#Check composite for each region
z <- list(1)
y <- list(1)
  for(i in 1:27) {

  z[[i]] <-   gtrends(c("що таке+новини+ігри+рецепти+Як+ціна","что такое+Как+новости+игры+рецепты+цена"), gprop = "web", time = "2010-01-01 2024-10-23",geo = geo.codes[i])[[1]]
  
  z[[i]]$hits <- ifelse(z[[i]]$hits == "<1",.5,z[[i]]$hits)
  z[[i]]$hits <- as.numeric(ifelse(z[[i]]$hits == "",0,z[[i]]$hits))

  cols <- c(colnames(z[[i]])[!(colnames(z[[i]]) %in% c("keyword","hits"))], unique(z[[i]]$keyword))
  y[[i]] <- spread(z[[i]],keyword, hits)
  # Select the columns in the order they are listed in the cols vector
  y[[i]] <- y[[i]][,cols]
  n <- ncol(y[[i]])
  names(y[[i]])[(n-1):n] <- c("hits_ua","hits_ru")
  y[[i]]$ratio <- y[[i]]$hits_ua / y[[i]]$hits_ru
  y[[i]]$Oblast <- filter(ob,sub_code == y[[i]]$geo[1])[1,3]
  
  } 

#Plot for each of the regions if needed
g <- list(1)
  for(i in 1:27) {
g[[i]] <-  time_plot(y[[i]],y[[i]]$Oblast[1])
  }

grid.arrange(g[[1]],g[[2]],g[[3]],g[[4]],g[[5]],g[[6]],g[[7]],g[[8]],g[[9]],g[[10]],g[[11]],g[[12]],g[[13]],
             g[[14]],g[[15]],g[[16]],g[[17]],g[[18]],g[[19]],g[[20]],g[[21]],g[[22]],g[[23]],g[[24]],g[[25]],
             g[[26]],g[[27]],
             top="Graphs of Ukrainian/Russian Ratios over Time",nrow = 9)
