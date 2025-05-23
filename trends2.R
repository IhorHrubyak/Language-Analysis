#Useful Sites
#For mapping
# https://davetang.org/muse/2018/12/31/visualising-google-trends-results-with-r/
# https://www.storybench.org/mapping-search-data-from-google-trends-in-r/
# https://martinctc.github.io/blog/vignette-google-trends-with-gtrendsr/

library(gtrendsR)
library(reshape2)
library(dplyr)
library(stargazer)
library(tidyr)

#Set Language to Ukrainian. HAVE TO RERUN ANYTIME FILE IS EVEN SAVED!
Sys.setlocale("LC_CTYPE", "ukrainian")

#Could possible create a function that queries based on search terms and time to automate the below

#Corona spliting for 2020 splitting based on 
corona  <- gtrends(c("??????????????????????","??????????????????????"), gprop = "web", geo = c("UA"), time = "2020-01-01 2021-10-31")
corona_time <- corona[[1]]
corona_region <- corona[[3]]
corona_city <- select(corona[[5]], hits, location,keyword)

#If need to check region codes in Ukraine
ua <- filter(data(countries), country_code == "UA")

corona_kiev  <- gtrends(c("??????????????????????","??????????????????????"), gprop = "web", geo="UA-30", time = "2020-01-01 2021-10-31")
#Not fully Clear if this is the city or the oblast
corona_lviv  <- gtrends(c("??????????????????????","??????????????????????"), gprop = "web", geo="UA-46", time = "2020-01-01 2021-10-31")


#Fixing hits and setting to 0 if <1
corona  <- gtrends(c("??????????????????????","??????????????????????"), gprop = "web", geo = c("UA"), time = "2020-01-01 2021-10-31")
corona_time <- corona[[1]]
corona_time$hits2 <- as.numeric(ifelse(corona_time$hits == "<1",0,corona_time$hits))

png("myplot.png")
myplot <-  ggplot(corona_time,aes(x=date, y=hits2,group=keyword)) + 
  geom_line(aes(color=keyword)) +
  theme(legend.position="bottom") +
  labs(title ="UA ?????????????????????? v RU ??????????????????????", x = "Date", y="Hits") +
  theme(plot.title = element_text(hjust = 0.5))
print(myplot)
dev.off()

#Table Ranking By Region

#Creating separte columns for two diff search terms
reg_ru <- corona_region  %>%
   filter(keyword=="??????????????????????") %>%
   arrange(location) %>% select(hits) 

reg_ua <- corona_region  %>%
  filter(keyword=="??????????????????????") %>%
   arrange(location) %>% select(location,hits) 

#Renaming above columsna nd cbind
reg_ua <- rename(reg_ua,hits_ua=hits)
reg_ru <- rename(reg_ru,hits_ru=hits)
regions_tab <- cbind(reg_ua,reg_ru)
#Setting Russian to numeric and sorting by it. Could be better to look at rus realtive to ukr as ratio?
regions_tab$hits_ua <-  ifelse(regions_tab$hits_ua == "<1",0,regions_tab$hits_ua)
regions_tab$hits_ua <-  as.numeric(ifelse(regions_tab$hits_ua == "",0,regions_tab$hits_ua))
regions_tab$hits_ru <- as.numeric(regions_tab$hits_ru)

regions_tab$rat <- regions_tab$hits_ua / regions_tab$hits_ru

regions_tab <-  arrange(regions_tab, -regions_tab$rat)
names(regions_tab) <- c("Oblast/City", "Hits ??????????????????????", "Hits ??????????????????????", "Ratio UA/RU")
stargazer(regions_tab,summary=FALSE,
          title= "Ukrainian Oblasts and Cities Ranked by Hits in Russian", notes = "Crime and Sevastapol had insignifant Ukrainian",header =FALSE,
          type="text")

#News over all time splitting list into necessary dataframes
google.trends_news = gtrends(c("??????????????????????","??????????????????????"), gprop = "web", time = "2020-01-01 2021-10-31",geo = c("UA"))
news_time <- google.trends_news[[1]]
news_region <- google.trends_news[[3]]
news_city <- select(google.trends_news[[5]], hits, location,keyword)

#Making data wide
news_time$hits <- as.numeric(ifelse(news_time$hits == "<1",0,news_time$hits))
news_time_wide <- spread(news_time, keyword , hits)
names(news_time_wide)[6:7] <- c("UA","RU")

#Ratio 
news_time_wide$rat <- news_time_wide$RU/news_time_wide$UA
news_time_wide$rat2 <-news_time_wide$UA/ news_time_wide$RU
#Ratio over full span of data
ggplot(news_time_wide,aes(x=date, y=rat)) + 
  geom_line() +
  theme(legend.position="bottom") +
  labs(title ="2020 ????????????/?????????????? Ratio ", x = "Date", y="Ratio") +
  theme(plot.title = element_text(hjust = 0.5)) +
   geom_smooth(method = "lm", se = FALSE,color="red")

#Ratio after 2010
t <-  subset(news_time_wide , date> "2010-01-01")
ggplot(t,aes(x=date, y=rat)) + 
  geom_line() +
  theme(legend.position="bottom") +
  labs(title ="2020 ????????????/?????????????? Ratio ", x = "Date", y="Ratio") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm", se = FALSE,color="red")

#Could look at which regions had the biggest change in their ratio. For this a function would be needed 
news_ky <-  gtrends(c("??????????????","????????????"), gprop = "web", time = "all",geo = c("UA-30"))[[1]]
news_ky_wide <- spread(news_ky, keyword , hits)
#Ratio 
news_ky_wide$rat <- news_ky_wide$????????????/news_ky_wide$??????????????

t <-  subset(news_ky_wide , date> "2010-01-01")
ggplot(t,aes(x=date, y=rat)) + 
  geom_line() +
  theme(legend.position="bottom") +
  labs(title ="2020 ????????????/?????????????? Ratio ", x = "Date", y="Ratio") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm", se = FALSE,color="red")


#Basic Plot
plot(google.trends_news) + 
  my_theme() +
  geom_line(size = 0.5) 


geo.codes = sort(unique(countries[substr(countries$sub_code, 1, 2) == "UA", ]$sub_code))

google.trends = gtrends(c("????????????","????????????"), geo = c("UA"), gprop = "web", time = "2010-06-30 2017-06-30")
google.trends = gtrends(c("????????????","????????????"), geo = c("UA"), gprop = "web", time = "2019-06-30 2020-06-30", hl='ru')
plot(google.trends)

res <- gtrends(c("????????????","????????????"), geo = c("UA", "UA"),gprop = "web")
plot(res)

mov <- gtrends(c("????????????+????????????","????????????+????????????"), geo = c("UA", "UA"),gprop = "web")
plot(mov)  
  
trans <- gtrends(c("????????????????????+????????????","??????????????????+??????????????"), geo = c("UA", "UA"),gprop = "web",time = "today+5-y")
trans <- gtrends(c("????????????????????+????????????","??????????????????+??????????????"), geo = c("UA"),gprop = "web",time = "all")
plot(trans)  

sort(unique(countries))

countries %>% 
filter(countries$name == "UKRAINE" )

#Use a composite of the most popular search terms
#https://trends.google.com/trends/?geo=UA
cornoa <- gtrends(c("??????????????????????","??????????????????????"), geo = c("UA"),gprop = "web",time = "2020-01-01 2020-12-27")
plot(cornoa)  
#Subsetting to region. Can I not have time periods by region?
corona_region <- cornoa[[3]]
#only Kharkiv as an example

kh <- filter(corona_region, location == "Kharkiv Oblast")

#News Ukraine seems like a good popular search
news <- gtrends(c("?????????????? ??????????????","???????????? ??????????????"), geo = c("UA"),gprop = "web",time = "2020-01-01 2020-12-27")
plot(news)  
#Simply check of it's popularity in Kharkiv in 2013 v now
news20 <- gtrends(c("?????????????? ??????????????","???????????? ??????????????"), geo = c("UA"),gprop = "web",time = "2020-01-01 2020-12-27")
news13 <- gtrends(c("?????????????? ??????????????","???????????? ??????????????"), geo = c("UA"),gprop = "web",time = "2013-01-01 2013-12-31")
#In Kharkiv in both time periods, could write function or something to loop over multiple regions or time periods

#Need to check how the proportions work exactly between the search terms
kh20 <- filter(news20[[3]], location == "Kharkiv Oblast")
kh13 <- filter(news13[[3]], location == "Kharkiv Oblast")

#How often are different news sources searched by region?


