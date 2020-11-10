library(readr)
library(tidyverse)
library(robotstxt)
library(rvest)
library(knitr)
library(janitor)
library(shiny)
library(rtweet)
library(readxl)

#Roethlisberger, Jones, Bridgewater, Mayfield, Minshew, Wilson, Brees, Murray

#BEN ROETHLISBERGER

#Load gamelogs data
Ben_Roethlisberger <- read_excel("gamelogs/Ben_Roethlisberger.xlsx")

#Wrangle gamelogs data
Ben_Roethlisberger_logs <- Ben_Roethlisberger %>%
  mutate(total_yards = pyds + ryds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pyds + 0.1*ryds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n())) %>%
  filter(Week <=7)

#Dates from Game Logs (used in merge with twitter data)
Ben_Roethlisberger_dates <- Ben_Roethlisberger_logs %>%
  select(Date)

#Twitter Scrape of Tweets
Ben_Roethlisberger_tweets <- get_timeline("@_BigBen7", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(Ben_Roethlisberger_dates$Date[1]), 1, 
              ifelse(date > as.Date(Ben_Roethlisberger_dates$Date[1]) & date <= as.Date(Ben_Roethlisberger_dates$Date[2]), 2,
                     ifelse(date > as.Date(Ben_Roethlisberger_dates$Date[2]) & date <= as.Date(Ben_Roethlisberger_dates$Date[3]), 3,
                            ifelse(date > as.Date(Ben_Roethlisberger_dates$Date[3]) & date <= as.Date(Ben_Roethlisberger_dates$Date[4]), 4,
                                   ifelse(date > as.Date(Ben_Roethlisberger_dates$Date[4]) & date <= as.Date(Ben_Roethlisberger_dates$Date[5]), 5,
                                          ifelse(date > as.Date(Ben_Roethlisberger_dates$Date[5]) & date <= as.Date(Ben_Roethlisberger_dates$Date[6]), 6,
                                                 ifelse(date > as.Date(Ben_Roethlisberger_dates$Date[6]) & date <= as.Date(Ben_Roethlisberger_dates$Date[7]), 7, "error")))))))
                                        
  )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())

#Combined football and twitter data
Ben_Roethlisberger_data <- Ben_Roethlisberger_tweets %>%
  left_join(Ben_Roethlisberger_logs, by="Week") %>%
  mutate(player = "Ben_Roethlisberger")


#DANIEL JONES

#Load gamelogs data
Daniel_Jones <- read_excel("gamelogs/Daniel_Jones.xlsx")

#Wrangle gamelogs data
Daniel_Jones_logs <- Daniel_Jones %>%
  mutate(total_yards = pyds + ryds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pyds + 0.1*ryds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n())) %>%
  filter(Week <=7)

#Dates from Game Logs (used in merge with twitter data)
Daniel_Jones_dates <- Daniel_Jones_logs %>%
  select(Date)

#Twitter Scrape of Tweets
Daniel_Jones_tweets <- get_timeline("@Daniel_Jones10", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(Daniel_Jones_dates$Date[1]), 1, 
                             ifelse(date > as.Date(Daniel_Jones_dates$Date[1]) & date <= as.Date(Daniel_Jones_dates$Date[2]), 2,
                                    ifelse(date > as.Date(Daniel_Jones_dates$Date[2]) & date <= as.Date(Daniel_Jones_dates$Date[3]), 3,
                                           ifelse(date > as.Date(Daniel_Jones_dates$Date[3]) & date <= as.Date(Daniel_Jones_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(Daniel_Jones_dates$Date[4]) & date <= as.Date(Daniel_Jones_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(Daniel_Jones_dates$Date[5]) & date <= as.Date(Daniel_Jones_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(Daniel_Jones_dates$Date[6]) & date <= as.Date(Daniel_Jones_dates$Date[7]), 7, "error")))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())

#Combined football and twitter data NOTE: DOES NOT WORK SINCE ZERO TWEETS SINCE SEPT 1
Daniel_Jones_data <- Daniel_Jones_tweets %>%
  left_join(Daniel_Jones_logs, by="Week") %>%
  mutate(player = "Daniel_Jones")

#TEDDY BRIDGEWATER

#Load gamelogs data
Teddy_Bridgewater <- read_excel("gamelogs/Teddy_Bridgewater.xlsx")

#Wrangle gamelogs data
Teddy_Bridgewater_logs <- Teddy_Bridgewater %>%
  mutate(total_yards = pyds + ryds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pyds + 0.1*ryds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n())) %>%
  filter(Week <=7)

#Dates from Game Logs (used in merge with twitter data)
Teddy_Bridgewater_dates <- Teddy_Bridgewater_logs %>%
  select(Date)

#Twitter Scrape of Tweets
Teddy_Bridgewater_tweets <- get_timeline("@teddyb_h2o", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(Teddy_Bridgewater_dates$Date[1]), 1, 
                             ifelse(date > as.Date(Teddy_Bridgewater_dates$Date[1]) & date <= as.Date(Teddy_Bridgewater_dates$Date[2]), 2,
                                    ifelse(date > as.Date(Teddy_Bridgewater_dates$Date[2]) & date <= as.Date(Teddy_Bridgewater_dates$Date[3]), 3,
                                           ifelse(date > as.Date(Teddy_Bridgewater_dates$Date[3]) & date <= as.Date(Teddy_Bridgewater_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(Teddy_Bridgewater_dates$Date[4]) & date <= as.Date(Teddy_Bridgewater_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(Teddy_Bridgewater_dates$Date[5]) & date <= as.Date(Teddy_Bridgewater_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(Teddy_Bridgewater_dates$Date[6]) & date <= as.Date(Teddy_Bridgewater_dates$Date[7]), 7, "error")))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())

#Combined football and twitter data
Teddy_Bridgewater_data <- Teddy_Bridgewater_tweets %>%
  left_join(Teddy_Bridgewater_logs, by="Week") %>%
  mutate(player = "Teddy_Bridgewater")


#BAKER MAYFIELD

#Load gamelogs data
Baker_Mayfield <- read_excel("gamelogs/Baker_Mayfield.xlsx")

#Wrangle gamelogs data
Baker_Mayfield_logs <- Baker_Mayfield %>%
  mutate(total_yards = pyds + ryds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pyds + 0.1*ryds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n())) %>%
  filter(Week <=7)

#Dates from Game Logs (used in merge with twitter data)
Baker_Mayfield_dates <- Baker_Mayfield_logs %>%
  select(Date)

#Twitter Scrape of Tweets
Baker_Mayfield_tweets <- get_timeline("@bakermayfield", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(Baker_Mayfield_dates$Date[1]), 1, 
                             ifelse(date > as.Date(Baker_Mayfield_dates$Date[1]) & date <= as.Date(Baker_Mayfield_dates$Date[2]), 2,
                                    ifelse(date > as.Date(Baker_Mayfield_dates$Date[2]) & date <= as.Date(Baker_Mayfield_dates$Date[3]), 3,
                                           ifelse(date > as.Date(Baker_Mayfield_dates$Date[3]) & date <= as.Date(Baker_Mayfield_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(Baker_Mayfield_dates$Date[4]) & date <= as.Date(Baker_Mayfield_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(Baker_Mayfield_dates$Date[5]) & date <= as.Date(Baker_Mayfield_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(Baker_Mayfield_dates$Date[6]) & date <= as.Date(Baker_Mayfield_dates$Date[7]), 7, "error")))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())

#Combined football and twitter data
Baker_Mayfield_data <- Baker_Mayfield_tweets %>%
  left_join(Baker_Mayfield_logs, by="Week") %>%
  mutate(player = "Baker_Mayfield")


#GARDNER MINSHEW

#Load gamelogs data
Gardner_Minshew <- read_excel("gamelogs/Gardner_Minshew.xlsx")

#Wrangle gamelogs data
Gardner_Minshew_logs <- Gardner_Minshew %>%
  mutate(total_yards = pyds + ryds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pyds + 0.1*ryds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n())) %>%
  filter(Week <=7)

#Dates from Game Logs (used in merge with twitter data)
Gardner_Minshew_dates <- Gardner_Minshew_logs %>%
  select(Date)

#Twitter Scrape of Tweets
Gardner_Minshew_tweets <- get_timeline("@GardnerMinshew5", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(Gardner_Minshew_dates$Date[1]), 1, 
                             ifelse(date > as.Date(Gardner_Minshew_dates$Date[1]) & date <= as.Date(Gardner_Minshew_dates$Date[2]), 2,
                                    ifelse(date > as.Date(Gardner_Minshew_dates$Date[2]) & date <= as.Date(Gardner_Minshew_dates$Date[3]), 3,
                                           ifelse(date > as.Date(Gardner_Minshew_dates$Date[3]) & date <= as.Date(Gardner_Minshew_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(Gardner_Minshew_dates$Date[4]) & date <= as.Date(Gardner_Minshew_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(Gardner_Minshew_dates$Date[5]) & date <= as.Date(Gardner_Minshew_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(Gardner_Minshew_dates$Date[6]) & date <= as.Date(Gardner_Minshew_dates$Date[7]), 7, "error")))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())

#Combined football and twitter data
Gardner_Minshew_data <- Gardner_Minshew_tweets %>%
  left_join(Gardner_Minshew_logs, by="Week") %>%
  mutate(player = "Gardner_Minshew")


#RUSSELL WILSON

#Load gamelogs data
Russell_Wilson <- read_excel("gamelogs/Russell_Wilson.xlsx")

#Wrangle gamelogs data
Russell_Wilson_logs <- Russell_Wilson %>%
  mutate(total_yards = pyds + ryds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pyds + 0.1*ryds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n())) %>%
  filter(Week <=7)

#Dates from Game Logs (used in merge with twitter data)
Russell_Wilson_dates <- Russell_Wilson_logs %>%
  select(Date)

#Twitter Scrape of Tweets
Russell_Wilson_tweets <- get_timeline("@DangeRussWilson", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(Russell_Wilson_dates$Date[1]), 1, 
                             ifelse(date > as.Date(Russell_Wilson_dates$Date[1]) & date <= as.Date(Russell_Wilson_dates$Date[2]), 2,
                                    ifelse(date > as.Date(Russell_Wilson_dates$Date[2]) & date <= as.Date(Russell_Wilson_dates$Date[3]), 3,
                                           ifelse(date > as.Date(Russell_Wilson_dates$Date[3]) & date <= as.Date(Russell_Wilson_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(Russell_Wilson_dates$Date[4]) & date <= as.Date(Russell_Wilson_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(Russell_Wilson_dates$Date[5]) & date <= as.Date(Russell_Wilson_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(Russell_Wilson_dates$Date[6]) & date <= as.Date(Russell_Wilson_dates$Date[7]), 7, "error")))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())

#Combined football and twitter data
Russell_Wilson_data <- Russell_Wilson_tweets %>%
  left_join(Russell_Wilson_logs, by="Week") %>%
  mutate(player = "Russell_Wilson")


#DREW BREES

#Load gamelogs data
Drew_Brees <- read_excel("gamelogs/Drew_Brees.xlsx")

#Wrangle gamelogs data
Drew_Brees_logs <- Drew_Brees %>%
  mutate(total_yards = pyds + ryds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pyds + 0.1*ryds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n())) %>%
  filter(Week <=7)

#Dates from Game Logs (used in merge with twitter data)
Drew_Brees_dates <- Drew_Brees_logs %>%
  select(Date)

#Twitter Scrape of Tweets
Drew_Brees_tweets <- get_timeline("@drewbrees", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(Drew_Brees_dates$Date[1]), 1, 
                             ifelse(date > as.Date(Drew_Brees_dates$Date[1]) & date <= as.Date(Drew_Brees_dates$Date[2]), 2,
                                    ifelse(date > as.Date(Drew_Brees_dates$Date[2]) & date <= as.Date(Drew_Brees_dates$Date[3]), 3,
                                           ifelse(date > as.Date(Drew_Brees_dates$Date[3]) & date <= as.Date(Drew_Brees_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(Drew_Brees_dates$Date[4]) & date <= as.Date(Drew_Brees_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(Drew_Brees_dates$Date[5]) & date <= as.Date(Drew_Brees_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(Drew_Brees_dates$Date[6]) & date <= as.Date(Drew_Brees_dates$Date[7]), 7, "error")))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())

#Combined football and twitter data
Drew_Brees_data <- Drew_Brees_tweets %>%
  left_join(Drew_Brees_logs, by="Week") %>%
  mutate(player = "Drew_Brees")


#KYLER MURRAY

#Load gamelogs data
Kyler_Murray <- read_excel("gamelogs/Kyler_Murray.xlsx")

#Wrangle gamelogs data
Kyler_Murray_logs <- Kyler_Murray %>%
  mutate(total_yards = pyds + ryds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pyds + 0.1*ryds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n())) %>%
  filter(Week <=7)

#Dates from Game Logs (used in merge with twitter data)
Kyler_Murray_dates <- Kyler_Murray_logs %>%
  select(Date)

#Twitter Scrape of Tweets
Kyler_Murray_tweets <- get_timeline("@K1", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(Kyler_Murray_dates$Date[1]), 1, 
                             ifelse(date > as.Date(Kyler_Murray_dates$Date[1]) & date <= as.Date(Kyler_Murray_dates$Date[2]), 2,
                                    ifelse(date > as.Date(Kyler_Murray_dates$Date[2]) & date <= as.Date(Kyler_Murray_dates$Date[3]), 3,
                                           ifelse(date > as.Date(Kyler_Murray_dates$Date[3]) & date <= as.Date(Kyler_Murray_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(Kyler_Murray_dates$Date[4]) & date <= as.Date(Kyler_Murray_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(Kyler_Murray_dates$Date[5]) & date <= as.Date(Kyler_Murray_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(Kyler_Murray_dates$Date[6]) & date <= as.Date(Kyler_Murray_dates$Date[7]), 7, "error")))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())

#Combined football and twitter data
Kyler_Murray_data <- Kyler_Murray_tweets %>%
  left_join(Kyler_Murray_logs, by="Week") %>%
  mutate(player = "Kyler_Murray")



#COMBINED QB DATA + ANALYSIS


#Overall Week Points for All QBs

#QB Data points combined
qb_overall_data <- Kyler_Murray_data %>%
  full_join(Drew_Brees_data) %>%
  full_join(Russell_Wilson_data) %>%
  full_join(Gardner_Minshew_data) %>%
  full_join(Baker_Mayfield_data) %>%
  full_join(Teddy_Bridgewater_data) %>%
  full_join(Daniel_Jones_data) %>%
  full_join(Ben_Roethlisberger_data) %>%
  select(tweetcount, fpts, Rate, total_yards, total_tds)

#scatterplot of all relevant weeks for all QBs: tweets vs. fantasy points
ggplot(qb_overall_data, aes(
  x=tweetcount,
  y=fpts
)) + geom_point() 

#Correlation matrix of those points
cor(qb_overall_data)


#QB Tweet Comparison

#QB Total Tweets with names
qb_overall_tweets_byname <- Kyler_Murray_data %>%
  full_join(Drew_Brees_data) %>%
  full_join(Russell_Wilson_data) %>%
  full_join(Gardner_Minshew_data) %>%
  full_join(Baker_Mayfield_data) %>%
  full_join(Teddy_Bridgewater_data) %>%
  full_join(Daniel_Jones_data) %>%
  full_join(Ben_Roethlisberger_data) %>%
  group_by(player) %>%
  summarize(count = sum(tweetcount)) %>%
  arrange(desc(count))

#QB Tweets chart 
ggplot(data=qb_overall_tweets_byname, aes(
  x=player, y=count
)) + geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



  
