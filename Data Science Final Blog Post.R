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
  mutate(player = "Ben Roethlisberger")


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
  mutate(player = "Daniel Jones")

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
  mutate(player = "Teddy Bridgewater")


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
  mutate(player = "Baker Mayfield")


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
  mutate(player = "Gardner Minshew")


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
  mutate(player = "Russell Wilson")


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
  mutate(player = "Drew Brees")


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
  mutate(player = "Kyler Murray")


#JOE BURROW

#Load gamelogs data
joe_burrow <- read_excel("gamelogs/joe_burrow.xls")
#Wrangle gamelogs data
joe_burrow_logs <- joe_burrow %>%
  mutate(total_yards = PYds + RYds) %>%
  mutate(total_tds = PTD + RTD) %>%
  mutate(fpts = 4*PTD + 6*RTD + 0.025*PYds + 0.1*RYds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n()))
#Dates from Game Logs (used in merge with twitter data)
joe_burrow_dates <- joe_burrow_logs %>%
  select(Date)
#Twitter Scrape of Tweets
joe_burrow_tweets <- get_timeline("@joeyb", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(joe_burrow_dates$Date[1]), 1, 
                             ifelse(date > as.Date(joe_burrow_dates$Date[1]) & 
                                      date <= as.Date(joe_burrow_dates$Date[2]), 2,
                                    ifelse(date > as.Date(joe_burrow_dates$Date[2]) & 
                                             date <= as.Date(joe_burrow_dates$Date[3]), 3,
                                           ifelse(date > as.Date(joe_burrow_dates$Date[3]) & 
                                                    date <= as.Date(joe_burrow_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(joe_burrow_dates$Date[4]) & 
                                                           date <= as.Date(joe_burrow_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(joe_burrow_dates$Date[5]) & 
                                                                  date <= as.Date(joe_burrow_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(joe_burrow_dates$Date[6]) & 
                                                                         date <= as.Date(joe_burrow_dates$Date[7]), 7,
                                                                       ifelse(date > as.Date(joe_burrow_dates$Date[7]) & 
                                                                                date <= as.Date(joe_burrow_dates$Date[8]), 8, "error")
                                                                )))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())
#Combined football and twitter data
joe_burrow_data <- joe_burrow_tweets %>%
  left_join(joe_burrow_logs, by="Week") %>%
  mutate(player = "Joe Burrow")


# MATT RYAN


#Load gamelogs data
matt_ryan <- read_excel("gamelogs/matt_ryan.xls")
#Wrangle gamelogs data
matt_ryan_logs <- matt_ryan %>%
  mutate(total_yards = PYds + RYds) %>%
  mutate(total_tds = PTD + RTD) %>%
  mutate(fpts = 4*PTD + 6*RTD + 0.025*PYds + 0.1*RYds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n()))
#Dates from Game Logs (used in merge with twitter data)
matt_ryan_dates <- matt_ryan_logs %>%
  select(Date)
#Twitter Scrape of Tweets
matt_ryan_tweets <- get_timeline("m_ryan02", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(matt_ryan_dates$Date[1]), 1, 
                             ifelse(date > as.Date(matt_ryan_dates$Date[1]) & 
                                      date <= as.Date(matt_ryan_dates$Date[2]), 2,
                                    ifelse(date > as.Date(matt_ryan_dates$Date[2]) & 
                                             date <= as.Date(matt_ryan_dates$Date[3]), 3,
                                           ifelse(date > as.Date(matt_ryan_dates$Date[3]) & 
                                                    date <= as.Date(matt_ryan_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(matt_ryan_dates$Date[4]) & 
                                                           date <= as.Date(matt_ryan_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(matt_ryan_dates$Date[5]) & 
                                                                  date <= as.Date(matt_ryan_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(matt_ryan_dates$Date[6]) & 
                                                                         date <= as.Date(matt_ryan_dates$Date[7]), 7,
                                                                       ifelse(date > as.Date(matt_ryan_dates$Date[7]) & 
                                                                                date <= as.Date(matt_ryan_dates$Date[8]), 8, "error")
                                                                )))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())
#Combined football and twitter data
matt_ryan_data <- matt_ryan_tweets %>%
  left_join(matt_ryan_logs, by="Week") %>%
  mutate(player = "Matt Ryan")



# Tom Brady


#Load gamelogs data
tom_brady <- read_excel("gamelogs/tom_brady.xls")
#Wrangle gamelogs data
tom_brady_logs <- tom_brady %>%
  mutate(total_yards = PYds + RYds) %>%
  mutate(total_tds = PTD + RTD) %>%
  mutate(fpts = 4*PTD + 6*RTD + 0.025*PYds + 0.1*RYds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n()))
#Dates from Game Logs (used in merge with twitter data)
tom_brady_dates <- tom_brady_logs %>%
  select(Date)
#Twitter Scrape of Tweets
tom_brady_tweets <- get_timeline("tombrady", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(tom_brady_dates$Date[1]), 1, 
                             ifelse(date > as.Date(tom_brady_dates$Date[1]) & 
                                      date <= as.Date(tom_brady_dates$Date[2]), 2,
                                    ifelse(date > as.Date(tom_brady_dates$Date[2]) & 
                                             date <= as.Date(tom_brady_dates$Date[3]), 3,
                                           ifelse(date > as.Date(tom_brady_dates$Date[3]) & 
                                                    date <= as.Date(tom_brady_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(tom_brady_dates$Date[4]) & 
                                                           date <= as.Date(tom_brady_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(tom_brady_dates$Date[5]) & 
                                                                  date <= as.Date(tom_brady_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(tom_brady_dates$Date[6]) & 
                                                                         date <= as.Date(tom_brady_dates$Date[7]), 7,
                                                                       ifelse(date > as.Date(tom_brady_dates$Date[7]) & 
                                                                                date <= as.Date(tom_brady_dates$Date[8]), 8, "error")
                                                                )))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())
#Combined football and twitter data
tom_brady_data <- tom_brady_tweets %>%
  left_join(tom_brady_logs, by="Week") %>%
  mutate(player = "Tom Brady")



# Carson Wentz


#Load gamelogs data
carson_wentz <- read_excel("gamelogs/carson_wentz.xls")
#Wrangle gamelogs data
carson_wentz_logs <- carson_wentz %>%
  mutate(total_yards = PYds + RYds) %>%
  mutate(total_tds = PTD + RTD) %>%
  mutate(fpts = 4*PTD + 6*RTD + 0.025*PYds + 0.1*RYds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n()))
#Dates from Game Logs (used in merge with twitter data)
carson_wentz_dates <- carson_wentz_logs %>%
  select(Date)
#Twitter Scrape of Tweets
carson_wentz_tweets <- get_timeline("@cj_wentz", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(carson_wentz_dates$Date[1]), 1, 
                             ifelse(date > as.Date(carson_wentz_dates$Date[1]) & 
                                      date <= as.Date(carson_wentz_dates$Date[2]), 2,
                                    ifelse(date > as.Date(carson_wentz_dates$Date[2]) & 
                                             date <= as.Date(carson_wentz_dates$Date[3]), 3,
                                           ifelse(date > as.Date(carson_wentz_dates$Date[3]) & 
                                                    date <= as.Date(carson_wentz_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(carson_wentz_dates$Date[4]) & 
                                                           date <= as.Date(carson_wentz_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(carson_wentz_dates$Date[5]) & 
                                                                  date <= as.Date(carson_wentz_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(carson_wentz_dates$Date[6]) & 
                                                                         date <= as.Date(carson_wentz_dates$Date[7]), 7,
                                                                       ifelse(date > as.Date(carson_wentz_dates$Date[7]) & 
                                                                                date <= as.Date(carson_wentz_dates$Date[8]), 8, "error")
                                                                )))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())
#Combined football and twitter data
carson_wentz_data <- carson_wentz_tweets %>%
  left_join(carson_wentz_logs, by="Week") %>%
  mutate(player = "Carson Wentz")




# Jared Goff


#Load gamelogs data
jared_goff <- read_excel("gamelogs/jared_goff.xls")
#Wrangle gamelogs data
jared_goff_logs <- jared_goff %>%
  mutate(total_yards = PYds + RYds) %>%
  mutate(total_tds = PTD + RTD) %>%
  mutate(fpts = 4*PTD + 6*RTD + 0.025*PYds + 0.1*RYds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts)%>%
  mutate(Week = as.numeric(1:n()))
#Dates from Game Logs (used in merge with twitter data)
jared_goff_dates <- jared_goff_logs %>%
  select(Date)
#Twitter Scrape of Tweets
jared_goff_tweets <- get_timeline("@jaredgoff16", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(jared_goff_dates$Date[1]), 1, 
                             ifelse(date > as.Date(jared_goff_dates$Date[1]) & 
                                      date <= as.Date(jared_goff_dates$Date[2]), 2,
                                    ifelse(date > as.Date(jared_goff_dates$Date[2]) & 
                                             date <= as.Date(jared_goff_dates$Date[3]), 3,
                                           ifelse(date > as.Date(jared_goff_dates$Date[3]) & 
                                                    date <= as.Date(jared_goff_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(jared_goff_dates$Date[4]) & 
                                                           date <= as.Date(jared_goff_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(jared_goff_dates$Date[5]) & 
                                                                  date <= as.Date(jared_goff_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(jared_goff_dates$Date[6]) & 
                                                                         date <= as.Date(jared_goff_dates$Date[7]), 7,
                                                                       ifelse(date > as.Date(jared_goff_dates$Date[7]) & 
                                                                                date <= as.Date(jared_goff_dates$Date[8]), 8, "error")
                                                                )))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())
#Combined football and twitter data
jared_goff_data <- jared_goff_tweets %>%
  left_join(jared_goff_logs, by="Week") %>%
  mutate(player = "Jared Goff")




# Patrick Mahomes


#Load gamelogs data
patrick_mahomes <- read_excel("gamelogs/patrick_mahomes.xls")
#Wrangle gamelogs data
patrick_mahomes_logs <- patrick_mahomes %>%
  mutate(total_yards = PYds + RYds) %>%
  mutate(total_tds = PTD + RTD) %>%
  mutate(fpts = 4*PTD + 6*RTD + 0.025*PYds + 0.1*RYds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n()))
#Dates from Game Logs (used in merge with twitter data)
patrick_mahomes_dates <- patrick_mahomes_logs %>%
  select(Date)
#Twitter Scrape of Tweets
patrick_mahomes_tweets <- get_timeline("@patrickmahomes", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(patrick_mahomes_dates$Date[1]), 1, 
                             ifelse(date > as.Date(patrick_mahomes_dates$Date[1]) & 
                                      date <= as.Date(patrick_mahomes_dates$Date[2]), 2,
                                    ifelse(date > as.Date(patrick_mahomes_dates$Date[2]) & 
                                             date <= as.Date(patrick_mahomes_dates$Date[3]), 3,
                                           ifelse(date > as.Date(patrick_mahomes_dates$Date[3]) & 
                                                    date <= as.Date(patrick_mahomes_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(patrick_mahomes_dates$Date[4]) & 
                                                           date <= as.Date(patrick_mahomes_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(patrick_mahomes_dates$Date[5]) & 
                                                                  date <= as.Date(patrick_mahomes_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(patrick_mahomes_dates$Date[6]) & 
                                                                         date <= as.Date(patrick_mahomes_dates$Date[7]), 7,
                                                                       ifelse(date > as.Date(patrick_mahomes_dates$Date[7]) & 
                                                                                date <= as.Date(patrick_mahomes_dates$Date[8]), 8, "error")
                                                                )))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())
#Combined football and twitter data
patrick_mahomes_data <- patrick_mahomes_tweets %>%
  left_join(patrick_mahomes_logs, by="Week") %>%
  mutate(player = "Patrick Mahomes")



# Aaron Rodgers

#Load gamelogs data
aaron_rodgers <- read_excel("gamelogs/aaron_rodgers.xls")
#Wrangle gamelogs data
aaron_rodgers_logs <- aaron_rodgers %>%
  mutate(total_yards = PYds + RYds) %>%
  mutate(total_tds = PTD + RTD) %>%
  mutate(fpts = 4*PTD + 6*RTD + 0.025*PYds + 0.1*RYds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n()))
#Dates from Game Logs (used in merge with twitter data)
aaron_rodgers_dates <- aaron_rodgers_logs %>%
  select(Date)
#Twitter Scrape of Tweets
aaron_rodgers_tweets <- get_timeline("@aaronrodgers12", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(aaron_rodgers_dates$Date[1]), 1, 
                             ifelse(date > as.Date(aaron_rodgers_dates$Date[1]) & 
                                      date <= as.Date(aaron_rodgers_dates$Date[2]), 2,
                                    ifelse(date > as.Date(aaron_rodgers_dates$Date[2]) & 
                                             date <= as.Date(aaron_rodgers_dates$Date[3]), 3,
                                           ifelse(date > as.Date(aaron_rodgers_dates$Date[3]) & 
                                                    date <= as.Date(aaron_rodgers_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(aaron_rodgers_dates$Date[4]) & 
                                                           date <= as.Date(aaron_rodgers_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(aaron_rodgers_dates$Date[5]) & 
                                                                  date <= as.Date(aaron_rodgers_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(aaron_rodgers_dates$Date[6]) & 
                                                                         date <= as.Date(aaron_rodgers_dates$Date[7]), 7,
                                                                       ifelse(date > as.Date(aaron_rodgers_dates$Date[7]) & 
                                                                                date <= as.Date(aaron_rodgers_dates$Date[8]), 8, "error")
                                                                )))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())
#Combined football and twitter data
aaron_rodgers_data <- aaron_rodgers_tweets %>%
  left_join(aaron_rodgers_logs, by="Week") %>%
  mutate(player = "Aaron Rodgers")



# Josh Allen


#Load gamelogs data
josh_allen <- read_excel("gamelogs/josh_allen.xls")
#Wrangle gamelogs data
josh_allen_logs <- josh_allen %>%
  mutate(total_yards = PYds + RYds) %>%
  mutate(total_tds = PTD + RTD) %>%
  mutate(fpts = 4*PTD + 6*RTD + 0.025*PYds + 0.1*RYds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n()))
#Dates from Game Logs (used in merge with twitter data)
josh_allen_dates <- josh_allen_logs %>%
  select(Date)
#Twitter Scrape of Tweets
josh_allen_tweets <- get_timeline("@joshallenqb", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(josh_allen_dates$Date[1]), 1, 
                             ifelse(date > as.Date(josh_allen_dates$Date[1]) & 
                                      date <= as.Date(josh_allen_dates$Date[2]), 2,
                                    ifelse(date > as.Date(josh_allen_dates$Date[2]) & 
                                             date <= as.Date(josh_allen_dates$Date[3]), 3,
                                           ifelse(date > as.Date(josh_allen_dates$Date[3]) & 
                                                    date <= as.Date(josh_allen_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(josh_allen_dates$Date[4]) & 
                                                           date <= as.Date(josh_allen_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(josh_allen_dates$Date[5]) & 
                                                                  date <= as.Date(josh_allen_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(josh_allen_dates$Date[6]) & 
                                                                         date <= as.Date(josh_allen_dates$Date[7]), 7,
                                                                       ifelse(date > as.Date(josh_allen_dates$Date[7]) & 
                                                                                date <= as.Date(josh_allen_dates$Date[8]), 8, "error")
                                                                )))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())
#Combined football and twitter data
josh_allen_data <- josh_allen_tweets %>%
  left_join(josh_allen_logs, by="Week") %>%
  mutate(player = "Josh Allen")


#DEREK CARR
derek_carr <- read_excel("gamelogs/derek_carr.xlsx")    
#Wrangle gamelogs data
derek_carr_logs <- derek_carr %>%
  mutate(total_yards = pYds + rYds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pYds + 0.1*rYds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n()))
#Dates from Game Logs (used in merge with twitter data)
derek_carr_dates <- derek_carr_logs %>%
  select(Date)
#Twitter Scrape of Tweets
derek_carr_tweets <- get_timeline("@derekcarrqb", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(derek_carr_dates$Date[1]), 1, 
                             ifelse(date > as.Date(derek_carr_dates$Date[1]) & date <= as.Date(derek_carr_dates$Date[2]), 2,
                                    ifelse(date > as.Date(derek_carr_dates$Date[2]) & date <= as.Date(derek_carr_dates$Date[3]), 3,
                                           ifelse(date > as.Date(derek_carr_dates$Date[3]) & date <= as.Date(derek_carr_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(derek_carr_dates$Date[4]) & date <= as.Date(derek_carr_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(derek_carr_dates$Date[5]) & date <= as.Date(derek_carr_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(derek_carr_dates$Date[6]) & date <= as.Date(derek_carr_dates$Date[7]), 7, "error")))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n()) 
#Combined football and twitter data
derek_carr_data <- derek_carr_tweets %>%
  left_join(derek_carr_logs, by="Week") %>%
  mutate(player="Derek Carr")




#DESHAUN WATSON (NOT AUTHORIZED ERROR, PROTECTED TWEETS)
deshaun_watson <- read_excel("gamelogs/deshaun_watson.xlsx")
#Wrangle gamelogs data
deshaun_watson_logs <- deshaun_watson %>%
  mutate(total_yards = pYds + rYds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pYds + 0.1*rYds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n()))
#Dates from Game Logs (used in merge with twitter data)
derek_carr_dates <- deshaun_watson_logs %>%
  select(Date)
#Twitter Scrape of Tweets
deshaun_watson_tweets <- get_timeline("@deshaunwatson", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(deshaun_watson_dates$Date[1]), 1, 
                             ifelse(date > as.Date(deshaun_watson_dates$Date[1]) & date <= as.Date(deshaun_watson_dates$Date[2]), 2,
                                    ifelse(date > as.Date(deshaun_watson_dates$Date[2]) & date <= as.Date(deshaun_watson_dates$Date[3]), 3,
                                           ifelse(date > as.Date(deshaun_watson_dates$Date[3]) & date <= as.Date(deshaun_watson_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(deshaun_watson_dates$Date[4]) & date <= as.Date(deshaun_watson_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(deshaun_watson_dates$Date[5]) & date <= as.Date(deshaun_watson_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(deshaun_watson_dates$Date[6]) & date <= as.Date(deshaun_watson_dates$Date[7]), 7, "error")))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())
#Combined football and twitter data
deshaun_watson_data <- deshaun_watson_tweets %>%
  left_join(deshaun_watson_logs, by="Week") %>%
  mutate(player="Deshaun Watson")



#KIRK COUSINS
kirk_cousins <- read_excel("gamelogs/kirk_cousins.xlsx")    
#Wrangle gamelogs data
kirk_cousins_logs <- kirk_cousins %>%
  mutate(total_yards = pYds + rYds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pYds + 0.1*rYds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n()))
#Dates from Game Logs (used in merge with twitter data)
kirk_cousins_dates <- kirk_cousins_logs %>%
  select(Date)
#Twitter Scrape of Tweets
kirk_cousins_tweets <- get_timeline("@KirkCousins8", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(kirk_cousins_dates$Date[1]), 1, 
                             ifelse(date > as.Date(kirk_cousins_dates$Date[1]) & date <= as.Date(kirk_cousins_dates$Date[2]), 2,
                                    ifelse(date > as.Date(kirk_cousins_dates$Date[2]) & date <= as.Date(kirk_cousins_dates$Date[3]), 3,
                                           ifelse(date > as.Date(kirk_cousins_dates$Date[3]) & date <= as.Date(kirk_cousins_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(kirk_cousins_dates$Date[4]) & date <= as.Date(kirk_cousins_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(kirk_cousins_dates$Date[5]) & date <= as.Date(kirk_cousins_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(kirk_cousins_dates$Date[6]) & date <= as.Date(kirk_cousins_dates$Date[7]), 7, "error")))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())
#Combined football and twitter data
kirk_cousins_data <- kirk_cousins_tweets %>%
  left_join(kirk_cousins_logs, by="Week") %>%
  mutate(player="Kirk Cousins")


#LAMAR JACKSON
lamar_jackson <- read_excel("gamelogs/lamar_jackson.xlsx") 

#Wrangle gamelogs data
lamar_jackson_logs <- lamar_jackson %>%
  mutate(total_yards = pYds + rYds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pYds + 0.1*rYds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n()))
#Dates from Game Logs (used in merge with twitter data)
lamar_jackson_dates <- lamar_jackson_logs %>%
  select(Date)
#Twitter Scrape of Tweets
lamar_jackson_tweets <- get_timeline("@Lj_era8", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(lamar_jackson_dates$Date[1]), 1, 
                             ifelse(date > as.Date(lamar_jackson_dates$Date[1]) & date <= as.Date(lamar_jackson_dates$Date[2]), 2,
                                    ifelse(date > as.Date(lamar_jackson_dates$Date[2]) & date <= as.Date(lamar_jackson_dates$Date[3]), 3,
                                           ifelse(date > as.Date(lamar_jackson_dates$Date[3]) & date <= as.Date(lamar_jackson_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(lamar_jackson_dates$Date[4]) & date <= as.Date(lamar_jackson_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(lamar_jackson_dates$Date[5]) & date <= as.Date(lamar_jackson_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(lamar_jackson_dates$Date[6]) & date <= as.Date(lamar_jackson_dates$Date[7]), 7, "error")))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())
#Combined football and twitter data
lamar_jackson_data <- lamar_jackson_tweets %>%
  left_join(lamar_jackson_logs, by="Week") %>%
  mutate(player="Lamar Jackson")



#MATTHEW STAFFORD (HAS NOT TWEETED SINCE 2011)
matthew_stafford <- read_excel("gamelogs/matthew_stafford.xlsx")    
#Wrangle gamelogs data
matthew_stafford_logs <- matthew_stafford %>%
  mutate(total_yards = pYds + rYds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pYds + 0.1*rYds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n()))
#Dates from Game Logs (used in merge with twitter data)
matthew_stafford_dates <- matthew_stafford_logs %>%
  select(Date)
#Twitter Scrape of Tweets
matthew_stafford_tweets <- get_timeline("@matt_stafford9", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(matthew_stafford_dates$Date[1]), 1, 
                             ifelse(date > as.Date(matthew_stafford_dates$Date[1]) & date <= as.Date(matthew_stafford_dates$Date[2]), 2,
                                    ifelse(date > as.Date(matthew_stafford_dates$Date[2]) & date <= as.Date(matthew_stafford_dates$Date[3]), 3,
                                           ifelse(date > as.Date(matthew_stafford_dates$Date[3]) & date <= as.Date(matthew_stafford_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(matthew_stafford_dates$Date[4]) & date <= as.Date(matthew_stafford_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(matthew_stafford_dates$Date[5]) & date <= as.Date(matthew_stafford_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(matthew_stafford_dates$Date[6]) & date <= as.Date(matthew_stafford_dates$Date[7]), 7, "error")))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())
#Combined football and twitter data
matthew_stafford_data <- matthew_stafford_tweets %>%
  left_join(matthew_stafford_logs, by="Week") %>%
  mutate(player="Matthew Stafford")




#PHILIP RIVERS
philip_rivers <- read_excel("gamelogs/philip_rivers.xlsx")    
#Wrangle gamelogs data
philip_rivers_logs <- philip_rivers %>%
  mutate(total_yards = pYds + rYds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pYds + 0.1*rYds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n()))
#Dates from Game Logs (used in merge with twitter data)
philip_rivers_dates <- philip_rivers_logs %>%
  select(Date)
#Twitter Scrape of Tweets
philip_rivers_tweets <- get_timeline("@FauxPhilipRiver", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(philip_rivers_dates$Date[1]), 1, 
                             ifelse(date > as.Date(philip_rivers_dates$Date[1]) & date <= as.Date(philip_rivers_dates$Date[2]), 2,
                                    ifelse(date > as.Date(philip_rivers_dates$Date[2]) & date <= as.Date(philip_rivers_dates$Date[3]), 3,
                                           ifelse(date > as.Date(philip_rivers_dates$Date[3]) & date <= as.Date(philip_rivers_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(philip_rivers_dates$Date[4]) & date <= as.Date(philip_rivers_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(philip_rivers_dates$Date[5]) & date <= as.Date(philip_rivers_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(philip_rivers_dates$Date[6]) & date <= as.Date(philip_rivers_dates$Date[7]), 7, "error")))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())
#Combined football and twitter data
philip_rivers_data <- philip_rivers_tweets %>%
  left_join(philip_rivers_logs, by="Week") %>%
  mutate(player="Philip Rivers")


#RYAN TANNEHILL 
ryan_tannehill <- read_excel("gamelogs/ryan_tannehill.xlsx")    
#Wrangle gamelogs data
ryan_tannehill_logs <- ryan_tannehill %>%
  mutate(total_yards = pYds + rYds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pYds + 0.1*rYds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:n()))
#Dates from Game Logs (used in merge with twitter data)
ryan_tannehill_dates <- ryan_tannehill_logs %>%
  select(Date)
#Twitter Scrape of Tweets
ryan_tannehill_tweets <- get_timeline("@ryantannehill1", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(ryan_tannehill_dates$Date[1]), 1, 
                             ifelse(date > as.Date(ryan_tannehill_dates$Date[1]) & date <= as.Date(ryan_tannehill_dates$Date[2]), 2,
                                    ifelse(date > as.Date(ryan_tannehill_dates$Date[2]) & date <= as.Date(ryan_tannehill_dates$Date[3]), 3,
                                           ifelse(date > as.Date(ryan_tannehill_dates$Date[3]) & date <= as.Date(ryan_tannehill_dates$Date[4]), 4,
                                                  ifelse(date > as.Date(ryan_tannehill_dates$Date[4]) & date <= as.Date(ryan_tannehill_dates$Date[5]), 5,
                                                         ifelse(date > as.Date(ryan_tannehill_dates$Date[5]) & date <= as.Date(ryan_tannehill_dates$Date[6]), 6,
                                                                ifelse(date > as.Date(ryan_tannehill_dates$Date[6]) & date <= as.Date(ryan_tannehill_dates$Date[7]), 7, "error")))))))
                      
    )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())
#Combined football and twitter data
ryan_tannehill_data <- ryan_tannehill_tweets %>%
  left_join(ryan_tannehill_logs, by="Week") %>%
  mutate(player="Ryan Tannehill")





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
  full_join(joe_burrow_data) %>%
  full_join(matt_ryan_data) %>%
  full_join(tom_brady_data) %>%
  full_join(carson_wentz_data) %>%
  full_join(jared_goff_data) %>%
  full_join(patrick_mahomes_data) %>%
  full_join(aaron_rodgers_data) %>%
  full_join(josh_allen_data) %>%
  full_join(derek_carr_data) %>%
  full_join(matthew_stafford_data) %>%
  full_join(philip_rivers_data) %>%
  full_join(lamar_jackson_data) %>%
  full_join(ryan_tannehill_data) %>%
  full_join(kirk_cousins_data) %>%
  select(tweetcount, fpts, Rate, total_yards, total_tds)

#scatterplot of all relevant weeks for all QBs: tweets vs. fantasy points
ggplot(qb_overall_data, aes(
  x=log(tweetcount),
  y=fpts
)) + geom_point()  

#Correlation matrix of those points
cor(qb_overall_data)

#Individual QB Correlation Coefficients + Plot

roethlisberger_cor_data <- Ben_Roethlisberger_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
roethlisberger_cor<- cor(roethlisberger_cor_data$fpts, roethlisberger_cor_data$tweetcount)

bridgewater_cor_data <- Teddy_Bridgewater_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
bridgewater_cor<- cor(bridgewater_cor_data$fpts, bridgewater_cor_data$tweetcount)

mayfield_cor_data <- Baker_Mayfield_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
mayfield_cor<- cor(mayfield_cor_data$fpts, mayfield_cor_data$tweetcount)

minshew_cor_data <- Gardner_Minshew_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
minshew_cor<- cor(minshew_cor_data$fpts, minshew_cor_data$tweetcount)

brees_cor_data <- Drew_Brees_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
brees_cor<- cor(brees_cor_data$fpts, brees_cor_data$tweetcount)

wilson_cor_data <- Russell_Wilson_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
wilson_cor<- cor(wilson_cor_data$fpts, wilson_cor_data$tweetcount)

murray_cor_data <- Kyler_Murray_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
murray_cor<- cor(murray_cor_data$fpts, murray_cor_data$tweetcount)

burrow_cor_data <- joe_burrow_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
burrow_cor<- cor(burrow_cor_data$fpts, burrow_cor_data$tweetcount)

brady_cor_data <- tom_brady_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
brady_cor<- cor(brady_cor_data$fpts, brady_cor_data$tweetcount)

ryan_cor_data <- matt_ryan_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
ryan_cor<- cor(ryan_cor_data$fpts, ryan_cor_data$tweetcount)

wentz_cor_data <- carson_wentz_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
wentz_cor<- cor(wentz_cor_data$fpts, wentz_cor_data$tweetcount)

goff_cor_data <- jared_goff_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
goff_cor<- cor(goff_cor_data$fpts, goff_cor_data$tweetcount)

mahomes_cor_data <- patrick_mahomes_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
mahomes_cor<- cor(mahomes_cor_data$fpts, mahomes_cor_data$tweetcount)

rodgers_cor_data <- aaron_rodgers_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
rodgers_cor<- cor(rodgers_cor_data$fpts, rodgers_cor_data$tweetcount)

allen_cor_data <- josh_allen_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
allen_cor<- cor(allen_cor_data$fpts, allen_cor_data$tweetcount)

carr_cor_data <- derek_carr_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
carr_cor<- cor(carr_cor_data$fpts, carr_cor_data$tweetcount)

stafford_cor_data <- matthew_stafford_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
stafford_cor<- cor(stafford_cor_data$fpts, stafford_cor_data$tweetcount)

rivers_cor_data <- philip_rivers_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
rivers_cor<- cor(rivers_cor_data$fpts, rivers_cor_data$tweetcount)

jackson_cor_data <- lamar_jackson_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
jackson_cor<- cor(jackson_cor_data$fpts, jackson_cor_data$tweetcount)

tannehill_cor_data <- ryan_tannehill_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
tannehill_cor<- cor(tannehill_cor_data$fpts, tannehill_cor_data$tweetcount)

cousins_cor_data <- kirk_cousins_data %>%
  select(fpts, Rate, total_tds, total_yards, tweetcount)
cousins_cor<- cor(cousins_cor_data$fpts, cousins_cor_data$tweetcount)

names <- c("Ben Roethlisberger", "Teddy Bridgewater", "Baker Mayfield", "Russell Wilson", "Drew Brees", "Kyler Murray", 
            "Matt Ryan", "Tom Brady", "Carson Wentz", "Patrick Mahomes", "Aaron Rodgers", "Josh Allen", 
           "Derek Carr", "Lamar Jackson", "Kirk Cousins", "Ryan Tannehill", "Philip Rivers")

cors <- c(roethlisberger_cor, bridgewater_cor, mayfield_cor, wilson_cor, brees_cor, murray_cor, 
          ryan_cor, brady_cor, wentz_cor, mahomes_cor, rodgers_cor, allen_cor,
          carr_cor, jackson_cor, cousins_cor, tannehill_cor, rivers_cor)

QBtweetsfptscors <- data.frame(names, cors) %>%
  mutate(names = as.factor(names)) %>%
  arrange(cors)

ggplot(data=QBtweetsfptscors, aes(
  x=names, y=cors) 
) + geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

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
  full_join(joe_burrow_data) %>%
  full_join(matt_ryan_data) %>%
  full_join(tom_brady_data) %>%
  full_join(carson_wentz_data) %>%
  full_join(jared_goff_data) %>%
  full_join(patrick_mahomes_data) %>%
  full_join(aaron_rodgers_data) %>%
  full_join(josh_allen_data) %>%
  full_join(derek_carr_data) %>%
  full_join(matthew_stafford_data) %>%
  full_join(philip_rivers_data) %>%
  full_join(lamar_jackson_data) %>%
  full_join(ryan_tannehill_data) %>%
  full_join(kirk_cousins_data) %>%
  group_by(player) %>%
  summarize(count = sum(tweetcount)) %>%
  arrange(desc(count))

#QB Tweets chart 
ggplot(data=qb_overall_tweets_byname, aes(
  x=player, y=count
)) + geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))






#NOTES

#Deshaun Watson (Private Account) and Daniel Jones (no tweets) were omitted from overall analysis.
#Gardner Minshew, Jared Goff, Joe Burrow, and Matthew Stafford were omitted from individual correlation analysis, as they lacked the tweet sample sizes to produce coefficients.

