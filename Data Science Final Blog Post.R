library(readr)
library(tidyverse)
library(robotstxt)
library(rvest)
library(knitr)
library(janitor)
library(shiny)
library(rtweet)
library(readxl)

#BEN ROETHLISBERGER

#Load gamelogs data
ben_roethlisberger <- read_excel("gamelogs/ben_roethlisberger.xlsx")

#Wrangle gamelogs data
ben_roesthlisberger_logs <- ben_roethlisberger %>%
  mutate(total_yards = pyds + ryds) %>%
  mutate(total_tds = pTD + rTD) %>%
  mutate(fpts = 4*pTD + 6*rTD + 0.025*pyds + 0.1*ryds - 2*Int - 2*Fmb) %>%
  select(Date, Week, Rate, total_yards, total_tds, fpts) %>%
  mutate(Week = as.numeric(1:nrow(ben_roesthlisberger_logs)))

#Dates from Game Logs (used in merge with twitter data)
ben_roesthlisberger_dates <- ben_roesthlisberger_logs %>%
  select(Date)

#Twitter Scrape of Tweets
ben_roesthlisberger_tweets <- get_timeline("@_BigBen7", n=1000) %>%
  mutate(date = as.Date(created_at)) %>% 
  filter(date > as.Date("2020-09-01")) %>%
  select(created_at, date, text) %>% 
  #Assigning week number to player (in this case, 7 weeks) NOTE: Instead of dealing with bye weeks, week means team game (1-16), not NFL week (1-17)
  mutate(
    Week = as.numeric(ifelse(date < as.Date(ben_roesthlisberger_dates$Date[1]), 1, 
              ifelse(date > as.Date(ben_roesthlisberger_dates$Date[1]) & date <= as.Date(ben_roesthlisberger_dates$Date[2]), 2,
                     ifelse(date > as.Date(ben_roesthlisberger_dates$Date[2]) & date <= as.Date(ben_roesthlisberger_dates$Date[3]), 3,
                            ifelse(date > as.Date(ben_roesthlisberger_dates$Date[3]) & date <= as.Date(ben_roesthlisberger_dates$Date[4]), 4,
                                   ifelse(date > as.Date(ben_roesthlisberger_dates$Date[4]) & date <= as.Date(ben_roesthlisberger_dates$Date[5]), 5,
                                          ifelse(date > as.Date(ben_roesthlisberger_dates$Date[5]) & date <= as.Date(ben_roesthlisberger_dates$Date[6]), 6,
                                                 ifelse(date > as.Date(ben_roesthlisberger_dates$Date[6]) & date <= as.Date(ben_roesthlisberger_dates$Date[7]), 7, "error")))))))
                                        
  )) %>%
  filter(Week !="error") %>%
  group_by(Week) %>%
  summarize(tweetcount = n())

#Combined football and twitter data
ben_roethlisberger_data <- ben_roesthlisberger_tweets %>%
  left_join(ben_roesthlisberger_logs, by="Week")

#Data used for correlation matrix (remove non-integer variables)
data_for_cor <- ben_roethlisberger_data %>%
  select(fpts, total_yards, total_tds, Rate, tweetcount)

#Data for correlation matrix
cor(data_for_cor)


