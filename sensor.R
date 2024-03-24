library(tidyverse)
library(scales)
library(lubridate)
setwd("D:/Users/EE43446/OneDrive - The James Hutton Institute/Rstudio")

sensor1 <- read.csv("sensor.csv", header=TRUE)
sensor2 <- read.csv("sensor2.csv", header=TRUE)
sensor <- rbind(sensor1, sensor2)
names(sensor) <- c('date', 'temp',	'humidity',	'light',	'volt',	'wifi',	'vibration',	'knocks',	'ext.temp')
sensor$date <- ymd_hms(sensor$date)

sensor %>%
  select(date, humidity) %>% 
  filter(!is.na(humidity)) %>% 
  ggplot(aes(x=date, y=humidity)) + geom_line()

sensor %>%
  select(date, temp) %>% 
  filter(!is.na(temp)) %>% 
  ggplot(aes(x=date, y=temp)) + geom_line()