rm(list = ls())
library(tidyverse)
theme_set(theme_light())

# Data ----
# getwd()
# setwd("~/Desktop/KirstensWork/Progress reports/FinalReport")
NARWOcc <- read.csv("dataRaw/WhaleOccurrence2014_2024_070825.csv", header = TRUE)
NARWOcc$Year <- as.factor(NARWOcc$Year)
NARWOcc$Year <- relevel(NARWOcc$Year, "2015")


lm_NARW <- lm(PercentPresence ~ #Month
                       Year
                       #+ as.factor(Period)
                       + DeviceType
                       ,data = NARWOcc)

print(class(NARWOcc$Period))
print(class(NARWOcc$Year))
print(class(NARWOcc$DeviceType))
print(class(NARWOcc$Month))
print(class(NARWOcc$Period))

print(lm_NARW)

car::Anova(lm_NARW)

summary(lm_NARW)


