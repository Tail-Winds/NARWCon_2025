rm(list = ls())
#library(tidyverse)
library(ggplot2)
theme_set(theme_light())
library(dplyr)
library(scales)
library(mgcv)
library(gamlss)
library(gamlss.tr)

D <- read.csv("data/WhaleOccurrence_2014_2025_090325.csv", header = TRUE)
D <- D %>%
    mutate(
        #Year = ifelse(Month %in% month.name[1:10], Year),  #Year + 1
        Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d")
    ) %>%
    mutate(Month = as.numeric(format(Date, "%m")),
           Period = ifelse(Year <= 2017, "PAM array", "RTWB")) %>%
    select(-Study) %>%
    arrange(Species, Date)

# Change year to categorical
D$YearCat <- as.factor(D$Year)
D$YearCat <- relevel(D$YearCat, "2015") #relevel for intercept to be 2015 not 2014

GGally::ggpairs(D_rw) #creates pairs plot, matrix pairwise relationships of all data
hist(D_rw$PercentOccurrence) # zero inflated, effect which families might be good options
sort(D_rw$PercentOccurrence)

m_rw <- gamlss(PercentOccurrence ~ #pbc(Month, max.df = 5) +
                   # pb(Year, max.df = 5) +
                   scs(Month, control = cs.control(cv = FALSE)) +
                   #ga(~ s(Month, bs = "cc")) +
                   YearCat
               # + Period
               , #+ Year:Period,
               # random = ~1 | Dummy
               #correlation = corARMA(p = 1)
               # ), # creates an autocorrelation structure
               family = ZINBI,
               control = gamlss.control(c.crit = 0.01, n.cyc = 100),
               data = D_rw)
par(mfrow = c(1, 2))
plot(m_rw)
plot(m_rw, ts = TRUE)

summary(m_rw)
term.plot(m_rw)

# Generate new "no year" and "no month" models for RW Likelihood Ratio tests
m_rwnoYear <- gamlss(PercentOccurrence ~ #pbc(Month, max.df = 5) +
                         # pb(Year, max.df = 5) +
                         scs(Month, control = cs.control(cv = FALSE))
                     #+
                     #ga(~ s(Month, bs = "cc")) +
                     #YearCat
                     # + Period
                     , #+ Year:Period,
                     # random = ~1 | Dummy
                     #correlation = corARMA(p = 1)
                     # ), # creates an autocorrelation structure
                     family = ZINBI,
                     control = gamlss.control(c.crit = 0.01, n.cyc = 100),
                     data = D_rw)

m_rwnoMonth <- gamlss(PercentOccurrence ~ #pbc(Month, max.df = 5) +
                          # pb(Year, max.df = 5) +
                          #scs(Month, control = cs.control(cv = FALSE))
                          #ga(~ s(Month, bs = "cc")) +
                          YearCat
                      # + Period
                      , #+ Year:Period,
                      # random = ~1 | Dummy
                      #correlation = corARMA(p = 1)
                      # ), # creates an autocorrelation structure
                      family = ZINBI,
                      control = gamlss.control(c.crit = 0.01, n.cyc = 100),
                      data = D_rw)

#Running Likelihood ratio tests for each explanatory variable
LR.test(m_rw_noYear, m_rw)
LR.test(m_rw_noMonth, m_rw)
