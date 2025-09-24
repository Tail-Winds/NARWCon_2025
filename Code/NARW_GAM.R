rm(list = ls())
#library(tidyverse)
library(ggplot2)
theme_set(theme_light())
library(dplyr)
library(scales)
library(mgcv)
library(gamlss)
library(gamlss.tr)


RWocc <- read.csv("DataRaw/WhaleOccurrence2014_2024_091825.csv")
RWocc<- RWocc %>%
    mutate(
        #Year = ifelse(Month %in% month.name[1:10], Year),  #Year + 1
        Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d")
    ) %>%
    mutate(Month = as.numeric(format(Date, "%m")))
           #Period = ifelse(Year <= 2017, "PAM array", "RTWB")) %>%
    #select(-Study) %>%
    #arrange(Species, Date)
table(RWocc$Year, RWocc$Site)
table(RWocc$DeviceType, RWocc$Period)

# Change year to categorical
RWocc$YearCat <- as.factor(RWocc$Year)
RWocc$YearCat <- relevel(RWocc$YearCat, "2015") #relevel for intercept to be 2015 not 2014

#Change period (1 = 2014-17, 2 = 2021-2024) to categorical
RWocc$Period <- as.factor(RWocc$Period)


GGally::ggpairs(RWocc %>% dplyr::relocate(PercentOccurrence, .after = last_col())) #creates pairs plot, matrix pairwise relationships of all data
hist(RWocc$PercentOccurrence) # zero inflated distribution, effect which families might be good options
mean(RWocc$PercentOccurrence == 0) # proportion of 0s
sort(RWocc$PercentOccurrence)

boxplot(PercentOccurrence ~ Site, data = RWocc)

interaction.plot( RWocc$YearCat, RWocc$DeviceType, RWocc$PercentOccurrence)


m_RWocc <- gamlss(PercentOccurrence ~ #pbc(Month, max.df = 5) +
                   # pb(Year, max.df = 5) +
                   scs(Month, control = cs.control(cv = FALSE)) +
                   #ga(~ s(Month, bs = "cc")) +
                   YearCat +
                   #Site +
                   #Period +
                   DeviceType*YearCat
               # + Period
               , #+ Year:Period,
               # random = ~1 | Dummy
               correlation = corARMA(p = 1),
               # ), # creates an autocorrelation structure
               family = ZINBI,
               control = gamlss.control(c.crit = 0.01, n.cyc = 100),
               data = RWocc)
par(mfrow = c(1, 2))
plot(m_RWocc)
plot(m_RWocc, ts = TRUE)


summary(m_RWocc)
term.plot(m_RWocc)



## Model testing results -- 9/22/25
## Year as continuous with Site and Period added - AIC: 1132.014
##     ACF of 0.3 significant (above dotted line) lag around 2.5 and 18
##
## Year as categorical with Site and Period added - AIC 1125.248
##      Partial plots for month and year look way better
##      ACF still significant (does this matter if its 0.3?)
##      otherwise residual plots, homoskedascity, quantile resids look good
##
## Year as categorical with Site, Period AND DeviceType*Period - AIC:1125.248
##      very similar results


# Generate new "no year" and "no month" models for RW Likelihood Ratio tests
m_RWocc_noYear <- gamlss(PercentOccurrence ~ #pbc(Month, max.df = 5) +
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
                     data = RWocc)

m_RWocc_noMonth <- gamlss(PercentOccurrence ~ #pbc(Month, max.df = 5) +
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
                      data = RWocc)

#Running Likelihood ratio tests for each explanatory variable
LR.test(m_RWocc_noYear, m_RWocc)
LR.test(m_RWocc_noMonth, m_RWocc)
