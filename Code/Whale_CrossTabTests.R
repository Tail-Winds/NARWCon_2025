rm(list = ls())
library(tidyverse)
theme_set(theme_light())
library(ggplot2)
library(dplyr)
library(scales)

# Data ----
# getwd()
# setwd("~/Desktop/KirstensWork/Progress reports/FinalReport")
WhaleOcc <- read.csv("DataRaw/NARWWhaleOccur_2014_2024.csv")


D <- WhaleOcc %>%
      mutate(
          #Year = ifelse(Month %in% month.name[1:10], Year),  #Year + 1
          Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d"),
          ArchivalPeriod = case_when(
              DeviceType == "Archival" & Date < as.Date("2018-01-01") ~ 1,
              DeviceType == "Archival" & Date >= as.Date("2023-01-01") ~ 2,
              TRUE ~ 0 # Or NA, for other devices/gaps
          )
      )
  # D$DeviceType[D$Year <= 2017] = "Archival"
  # D$DeviceType[D$Year >= 2021] = "Real Time"



  #Dw_long <- Dw_wide %>%
      #pivot_longer(
          #cols = -c(Month, Year, Study, Date),  # Keep these as ID columns
          #names_to = "Species",
          #values_to = "PercentOccurrence"
      #)

#write.csv(D, "WhaleOccurrence_2014_2024_082725.csv", row.names = FALSE)

# MANOVA ----
mlm0 <- lm(cbind(FinWhale, NARW, HumpbackWhale) ~ #-1 #, FreqDelta
             Month
           #+ as.factor(Year)
           + Year
           + Study
           ,data = Dw)
summary(mlm0)

car::Manova(mlm0)

# ANOVA F-tests

lm_FinWhale <- lm(FinWhale ~ Month
           + Year
           + Study
           ,data = Dw)
car::Anova(lm_FinWhale)


lm_HumpbackWhale <- lm(HumpbackWhale ~ Month
                  + Year
                  + Study
                  ,data = Dw)
car::Anova(lm_HumpbackWhale)


lm_NARW <- lm(NARW ~ Month #percent presence ~ month, year, period, device
                       + Year
                       + Study
                       ,data = Dw)
car::Anova(lm_NARW)

jpeg("PercentOcc_2014_2024_100525.jpeg", width = 1900, height = 1000, res = 250)

ggplot(D, aes(x = Date, y = PercentOccurrence, color = DeviceType)) +
    geom_line(aes(group = interaction(DeviceType, ArchivalPeriod)),linewidth = 1.2, alpha = 0.7) +
    #facet_wrap(~Species, ncol = 1, scales = "free_y") +
    scale_x_date(
        date_labels = "%B %Y",
        date_breaks = "6 month"
    ) +
    scale_color_manual(
        values = c("Archival" = "#377eb8", "Real Time" = "#ff7f00"),
        breaks = c("Archival", "Real Time")
    ) +
    theme_minimal() +
    labs(
        y = "Percent Occurrence",
        x = "Date"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

########################################## Kirsten's old code

#reformat data in csv

#D = WhaleOcc #%>%
#pivot_longer(starts_with("Y20"),
#names_to = "Year",
#values_to = "PercentOccurrence") %>%
#mutate(Year = as.numeric(substr(Year, 2, 5)),
#study = "RTWB") +
# Correct Year to the next year for months Jan - Oct
# because the year is recorded starting in Nov
#mutate(Year = ifelse(Month %in% month.name[1:10], Year + 1, Year)) %>%
#mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d"))
#D$Study[D$Year <= 2017] = "PAM array"
#Dw = D
#pivot_wider(names_from = Species,
# id_cols = c(Month, Year, Study, Date),
#values_from = PercentOccurrence)

#plot data

# set.seed(123)
#Dw %>% ggplot(aes(x=Date, y=PercentOccurrence, color = Study)) +
# geom_jitter(width = 0, height = 2) +
#geom_line(lwd = 1.5) +
#facet_wrap(~Species, ncol = 1)

########################################## Caroline's Code Edits 1/26/24

#D$MonthYear <- merge(D$Month, D$Year) ### Doesn't work, got an error message
                                      ### Tried to create a new column in the dataframe like 'Nov-2014' for ex.
                                      ### so that it is shown as a time series and not grouping by month

### Decided to make plots where year is shown by color (fill) and to do this had to subset by study

#D_BOEM <- subset(D, Study=="BOEM")    ### Subset BOEM only
#D_RTWB <- subset(D, Study=="RTWB")    ### Subset RTWB only

#set.seed(123)
## BOEM
#D_BOEM %>% ggplot(aes(x=Month, y=PercOccur, fill = Year)) +     ### x = Month instead of Year here, need to rotate label
    #geom_jitter(width = 0, height = 2) + facet_grid(~Species)   ### Year is being shown as a scale, how do we fix this?

## RTWB
#D_RTWB %>% ggplot(aes(x=Month, y=PercOccur, fill = Year)) +
  #geom_jitter(width = 0, height = 2) + facet_grid(~Species)
