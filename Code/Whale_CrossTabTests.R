rm(list = ls())
library(tidyverse)
theme_set(theme_light())
library(ggplot2)
library(dplyr)
library(scales)

# Data ----
# getwd()
# setwd("~/Desktop/KirstensWork/Progress reports/FinalReport")


# Only sites A-7M and RTWB for archival vs real time plot
WhaleOcc_rtwb <- read.csv("DataRaw/NARWOccurrence2021_2022_RTWBonly.csv")


D_rtwb <- WhaleOcc_rtwb %>%
    mutate(
        #Year = ifelse(Month %in% month.name[1:10], Year),  #Year + 1
        Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d"))
        # ArchivalPeriod = case_when(
        #     DeviceType == "Archival" & Date < as.Date("2018-01-01") ~ 1,
        #     DeviceType == "Archival" & Date >= as.Date("2023-01-01") ~ 2,
        #     TRUE ~ 0 # Or NA, for other devices/gaps
        #)


# Only sites A-7M and RTWB for archival vs real time plot
WhaleOcc <- read.csv("DataRaw/NARWOccurrence2014_2024_A7MandRTWBonly.csv")


D <- WhaleOcc %>%
      mutate(
          #Year = ifelse(Month %in% month.name[1:10], Year),  #Year + 1
          Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d"),
          ArchivalPeriod = case_when(
              DeviceType == "Archival" & Date < as.Date("2018-01-01") ~ 1,
              DeviceType == "Archival" & Date >= as.Date("2023-01-01") ~ 2,
              TRUE ~ 0 # Or NA, for other devices/gaps
          ))


WhaleOcc_all <- read.csv("DataRaw/NARWOccurrence2014_2024_070926.csv")


D_all <- WhaleOcc_all %>%
    mutate(
        #Year = ifelse(Month %in% month.name[1:10], Year),  #Year + 1
        Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d"),
        ArchivalPeriod = case_when(
            DeviceType == "Archival" & Date < as.Date("2018-01-01") ~ 1,
            DeviceType == "Archival" & Date >= as.Date("2023-01-01") ~ 2,
            TRUE ~ 0))

D_all$Month <- factor(D_all$Month, levels = month.name)

  # D$DeviceType[D$Year <= 2017] = "Archival"
  # D$DeviceType[D$Year >= 2021] = "Real Time"


# To show 2 years of RTWB monitoring 2021-2022

jpeg("PercentOcc_2021_2022_RTWB.jpeg", width = 1900, height = 1000, res = 250)


# Assuming your data frame is named 'D'

ggplot(D_rtwb, aes(x = Date, y = PercentOccurrence)) +

    # 1. Plot Archival Data (Behind and Transparent)
    # The Archival data is plotted first, using filter() to select it
    geom_line(
        aes(color = DeviceType),
        linewidth = 1.2,
        alpha = 0.8 # <--- No alpha here (or alpha = 1)
    ) +

    # The rest of your code remains the same
    scale_x_date(
        date_labels = "%B %Y",
        date_breaks = "2 month"
    ) +
    scale_y_continuous(limits=c(0,100)) +
    scale_color_manual(
        # The color values define the colors for the legends and lines
        values = c("Real Time" = "#377eb8"),
        breaks = c("Real Time")
    ) +
    # NOTE: You don't need scale_linetype_manual if both are "solid"
    theme_minimal() +
    labs(
        y = "Percent Occurrence",
        x = "Date"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()


# STACKED BAR CHARTS AND OTHER SITE/YEAR VISUALIZATIONS

year_colors <- c(
    # Period 1 (2014–2017): Warm Oranges (Light -> Dark)
    "2014" = "#FDD0A2",  # Light Peach/Orange
    "2015" = "#FDAE6B",  # Medium Orange
    "2016" = "#E6550D",  # Dark Orange
    "2017" = "#A63603",   # Deep Rust/Brown-Orange

    # Period 2 (2021–2024): Cool Blues (Light -> Dark)
    "2021" = "#C6DBEF",  # Very Light Blue
    "2022" = "#9ECAE1",  # Light Blue
    "2023" = "#4292C6",  # Medium Blue
    "2024" = "#08519C"  # Dark Navy
)

orange_year_colors <- c(
    "2014" = "#FDB863",  # Medium-light amber (much darker & visible)
    "2015" = "#FDAE6B",  # Warm medium orange
    "2016" = "#F16913",  # Vibrant orange
    "2017" = "#D94801",  # Dark orange
    "2021" = "#B33C00",  # Burnt orange
    "2022" = "#8C2D04",  # Deep rust
    "2023" = "#661E00",  # Dark mahogany orange
    "2024" = "#3D1200"   # Deepest brown-orange
)

custom_months <- c(month.name[10:12], month.name[1:9])

#Bar chart showing across months at different years

ggplot(D_all, aes(x = Month, y = PercentOccurrence, fill = factor(Year))) +
    geom_col(position = "dodge") +
    labs(
        x = "Month",
        y = "Percent Monthly Occurrence",
        fill = "Year"
    ) +
    scale_x_discrete(limits = custom_months) +
    scale_fill_manual(
        # The color values define the colors for the legends and lines
        values = year_colors) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 25) # Styles facet banner labels
    )

### Modified code
library(ggplot2)
library(dplyr)
library(tidyr) # Needed for complete()

# 1. Summarize and ensure all Month x Year combinations exist
D_summary <- D_all %>%
    group_by(Month, Year) %>%
    summarise(
        MeanOccurrence = mean(PercentOccurrence, na.rm = TRUE),
        SE = sd(PercentOccurrence, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
    ) %>%
    # Fill missing Month-Year combinations so empty factor slots remain consistent
    complete(Month, Year, fill = list(MeanOccurrence = 0, SE = NA))

# 2. Define a fixed dodge position object
dodge <- position_dodge(width = 0.8)

# 3. Plot with explicit group aesthetic
ggplot(D_summary, aes(
    x = Month,
    y = MeanOccurrence,
    fill = factor(Year),
    group = factor(Year) # Crucial: links bars and error bars to the same x-offsets
)) +
    geom_col(
        position = dodge,
        width = 0.7
    ) +
    # geom_errorbar(
    #     aes(ymin = MeanOccurrence - SE, ymax = MeanOccurrence + SE),
    #     position = dodge,
    #     width = 0.25,
    #     linewidth = 0.4,
    #     color = "black",
    #     na.rm = TRUE # Silently drops NA error bars without shifting positions
    # )
    labs(
        x = "Month",
        y = "Percent Monthly Occurrence",
        fill = "Year"
    ) +
    scale_x_discrete(limits = custom_months) +
    scale_y_continuous(limits = c(0, 100)) +
    scale_fill_manual(values = year_colors) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 25)
    )


# check data after noticing some months don't have error bars

# Check sample size (n) and SD for each group
D_all %>%
    group_by(Month, Year) %>%
    summarise(
        n_obs = sum(!is.na(PercentOccurrence)),
        sd_val = sd(PercentOccurrence, na.rm = TRUE),
        mean_val = mean(PercentOccurrence, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    filter(n_obs < 2 | sd_val == 0 | is.na(sd_val))

######

# dodge graph version of bar chart
ggplot(D, aes(x = Month, y = PercentOccurrence, fill = factor(Site))) +
    geom_col(position = "dodge") +
    labs(
        x = "Month",
        y = "Percent Monthly Occurrence",
        fill = "Site"
    ) +
    scale_x_discrete(limits = custom_months) +
    scale_fill_manual(
        # The color values define the colors for the legends and lines
        values = c("A7M" = "#41b6c4","RTWB" = "#225ea9")) +
    theme_minimal() +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Modified code

library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Summarize data by Month and Site
D_site_summary <- D %>%
    group_by(Month, Site) %>%
    summarise(
        MeanOccurrence = mean(PercentOccurrence, na.rm = TRUE),
        SE = sd(PercentOccurrence, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
    ) %>%
    # Fill missing Month-Site combinations so bar positions stay aligned
    complete(Month, Site, fill = list(MeanOccurrence = 0, SE = NA))

# 2. Define shared dodge position
dodge <- position_dodge(width = 0.8)

# 3. Plot with aligned error bars
ggplot(D_site_summary, aes(
    x = Month,
    y = MeanOccurrence,
    fill = factor(Site),
    group = factor(Site) # Aligns error bars to the corresponding site bar
)) +
    geom_col(
        position = dodge,
        width = 0.7
    ) +
    geom_errorbar(
        aes(ymin = MeanOccurrence - SE, ymax = MeanOccurrence + SE),
        position = dodge,
        width = 0.2,
        linewidth = 0.4,
        color = "black",
        na.rm = TRUE
    ) +
    labs(
        x = "Month",
        y = "Percent Monthly Occurrence",
        fill = "Site"
    ) +
    scale_x_discrete(limits = custom_months) +
    scale_y_continuous(limits = c(0, 100)) +
    scale_fill_manual(
        values = c("A7M" = "#41b6c4", "RTWB" = "#225ea9")
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

######

# dodge graph version of bar chart
ggplot(D_all, aes(x = Month, y = PercentOccurrence, fill = factor(Site))) +
    geom_col(position = "dodge") +
    labs(
        x = "Month",
        y = "Percent Monthly Occurrence",
        fill = "Site"
    ) +
    scale_x_discrete(limits = custom_months) +
    scale_fill_manual(
        # The color values define the colors for the legends and lines
        values = c("A2M" = "#7fcdbb","A7M" = "#41b6c4","RTWB" = "#225ea9", "T2M" ="#081d58")) +
    theme_minimal() +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Line graph version showing trends across months for different years

ggplot(D, aes(x = Month, y = PercentOccurrence, color = factor(Year), group = factor(Year))) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) + # Optional: Adds data points on each month for clarity
    labs(
        x = "Month",
        y = "Percent Monthly Occurrence",
        color = "Year"
    ) +
    scale_x_discrete(limits = custom_months) +
    scale_color_manual(values = year_colors) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 25)
    )


#Bar chart showing across months at different sites

ggplot(D_all, aes(x = Month, y = PercentOccurrence, fill = factor(Site))) +
    geom_col(position = "stack") +
    labs(
        x = "Month",
        y = "Proportion of Monthly Occurrence",
        fill = "Site"
    ) +
    scale_x_discrete(limits = custom_months) +
    scale_fill_manual(
        # The color values define the colors for the legends and lines
        values = c("A2M" = "#7fcdbb","A7M" = "#41b6c4","RTWB" = "#225ea9", "T2M" ="#081d58")) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 15) # Styles facet banner labels
    )


#Bar chart showing across months at different sites - showing proportion of monthly occurrence by site

ggplot(D_all, aes(x = Month, y = PercentOccurrence, fill = factor(Site))) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = scales::percent) + # Formats y-axis as 0% - 100%
    labs(
        x = "Month",
        y = "Proportion of Monthly Occurrence",
        fill = "Site"
    ) +
    scale_x_discrete(limits = custom_months) +
    scale_fill_manual(
        # The color values define the colors for the legends and lines
        values = c("A2M" = "#7fcdbb","A7M" = "#41b6c4","RTWB" = "#225ea9", "T2M" ="#081d58")) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 15) # Styles facet banner labels
    )



#To show A7M and RTWB as Archival vs Real Time plot

jpeg("PercentOcc_2014_2024_102926.jpeg", width = 1950, height = 1000, res = 250)

# Assuming your data frame is named 'D'

ggplot(D, aes(x = Date, y = PercentOccurrence)) +

    # 1. Plot Archival Data (Behind and Transparent)
    # The Archival data is plotted first, using filter() to select it
    geom_line(
        data = filter(D, DeviceType == "Archival"),
        aes(color = DeviceType, group = interaction(DeviceType, ArchivalPeriod)),
        linewidth = 1.2,
        alpha = 0.7  # <--- Transparency applied ONLY to Archival
    ) +

    # 2. Plot Real Time Data (In Front and Opaque)
    # The Real Time data is plotted second, ensuring it appears on top
    geom_line(
        data = filter(D, DeviceType == "Real Time"),
        aes(color = DeviceType, group = interaction(DeviceType, ArchivalPeriod)),
        linewidth = 1.2,
        alpha = 0.8 # <--- No alpha here (or alpha = 1)
    ) +

    # The rest of your code remains the same
    scale_x_date(
        date_labels = "%B %Y",
        date_breaks = "6 month"
    ) +
    scale_color_manual(
        # The color values define the colors for the legends and lines
        values = c("Real Time" = "#377eb8", "Archival" = "#ff7f00"),
        breaks = c("Real Time", "Archival")
    ) +
    # NOTE: You don't need scale_linetype_manual if both are "solid"
    scale_linetype_manual(
        values = c("Real Time" = "solid", "Archival" = "solid")
    ) +
    theme_minimal() +
    labs(
        y = "Percent Occurrence",
        x = "Date",
        color = "DeviceType" # Ensure the legend title is correct
    ) +
    scale_y_continuous(limits = c(0, 100)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

#To make figure showing all sites percent occcurrence not just A-7M and RTWB

jpeg("PercentOcc_2014_2024_All Sites_101326.jpeg", width = 1900, height = 1000, res = 250)


# Assuming your data frame is named 'D'

ggplot(D_all, aes(x = Date, y = PercentOccurrence)) +

    # 1. Plot Archival Data (Behind and Transparent)
    # The Archival data is plotted first, using filter() to select it
    geom_line(
        data = filter(D_all, Site == "A2M"),
        aes(color = Site, group = interaction(Site, ArchivalPeriod)),
        linewidth = 1.2,
        alpha = 1  # <--- Transparency applied ONLY to Archival
    ) +

    geom_line(
        data = filter(D_all, Site == "A7M"),
        aes(color = Site, group = interaction(Site, ArchivalPeriod)),
        linewidth = 1.2,
        alpha = 1  # <--- Transparency applied ONLY to Archival
    ) +

    geom_line(
        data = filter(D_all, Site == "T2M"),
        aes(color = Site, group = interaction(Site, ArchivalPeriod)),
        linewidth = 1.2,
        alpha = 0.8  # <--- Transparency applied ONLY to Archival
    ) +

    # 2. Plot Real Time Data (In Front and Opaque)
    # The Real Time data is plotted second, ensuring it appears on top
    geom_line(
        data = filter(D_all, Site == "RTWB"),
        aes(color = Site, group = interaction(Site, ArchivalPeriod)),
        linewidth = 1.2,
        alpha = 1 # <--- No alpha here (or alpha = 1)
    ) +

    # The rest of your code remains the same
    scale_x_date(
        date_labels = "%B %Y",
        date_breaks = "3 month"
    ) +
    scale_color_manual(
        # The color values define the colors for the legends and lines
        values = c("A2M" = "#7fcdbb","A7M" = "#41b6c4","RTWB" = "#225ea9", "T2M" ="#081d58"),
        breaks = c("A2M", "A7M", "RTWB", "T2M")
    ) +
    # NOTE: You don't need scale_linetype_manual if both are "solid"
    # scale_linetype_manual(
    #     values = c("Real Time" = "solid", "Archival" = "solid")
    # ) +
    theme_minimal() +
    labs(
        y = "Percent Occurrence",
        x = "Date",
        color = "Site" # Ensure the legend title is correct
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()



# Plot sites individually

jpeg("PercentOcc_2014_2024_All Sites sep_102926.jpeg", width = 1900, height = 1000, res = 250)

# 1. Convert Site to a factor ordered from Inshore -> Offshore
D_all <- D_all %>%
    mutate(Site = factor(Site, levels = c("A2M", "A7M", "RTWB", "T2M")))

# 2. Plot with single geom_line layer and faceting
ggplot(D_all, aes(x = Date, y = PercentOccurrence)) +
    geom_line(
        aes(color = Site, group = interaction(Site, ArchivalPeriod)),
        linewidth = 1,
        alpha = 0.95
    ) +
    # Facet into stacked panels to easily align time series horizontally
    facet_wrap(~ Site, ncol = 1) +
    scale_x_date(
        date_labels = "%b %Y",
        date_breaks = "6 month"
    ) +

    scale_color_manual(
        values = c(
            "A2M"  = "#7fcdbb",  # nearshore
            "A7M"  = "#41b6c4",  # cyan
            "RTWB" = "#225ea9",  #  blue
            "T2M"  = "#081d58"   #  offshore deep blue
        )
    ) +
    theme_minimal() +
    labs(
        x = "Date",
        y = "Percent Occurrence",
        color = "Site"
    ) +
    scale_y_continuous(limits = c(0, 100)) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 10) # Styles facet banner labels
    )

dev.off()



jpeg("PercentOcc_2014_2024_070926.jpeg", width = 1900, height = 1000, res = 250)

ggplot(D, aes(x = Date, y = PercentOccurrence)) +

    # 1. Plot Archival Data (Behind and Transparent)
    # The Archival data is plotted first, using filter() to select it
    geom_line(
        data = filter(D, DeviceType == "Archival"),
        aes(color = DeviceType, group = interaction(DeviceType, ArchivalPeriod)),
        linewidth = 1.2,
        alpha = 0.7  # <--- Transparency applied ONLY to Archival
    ) +

# 2. Plot Real Time Data (In Front and Opaque)
# The Real Time data is plotted second, ensuring it appears on top
geom_line(
    data = filter(D, Site == "RTWB"),
    aes(color = Site, group = interaction(Site, ArchivalPeriod)),
    linewidth = 1.2,
    alpha = 0.8 # <--- No alpha here (or alpha = 1)
) +

    # The rest of your code remains the same
    scale_x_date(
        date_labels = "%B %Y",
        date_breaks = "6 month"
    ) +
    scale_color_manual(
        # The color values define the colors for the legends and lines
        values = c("Real Time" = "#377eb8", "Archival" = "#ff7f00"),
        breaks = c("Real Time", "Archival")
    ) +
    # NOTE: You don't need scale_linetype_manual if both are "solid"
    scale_linetype_manual(
        values = c("Real Time" = "solid", "Archival" = "solid")
    ) +
    theme_minimal() +
    labs(
        y = "Percent Occurrence",
        x = "Date",
        color = "DeviceType" # Ensure the legend title is correct
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))




#Dw_long <- Dw_wide %>%
#pivot_longer(
#cols = -c(Month, Year, Study, Date),  # Keep these as ID columns
#names_to = "Species",
#values_to = "PercentOccurrence"
#)

#write.csv(D, "WhaleOccurrence_2014_2024_082725.csv", row.names = FALSE)

# MANOVA ----
# mlm0 <- lm(cbind(FinWhale, NARW, HumpbackWhale) ~ #-1 #, FreqDelta
#              Month
#            #+ as.factor(Year)
#            + Year
#            + Study
#            ,data = Dw)
# summary(mlm0)
#
# car::Manova(mlm0)
#
# # ANOVA F-tests
#
# lm_FinWhale <- lm(FinWhale ~ Month
#            + Year
#            + Study
#            ,data = Dw)
# car::Anova(lm_FinWhale)
#
#
# lm_HumpbackWhale <- lm(HumpbackWhale ~ Month
#                   + Year
#                   + Study
#                   ,data = Dw)
# car::Anova(lm_HumpbackWhale)
#
#
# lm_NARW <- lm(NARW ~ Month #percent presence ~ month, year, period, device
#                        + Year
#                        + Study
#                        ,data = Dw)
# car::Anova(lm_NARW)



########################################## Caroline's old code adjusted Oct 8, 2025
# ggplot(D, aes(x = Date, y = PercentOccurrence, color = DeviceType)) +
#     geom_line(aes(group = interaction(DeviceType, ArchivalPeriod)),linewidth = 1.2, alpha = 0.5) +
#     #facet_wrap(~Species, ncol = 1, scales = "free_y") +
#     scale_x_date(
#         date_labels = "%B %Y",
#         date_breaks = "6 month"
#     ) +
#     scale_color_manual(
#         values = c("Real Time" = "#377eb8", "Archival" = "#ff7f00"),
#         breaks = c("Real Time", "Archival")
#     ) +
#     scale_linetype_manual(
#         values = c("Real Time" = "solid", "Archival" = "solid")
#     ) +
#     theme_minimal() +
#     labs(
#         y = "Percent Occurrence",
#         x = "Date"
#     ) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))



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
