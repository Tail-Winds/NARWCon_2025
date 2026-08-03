

# CALCULATE MONTHLY OCCURRENCE PERCENTAGES


# Load required libraries
library(dplyr)
library(lubridate)

# Load data
# -----------------------------------------------------------------
RWs_daily <- read.csv("~/Documents/Documents - Caroline’s MacBook Pro/Whale_Analyses/NARWCon_2025/DataRaw/masterNARW_noblanks.csv")
#RWs_wthPD <- read.csv("~/Documents/Documents - Caroline’s MacBook Pro/Manuscript_NARW Occurrence 2014-24/Analysis/RWs_wthPD.csv")

# percent occurence
monthly_occurrence <- RWs_daily %>%
  # 1. FIX THE DATE PARSING
  # Use mdy() if your data looks like "10/24/2014" or "10/24/14"
  # Use dmy() if your data looks like "24/10/2014" or "24/10/14"
  mutate(date = mdy(date)) %>%

  # Remove any rows where the date couldn't be parsed properly
  filter(!is.na(date)) %>%

  # 2. Extract the correct calendar Year and Month
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE)
  ) %>%

  # 3. Group by your exact column names
  group_by(year, month, site, device_type,) %>%

  # 4. Calculate metrics
  summarize(
    days_monitored = n(),
    days_present = sum(daily_occurrence, na.rm = TRUE),
    percent_occurrence = mean(daily_occurrence, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# View your corrected data
print(monthly_occurrence)

# Write to a new CSV file
write.csv(monthly_occurrence, "NARWPercentOccur_monthly.csv", row.names = FALSE)


###################################################################################################

# CALCULATE WEEKLY OCCURRENCE PERCENTAGES

library(dplyr)
library(lubridate)


# -----------------------------------------------------------------
weekly_occurrence <- RWs_daily %>%
    # Ensure date is recognized as a Date object
    mutate(date = mdy(date)) %>%

    # Group dates by the start of each week (e.g., every Monday)
    # Set week_start = 1 for Mondays, or week_start = 7 for Sundays
    mutate(week_start = floor_date(date, unit = "week", week_start = 1)) %>%

    # Group by week start date AND device_type
    group_by(week_start, site, device_type) %>%

    # Calculate weekly metrics
    summarize(
        days_monitored = n(),
        days_present = sum(daily_occurrence == 1, na.rm = TRUE),
        percent_occurrence = (days_present / days_monitored) * 100,
        .groups = "drop"
    )

# VIEW RESULTS
print(weekly_occurrence)

# Write to a new CSV file
write.csv(weekly_occurrence, "NARWPercentOccur_weekly.csv", row.names = FALSE)


###################################################################################################

# CALCULATE BI-WEEKLY OCCURRENCE PERCENTAGES

library(dplyr)
library(lubridate)


# -----------------------------------------------------------------
biweekly_occurrence <- RWs_daily %>%
    # Ensure date is recognized as a Date object
    mutate(date = mdy(date)) %>%

    # Group dates into 2-week intervals starting on Mondays (week_start = 1)
    mutate(biweek_start = as.Date(cut(date, breaks = "2 weeks", start.on.monday = TRUE))) %>%

    # Group by the 2-week start date AND device_type
    group_by(biweek_start, site, device_type) %>%

    # Calculate bi-weekly metrics
    summarize(
        days_monitored = n(),
        days_present = sum(daily_occurrence == 1, na.rm = TRUE),
        percent_occurrence = (days_present / days_monitored) * 100,
        .groups = "drop"
    )

# VIEW RESULTS
print(biweekly_occurrence)

# Write to a new CSV file
write.csv(biweekly_occurrence, "NARWPercentOccur_biweekly.csv", row.names = FALSE)


