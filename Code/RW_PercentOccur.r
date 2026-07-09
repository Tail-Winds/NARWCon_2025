

# CALCULATE MONTHLY OCCURRENCE PERCENTAGES


# Load required libraries
library(dplyr)
library(lubridate)

# Load data
# -----------------------------------------------------------------
RWs_noPD <- read.csv("~/Documents/Documents - Caroline’s MacBook Pro/Whale_Analyses/NARWCon_2025/DataRaw/masterNARW_noblanks.csv")
#RWs_wthPD <- read.csv("~/Documents/Documents - Caroline’s MacBook Pro/Manuscript_NARW Occurrence 2014-24/Analysis/RWs_wthPD.csv")

# percent occurence
monthly_occurrence_noPD <- RWs_noPD %>%
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
print(monthly_occurrence_noPD)

# Write to a new CSV file
write.csv(monthly_occurrence_noPD, "NARWPercentOccur.csv", row.names = FALSE)

# percent occurence
monthly_occurrence_wthPD <- RWs_wthPD %>%
  # 1. FIX THE DATE PARSING
  # Use mdy() if your data looks like "10/24/2014" or "10/24/14"
  # Use dmy() if your data looks like "24/10/2014" or "24/10/14"
  mutate(date = mdy(date)) %>%

  # Remove any rows where the date couldn't be parsed properly
  filter(!is.na(date)) %>%

  # 2. Extract the correct calendar Year and Month
  mutate(
    Year = year(date),
    Month = month(date, label = TRUE, abbr = TRUE)
  ) %>%

  # 3. Group by your exact column names
  group_by(Year, Month, device_type) %>%

  # 4. Calculate metrics
  summarize(
    days_monitored = n(),
    days_present = sum(daily_occurrence, na.rm = TRUE),
    percent_occurrence = mean(daily_occurrence, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# View your corrected data
print(monthly_occurrence_wthPD)

# Write to a new CSV file
write.csv(monthly_occurrence_wthPD, "PercentOccur_wthPD.csv", row.names = FALSE)

