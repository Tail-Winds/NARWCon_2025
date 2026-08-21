# read_env.R reads the environmental data (DOPPIO + NDBC + WAVERYS, 2014–2024)
# Sites: A-7M, A-2M, T-2M   |   Resolutions: "hourly", "three_hourly", "daily"
#
# First time only, install the reader package:  install.packages("nanoparquet")

library(nanoparquet)   # reads .parquet files (no system dependencies)

# get_env(): return a data.frame of environmental data for one site.
# Optionally limit it to a time window and/or to specific variables.
#
#   site        which mooring: "A-7M", "A-2M", or "T-2M"
#   vars        which columns you want, e.g. c("sst","wav_hs"); omit if you want all vars
#   start, end  time window, e.g. "2018-06-01" (UTC); omit for the full record
#   resolution  "hourly", "three_hourly", or "daily"
#   dir         folder that holds the .parquet files
get_env <- function(site, vars = NULL, start = NULL, end = NULL,
                    resolution = "hourly", dir = "cache_2014-2024") {

  #Builds the file name for this site + resolution, then open it.
  file <- file.path(dir, paste0(site, "_20140101-20241231_", resolution, ".parquet"))
  if (!file.exists(file)) stop("Data file not found: ", file)
  df <- as.data.frame(read_parquet(file))
  if (inherits(df$time, "POSIXct")) attr(df$time, "tzone") <- "UTC"  # timestamps are UTC

  # Keepd only rows in the requested time window.
  # (daily files store dates; hourly/3-hourly store full timestamps — this
  # just matches the type so the >= / <= comparison is correct.)
  as_time <- if (inherits(df$time, "Date")) as.Date else function(x) as.POSIXct(x, tz = "UTC")
  if (!is.null(start)) df <- df[df$time >= as_time(start), ]
  if (!is.null(end))   df <- df[df$time <= as_time(end),   ]

  # Keeps only the requested variables ("time" is always kept).
  if (!is.null(vars)) df <- df[, c("time", vars)]

  df   # Voila! hands the finished dataframe back :)
}

# ── Broken out by Site ──────────────────────────────────────────────────────────────

# Site A-2M

# whole record, one site, hourly:
a2m <- get_env("A-2M")

# a time window + chosen variables, 3-hourly (includes waves):
a2m_3hrly <- get_env("A-2M",
             vars = c("sst", "ndbc_wspd", "wav_hs"),
             start = "2014-11-03", end = "2024-12-31",
             resolution = "three_hourly")

# daily bottom temperature for data series:
a2m_dailyenv <- get_env("A-2M", vars = c("bottom_temp","sst", "sss", "zeta"),
             start = "2014-11-03", end = "2024-12-31",
             resolution = "daily")

# add_distances(): attach the static per-site distances (km) + depth (m) to a
# get_env() result. They don't vary in time, so the same values go on every row.
add_distances <- function(df, site, file = "site_distances.csv") {
  a2m_dailyenv   <- read.csv(file)
  drow <- a2m_dailyenv[a2m_dailyenv$site == site, -1, drop = FALSE]
  cbind(df, drow[rep(1, nrow(df)), , drop = FALSE], row.names = NULL)
}
                                        # example:  d <- add_distances(get_env("A-2M"), "A-2M")   # adds dist_shore_km, depth_m, etc.

                                        # OR you can just use the distance csv file by itself.  The add-distances is optional.
a2m_dailyenv <- add_distances(a2m_dailyenv, "A-2M")

# Write to a new CSV file
write.csv(a2m_dailyenv, "A2M_NARW_dailyenv.csv", row.names = FALSE)

# Site A-7M

# whole record, one site, hourly:
a7m <- get_env("A-7M")


# a time window + chosen variables, 3-hourly (includes waves):
a7m_3hrly <- get_env("A-7M",
                     vars = c("sst", "ndbc_wspd", "wav_hs"),
                     start = "2014-11-03", end = "2024-12-31",
                     resolution = "three_hourly")

# daily bottom temperature for data series:
a7m_dailyenv <- get_env("A-7M", vars = c("bottom_temp","sst", "sss", "zeta"),
                        start = "2014-11-03", end = "2024-12-31",
                        resolution = "daily")

# add_distances(): attach the static per-site distances (km) + depth (m) to a
# get_env() result. They don't vary in time, so the same values go on every row.
add_distances <- function(df, site, file = "site_distances.csv") {
  a7m_dailyenv   <- read.csv(file)
  drow <- a7m_dailyenv[a7m_dailyenv$site == site, -1, drop = FALSE]
  cbind(df, drow[rep(1, nrow(df)), , drop = FALSE], row.names = NULL)
}
# example:  d <- add_distances(get_env("A-7M"), "A-7M")   # adds dist_shore_km, depth_m, etc.

# OR you can just use the distance csv file by itself.  The add-distances is optional.
a7m_dailyenv <- add_distances(a7m_dailyenv, "A-7M")

# Write to a new CSV file
write.csv(a7m_dailyenv, "A7M_NARW_dailyenv.csv", row.names = FALSE)



# Site T-2M

# whole record, one site, hourly:
t2m <- get_env("T-2M")

# a time window + chosen variables, 3-hourly (includes waves):
t2m_3hrly <- get_env("T-2M",
                     vars = c("sst", "ndbc_wspd", "wav_hs"),
                     start = "2014-11-03", end = "2024-12-31",
                     resolution = "three_hourly")

# daily bottom temperature for data series:
t2m_dailyenv <- get_env("T-2M", vars = c("bottom_temp","sst", "sss", "zeta"),
                        start = "2014-11-03", end = "2024-12-31",
                        resolution = "daily")

# add_distances(): attach the static per-site distances (km) + depth (m) to a
# get_env() result. They don't vary in time, so the same values go on every row.
add_distances <- function(df, site, file = "site_distances.csv") {
  t2m_dailyenv   <- read.csv(file)
  drow <- t2m_dailyenv[t2m_dailyenv$site == site, -1, drop = FALSE]
  cbind(df, drow[rep(1, nrow(df)), , drop = FALSE], row.names = NULL)
}
# example:  d <- add_distances(get_env("T-2M"), "T-2M")   # adds dist_shore_km, depth_m, etc.

# OR you can just use the distance csv file by itself.  The add-distances is optional.
t2m_dailyenv <- add_distances(t2m_dailyenv, "T-2M")

# Write to a new CSV file
write.csv(t2m_dailyenv, "T2M_NARW_dailyenv.csv", row.names = FALSE)


# Exploratory analysis ----

# Data loading and modification
# library(tidyverse)
library(dplyr)
library(ggplot2)
theme_set(theme_light())
library(patchwork)
library(GGally)
library(lubridate)
library(corrplot)
library(suncalc)

# Load master data ----

RW_dailyenv0 <- read.csv("Master_NARWdailyenv2.csv")
RW_dailyenv <- RW_dailyenv0 %>%
  mutate(Date = mdy(Date)) %>%
  mutate(Month = as.numeric(format(Date, "%m"))) %>%
  # Change Site, Month, and Period (1 = 2014-17, 2 = 2021-2024) to categorical
  mutate(MonthCat = as.factor(Month),
         Period = as.factor(Period),
         Site = as.factor(Site)) %>%
  # Create lagged values of DailyOccurrence, by Site
  # group_by(Site) %>%
  # mutate(DailyOccurrence_l1 = dplyr::lag(DailyOccurrence, 1, order_by = Date),
  #        DailyOccurrence_l2 = dplyr::lag(DailyOccurrence, 2, order_by = Date)) %>%
  # ungroup() %>%
  # Sort by Site and Date
  arrange(Site, Date)

RW_dailyenv <- RW_dailyenv %>%
  mutate(date = as.Date(Date))


# lunar phase
moon_raw <- getMoonIllumination(date = RW_dailyenv$Date) %>%
    select(date, phase)

RW_dailyenv <- RW_dailyenv %>%
    left_join(moon_raw, by = "date") %>%
    mutate(
        # 4-Phase categorization
        # lunar_phase = case_when(
        #   phase < 0.125 | phase >= 0.875 ~ "New Moon",
        #   phase >= 0.125 & phase < 0.375 ~ "Waxing",
        #   phase >= 0.375 & phase < 0.625 ~ "Full Moon",
        #   phase >= 0.625 & phase < 0.875 ~ "Waning"
        # ),
        #8 day phase detailed
        lunar_phase = case_when(
            phase < 0.0625 | phase >= 0.9375 ~ "New Moon",
            phase >= 0.0625 & phase < 0.1875 ~ "Waxing Crescent",
            phase >= 0.1875 & phase < 0.3125 ~ "First Quarter",
            phase >= 0.3125 & phase < 0.4375 ~ "Waxing Gibbous",
            phase >= 0.4375 & phase < 0.5625 ~ "Full Moon",
            phase >= 0.5625 & phase < 0.6875 ~ "Waning Gibbous",
            phase >= 0.6875 & phase < 0.8125 ~ "Last Quarter",
            phase >= 0.8125 & phase < 0.9375 ~ "Waning Crescent"
        ),
        # # Convert to ordered factor
        # lunar_phase = factor(lunar_phase, levels = c(
        #   "New Moon", "Waxing", "Full Moon", "Waning"
        # )),
        lunar_phase = factor(lunar_phase, levels = c(
            "New Moon", "Waxing Crescent", "First Quarter", "Waxing Gibbous",
            "Full Moon", "Waning Gibbous", "Last Quarter", "Waning Crescent"
        ))
    )



# daylight duration
coords_df <- RW_dailyenv %>%
  select(date, lat, lon)

# Retrieve daily sunrise and sunset times
sun_times <- getSunlightTimes(
  data = coords_df,
  keep = c("sunrise", "sunset"),
  tz = "UTC" # Calculating in UTC avoids Daylight Savings Time headaches
)

# Calculate decimal hours of daylight and join back
RW_dailyenv <- RW_dailyenv %>%
  mutate(
    daylight_hours = as.numeric(difftime(sun_times$sunset, sun_times$sunrise, units = "hours"))
  )

# Add cold pool index

# Create the index as a categorical factor with descriptive labels
RW_dailyenv <- RW_dailyenv %>%
  mutate(
    cold_pool_index = if_else(
      stratification >= 0.2 & bottom_temp <= 10,
      "Present",
      "Absent"
    ),
    # Convert to a factor
    cold_pool_index = factor(cold_pool_index, levels = c("Absent", "Present"))
  )

#subset of data only environmental variables
env_vars <- RW_dailyenv[, c("sst", "bottom_temp", "sss", "stratification","depth_m",
                            "lunar_phase", "daylight_hours", "cold_pool_index")]

#subset of data only environmental variables with occurrence
env_varsRW <- RW_dailyenv[, c("sst", "bottom_temp", "sss", "stratification","depth_m",
                              "lunar_phase", "daylight_hours", "cold_pool_index", "DailyOccurrence")]

# GGpairs for pairwise grid - assess multicollinearity

# Scatter plots on one half, density distributions on the diagonal,
# and correlation coefficients (with significance levels) other half
# correlation values (r) > 0.7-0.8 between two variables = highly collinear

ggpairs(env_vars,
        title = "Pairwise Correlations of Environmental Variables",
        upper = list(continuous = wrap("cor", size = 4.5)),
        lower = list(continuous = wrap("points", alpha = 0.6, size = 1))) +
  theme_minimal()


ggpairs(env_varsRW,
        title = "Pairwise Correlations of Environmental Vars with RW Daily Occurrence",
        upper = list(continuous = wrap("cor", size = 4.5)),
        lower = list(continuous = wrap("points", alpha = 0.6, size = 1))) +
  theme_minimal()


# Calculate the correlation matrix (using pairwise complete observations)
cor_matrix <- cor(env_vars, use = "complete.obs")

# Plot the matrix
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust", # Groups similar variables together
         addCoef.col = "black", # Show correlation coefficients
         tl.col = "black",
         tl.srt = 45) # Rotate text labels

# Check temporal autocorrelation for Sea Surface Temperature
acf(RW_dailyenv$sst,
    main = "Temporal Autocorrelation (SST)",
    lag.max = 30) # Look at a 30-day window

# Check temporal autocorrelation for Sea Surface Salinity
acf(RW_dailyenv$sss,
    main = "Temporal Autocorrelation (SSS)",
    lag.max = 30) # Look at a 30-day window

# Check temporal autocorrelation for Sea Surface Salinity
acf(RW_dailyenv$daylight_hours,
    main = "Temporal Autocorrelation (photoperiod)",
    lag.max = 30) # Look at a 30-day window


ggplot(RW_dailyenv, aes(x = lunar_phase, y = DailyOccurrence)) +
  geom_boxplot(fill = "lightblue")








# Sample sizes for different combinations
table(RW_dailyenv$Year, RW_dailyenv$Site)
table(RW_dailyenv$DeviceType, RW_dailyenv$Period)

# Pairs plot, matrix pairwise relationships of all data
GGally::ggpairs(RW_dailyenv %>% dplyr::relocate(DailyOccurrence, .after = last_col()))

# Histogram of response variable
hist(RW_dailyenv$DailyOccurrence)
# zero inflated distribution, effect which families might be good options
mean(RW_dailyenv$DailyOccurrence == 0) # proportion of 0s
sort(RW_dailyenv$DailyOccurrence)

# Boxplots of Daily Occurrence by different categorical variables
p1 <- ggplot(RW_dailyenv, aes(x = as.factor(Year), y = DailyOccurrence)) +
  geom_boxplot(fill = "lightblue") +
  xlab("Year")
p2 <- ggplot(RW_dailyenv, aes(x = Month, y = DailyOccurrence)) +
  geom_boxplot(fill = "lightblue")
p3 <- ggplot(RW_dailyenv, aes(x = Period, y = DailyOccurrence)) +
  geom_boxplot(fill = "lightblue")
p4 <- ggplot(RW_dailyenv, aes(x = DeviceType, y = DailyOccurrence)) +
  geom_boxplot(fill = "lightblue")
p5 <- ggplot(RW_dailyenv, aes(x = Site, y = DailyOccurrence), facet_wrap(~Month)) +
  geom_boxplot(fill = "lightblue")
# Combine with patchwork
(p1 | p2) / (p3 | p4 | p5) +
  plot_annotation(title = "Daily occurrence by different categorical variables")

# Interaction plot
with(RW_dailyenv,
     interaction.plot(sst, bottom_temp, DailyOccurrence)
)
with(RW_dailyenv,
     interaction.plot(YearCat, MonthCat, DailyOccurrence, col = MonthCat)
)

# Chl-a and percent occurrence ggpairs analysis

library(tidyverse)
library(GGally)

d <- read_csv("/Users/kirsten/Desktop/KirstensWork/NARW Consortium 2025/NARWCon_2025/DataRaw/NARWPercentOccur_Chla_8daySummary_080726.csv")


d1 <- d %>%
    # Ensure date column is formatted properly and extract year
    mutate(
        # Explicitly convert to Date class first
        date_clean = as.Date(date),
        year = factor(format(date_clean, "%Y"))
    ) %>%
    # Filter strictly for 2014–2017
    filter(year %in% c("2014", "2015", "2016", "2017")) %>%
    # Drop NAs strictly from target analytical variables
    drop_na(percent_occurrence, mean_chla) %>%
    # Filter out non-positive chl-a values prior to log transformation
    filter(chla_mean > 0) %>%
    # Log10 transform Chl-a to address right-skewness
    mutate(log10_chla = log10(chla_mean))

#add scatter points and a linear model fit line
custom_smooth <- function(data, mapping, ...) {
    ggplot(data = data, mapping = mapping) +
        geom_point(alpha = 0.6, size = 2) +
        geom_smooth(method = "lm", color = "firebrick", se = TRUE, ...)
}

#run ggpairs plot

ggplot <- ggpairs(
   d,
    columns = c("percent_occurrence", "mean_chla"),
    columnLabels = c("percent_occurrence", "Chl-a (mg/m³)"),
    mapping = aes(color = site, alpha = 0.7),
    lower = list(continuous = custom_smooth),
    diag = list(continuous = wrap("densityDiag", alpha = 0.4)),
    upper = list(continuous = wrap("cor", size = 3.5))
) #+
    # theme_bw() +
    # labs(
    #     title = "North Atlantic Right Whale Occurrence vs. Chl-a (2014–2017)",
    #     subtitle = "8-Day Binned Data | Pairwise NA Removal Applied"
    # ) +
    # theme(
    #     plot.title = element_text(face = "bold", size = 14),
    #     strip.text = element_text(face = "bold")
    # )

print(ggplot)

cor.test(d$percent_occurrence, d$mean_chla, method = "spearman")


# Aggregating data to weekly, biweekly, and monthly timescales (DOPPIO ROMS DATA)

# function to create new dataframes for range of time scales with means for env variables
resample_env <- function(df, scale = "month") {
    df %>%
        # Round timestamps down to the specified time unit
        mutate(period = floor_date(Date, unit = scale)) %>%
        # Group by the new time period
        group_by(period, Site) %>%
        # Calculate the mean for all numeric columns (ignoring NAs)
        summarise(
            across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop"
        )
}

env_weekly <- resample_env(num_env_vars, scale = "week")
env_weekly <- num_env_vars %>%
    # week_start = 1 sets Monday as the start of the week
    mutate(period = floor_date(Date, unit = "week", week_start = 1)) %>%
    group_by(period, Site) %>%
    summarise(
        across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
    )

write.csv(env_weekly, "Env_weekly.csv", row.names = FALSE)

env_biweekly <- resample_env(num_env_vars, scale = "14 days")

anchor <- as.Date("2014-11-03")

env_biweekly <- num_env_vars %>%
    # Calculates 14-day chunks starting directly from your anchor date
    mutate(period = anchor + days((as.numeric(Date - anchor) %/% 14) * 14)) %>%
    group_by(period, Site) %>%
    summarise(
        across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
    )

write.csv(env_biweekly, "Env_biweekly.csv", row.names = FALSE)

env_monthly <- resample_env(num_env_vars, scale = "month")

write.csv(env_monthly, "Env_monthly.csv", row.names = FALSE)



# Load combined RW + env data on these scales -
RWenv_wkly <- read.csv("RWenv_weekly.csv")

RWenv_wkly <- RWenv_wkly %>%
    mutate(Date = mdy(Date)) %>%
    # Change Site, Month, and Period (1 = 2014-17, 2 = 2021-2024) to categorical
    mutate(device_type = as.factor(device_type),
           period = as.factor(period),
           Site = as.factor(Site),
    ) %>%
    # Sort by Site and Date
    arrange(Site, Date)

RWenv_biwkly <- read.csv("RWenv_biweekly.csv")

RWenv_biwkly <- RWenv_biwkly %>%
    mutate(Date = mdy(Date)) %>%
    # Change Site, Month, and Period (1 = 2014-17, 2 = 2021-2024) to categorical
    mutate(device_type = as.factor(device_type),
           period = as.factor(period),
           Site = as.factor(Site),
    ) %>%
    # Sort by Site and Date
    arrange(Site, Date)

RWenv_mnthly <- read.csv("RWenv_monthly.csv")

RWenv_mnthly <- RWenv_mnthly %>%
    mutate(Date = mdy(Date)) %>%
    mutate(Month = as.numeric(format(Date, "%m"))) %>%
    # Change Site, Month, and Period (1 = 2014-17, 2 = 2021-2024) to categorical
    mutate(MonthCat = as.factor(Month),
           device_type = as.factor(device_type),
           period = as.factor(period),
           Site = as.factor(Site),
           dist_200m_km = as.integer(dist_200m_km)
    ) %>%
    # Sort by Site and Date
    arrange(Site, Date)


# GGpairs for pairwise grid - assess multicollinearity

# Scatter plots on one half, density distributions on the diagonal,
# and correlation coefficients (with significance levels) other half
# correlation values (r) > 0.7-0.8 between two variables = highly collinear

# WEEKLY
ggpairs(RWenv_wkly,
        title = "Weekly Pairwise Correlations",
        upper = list(continuous = wrap("cor", size = 4.5)),
        lower = list(continuous = wrap("points", alpha = 0.6, size = 1))) +
    theme_minimal()

# Histogram of response variable
hist(RWenv_wkly$percent_occurrence)
# zero inflated distribution, effect which families might be good options
mean(RWenv_wkly$percent_occurrence == 0) # proportion of 0s
sort(RWenv_wkly$percent_occurrence)

# Boxplots of Occurrence by different categorical variables
p1 <- ggplot(RWenv_wkly, aes(x = Site, y = percent_occurrence)) +
    geom_boxplot(fill = "lightblue")
p2 <- ggplot(RWenv_wkly, aes(x = period, y = percent_occurrence)) +
    geom_boxplot(fill = "lightblue")
p3 <- ggplot(RWenv_wkly, aes(x = device_type, y = percent_occurrence)) +
    geom_boxplot(fill = "lightblue")
# Combine with patchwork
(p1 | p2 | p3) +
    plot_annotation(title = "Weekly occurrence by different categorical variables")

# BI WEEKLY
ggpairs(RWenv_biwkly,
        title = "Biweekly Pairwise Correlations",
        upper = list(continuous = wrap("cor", size = 4.5)),
        lower = list(continuous = wrap("points", alpha = 0.6, size = 1))) +
    theme_minimal()

# Boxplots of Occurrence by different categorical variables
p1 <- ggplot(RWenv_biwkly, aes(x = Site, y = percent_occurrence)) +
    geom_boxplot(fill = "lightblue")
p2 <- ggplot(RWenv_biwkly, aes(x = period, y = percent_occurrence)) +
    geom_boxplot(fill = "lightblue")
p3 <- ggplot(RWenv_biwkly, aes(x = device_type, y = percent_occurrence)) +
    geom_boxplot(fill = "lightblue")
# Combine with patchwork
(p1 | p2 | p3) +
    plot_annotation(title = "Biweekly occurrence by different categorical variables")

# MONTHLY
ggpairs(RWenv_mnthly,
        title = "Monthly Pairwise Correlations",
        upper = list(continuous = wrap("cor", size = 4.5)),
        lower = list(continuous = wrap("points", alpha = 0.6, size = 1))) +
    theme_minimal()

# Boxplots of Occurrence by different categorical variables
p1 <- ggplot(RWenv_mnthly, aes(x = Site, y = percent_occurrence)) +
    geom_boxplot(fill = "lightblue")
p2 <- ggplot(RWenv_mnthly, aes(x = as.factor(month), y = percent_occurrence)) +
    geom_boxplot(fill = "lightblue")
p3 <- ggplot(RWenv_mnthly, aes(x = period, y = percent_occurrence)) +
    geom_boxplot(fill = "lightblue")
p4 <- ggplot(RWenv_mnthly, aes(x = device_type, y = percent_occurrence)) +
    geom_boxplot(fill = "lightblue")
p5 <- ggplot(RWenv_mnthly, aes(x = as.factor(dist_200m_km), y = percent_occurrence)) +
    geom_boxplot(fill = "lightblue")
# Combine with patchwork
(p1 | p2) / (p3 | p4 | p5) +
    plot_annotation(title = "Monthly occurrence by different categorical variables")





