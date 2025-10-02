# Install if you haven't already: install.packages(c("ggplot2", "lubridate"))
library(ggplot2)
library(lubridate)
library(dplyr) # Recommended for general data handling

# 1. Load the transformed data
# Make sure 'deployment_periods.csv' is in your R Studio working directory.
data <- read.csv("DataRaw/deployment_periods.csv")

# 2. Ensure date columns are recognized as dates
data$Start_Date <- as.Date(data$Start_Date)
data$End_Date <- as.Date(data$End_Date)

# 3. Define Site order (optional, but good for control)
# You can customize this order as needed
site_order <- c("A-2M", "A-7M", "RTWB", "T-2M") # Example order
data$Site <- factor(data$Site, levels = site_order)

# 4. Define colors and linewidths for your specific devices
# Your devices are MARU, Rockhopper, and DMON.
device_colors <- c("MARU" = "#377eb8",       # Blue
                   "Rockhopper" = "#ff7f00", # Orange
                   "DMON" = "#4daf4a")       # Green

device_linewidths <- c("MARU" = 2,
                       "Rockhopper" = 2,
                       "DMON" = 2)

# Define the new axis limits
start_date_limit <- as.Date("2014-10-31") # November 2014
end_date_limit <- as.Date("2025-01-01")   # End of December 2024

# 6. Create the plot
ggplot(data, aes(y = Site, color = Recording_Device)) +
    # Draw the deployment segments
    geom_segment(aes(x = Start_Date, xend = End_Date, yend = Site, linewidth = Recording_Device),
                 lineend = "round") +

    # Apply custom colors and linewidths
    scale_color_manual(values = device_colors) +
    scale_linewidth_manual(values = device_linewidths, guide = "none") + # Hide linewidth legend

    # Configure the X-axis (Date)
    scale_x_date(date_breaks = "6 months",
                 date_labels = "%b %Y",
                 # === ADJUSTED X-AXIS LIMITS ===
                 limits = c(start_date_limit, end_date_limit)) +

    # Theme and Labels
    theme_minimal() +
    theme(
        # Text and Legend Formatting
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "right",
        legend.title = element_text(size = 12),

        # === AXIS LINE AND BORDER CONTROL ===
        # 1. Show the X and Y axis lines
        axis.line.x = element_line(colour = "black", linewidth = 0.5),
        axis.line.y = element_line(colour = "black", linewidth = 0.5),

        # 2. Hide the tick marks
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),

        # 3. Remove the panel border (which created the box)
        panel.border = element_blank(),

        # 4. Remove all grid lines (vertical and horizontal)
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
    ) +
    labs(
        x = "Recording Date",
        y = "Site",
        color = "Recording Device",
        #title = "Acoustic Monitoring Deployment Effort"
    )
