rm(list = ls())

# Data loading and modification
# library(tidyverse)
library(dplyr)
# library(scales)

# Visualization
library(ggplot2)
theme_set(theme_light())
library(patchwork)
library(GGally)

# Modeling
library(gamlss)
# library(gamlss.tr)
library(gamlss.add)
# library(mgcv)


# Load data ----

RWocc0 <- read.csv("DataRaw/WhaleOccurrence2014_2024_091825.csv")
RWocc <- RWocc0 %>%
    mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d")) %>%
    mutate(Month = as.numeric(format(Date, "%m"))) %>%
    # Change Site, Month, and Period (1 = 2014-17, 2 = 2021-2024) to categorical
    mutate(MonthCat = as.factor(Month),
           Period = as.factor(Period),
           Site = as.factor(Site)) %>%
    # Create lagged values of PercentOccurrence, by Site
    # group_by(Site) %>%
    # mutate(PercentOccurrence_l1 = dplyr::lag(PercentOccurrence, 1, order_by = Date),
    #        PercentOccurrence_l2 = dplyr::lag(PercentOccurrence, 2, order_by = Date)) %>%
    # ungroup() %>%
    # Sort by Site and Date
    arrange(Site, Date)

# Change year to categorical
RWocc$YearCat <- as.factor(RWocc$Year)
RWocc$YearCat <- relevel(RWocc$YearCat, "2015") #relevel for intercept to be 2015 not 2014


# Exploratory analysis ----

# Sample sizes for different combinations
table(RWocc$Year, RWocc$Site)
table(RWocc$DeviceType, RWocc$Period)

# Pairs plot, matrix pairwise relationships of all data
GGally::ggpairs(RWocc %>% dplyr::relocate(PercentOccurrence, .after = last_col()))

# Histogram of response variable
hist(RWocc$PercentOccurrence)
# zero inflated distribution, effect which families might be good options
mean(RWocc$PercentOccurrence == 0) # proportion of 0s
sort(RWocc$PercentOccurrence)

# Boxplots of PercentOccurrence by different categorical variables
p1 <- ggplot(RWocc, aes(x = as.factor(Year), y = PercentOccurrence)) +
    geom_boxplot(fill = "lightblue") +
    xlab("Year")
p2 <- ggplot(RWocc, aes(x = MonthCat, y = PercentOccurrence)) +
    geom_boxplot(fill = "lightblue")
p3 <- ggplot(RWocc, aes(x = Period, y = PercentOccurrence)) +
    geom_boxplot(fill = "lightblue")
p4 <- ggplot(RWocc, aes(x = DeviceType, y = PercentOccurrence)) +
    geom_boxplot(fill = "lightblue")
p5 <- ggplot(RWocc, aes(x = Site, y = PercentOccurrence)) +
    geom_boxplot(fill = "lightblue")
# Combine with patchwork
(p1 | p2) / (p3 | p4 | p5) +
    plot_annotation(title = "Percent occurrence by different categorical variables")

# Interaction plot
with(RWocc,
     interaction.plot(YearCat, DeviceType, PercentOccurrence)
)
with(RWocc,
     interaction.plot(YearCat, MonthCat, PercentOccurrence, col = MonthCat)
)


# Modeling ----
set.seed(123)
m_RWocc0 <- gamlss(PercentOccurrence ~ #pbc(Month, max.df = 5) +
                       # pb(Year, max.df = 5) +
                       scs(Month, control = cs.control(cv = FALSE)) +
                       #ga(~ s(Month, bs = "cc")) +
                       YearCat +
                       DeviceType +
                       scs(Month, control = cs.control(cv = FALSE))*Site
                   #MonthCat*YearCat
                   #Period +
                   #DeviceType*YearCat
                   # + Period
                   #+ Year:Period,
                   # random = ~1 | Dummy
                   # + PercentOccurrence_l1 + PercentOccurrence_l2, #correlation = corARMA(p = 1),
                   # ), # creates an autocorrelation structure
                   ,family = ZINBI,
                   control = gamlss.control(c.crit = 0.01, n.cyc = 100),
                   data = RWocc)
summary(m_RWocc0)

#

set.seed(123)
m_RWocc <- gamlss(PercentOccurrence / 100 ~
                      ga(~ti(Year, Month)) +
                      # scs(Month, df = 7, control = cs.control(cv = FALSE)) +
                      # ga(~s(Month, bs = "cp")) +
                      pb(Year) +
                      # YearCat + # seems like year as continuous is better
                      DeviceType
                   + pvc(Month, by = Site)
                  ,sigma.formula = ~pb(Month) #+ pb(Year)
                  ,family = BEINF, #ZINBI,
                  control = gamlss.control(c.crit = 0.01, n.cyc = 100),
                  data = RWocc)
summary(m_RWocc)
# Select model based on AIC
m_RWocc <- gamlss::stepGAIC(m_RWocc)

# Possible helper function to select distribution family
# gamlss::chooseDist(m_RWocc, type = "real0to1")

## Subset model ----
# 2025-10-07 Remove DeviceType after commments of Dave and Helen?
# Run a separate model with DeviceType on the subset of the data
# Model with DeviceType on subset of data from 2023 onward
# Because of the short subset, replace Year with as.factor(Year), and interaction with Month
set.seed(123)
m_RWocc_subs <- gamlss(PercentOccurrence / 100 ~
                      # ga(~ti(Year, Month)) +
                          as.factor(Year) *  as.factor(Month) +
                      scs(Month, df = 7, control = cs.control(cv = FALSE)) +
                      # ga(~s(Month, bs = "cp")) +
                      as.factor(Year) +
                  # YearCat + # seems like year as continuous is better
                  DeviceType
                  # pvc(Month, by = Site)
                  ,sigma.formula = ~pb(Month) #+ pb(Year)
                  ,family = BEINF, #ZINBI,
                  control = gamlss.control(c.crit = 0.01, n.cyc = 100),
                  data = RWocc %>% dplyr::filter(Year >= 2023))
# Select model based on AIC
m_RWocc_subs <- gamlss::stepGAIC(m_RWocc_subs)
summary(m_RWocc_subs)
# Conclusion about subset model:
# DeviceType is significant in this subset model
# The effect is comparable to the full model, so might continue using the full model.


## Model diagnostics ----
plot(m_RWocc)
plot(m_RWocc, ts = TRUE)
wp(m_RWocc)

## Term plots ----
# list all terms
modterms <- attr(terms(m_RWocc), "term.labels")

png("Figures/RWocc_GAMLSS_TermPlots.png", width = 9, height = 7, units = "in", res = 300)
par(mfrow = c(ceiling(length(modterms)/2), 2))
plot(getSmo(m_RWocc, what = "mu", which = 1), scheme = 2, las = 1)
for (i in 2:length(modterms)) {
    term.plot(m_RWocc, what = "mu", terms = modterms[i], las = 1)
}
dev.off()

# Reset plotting layout
par(mfrow = c(1, 1))
# If needed, the 2D plot in a different way
term.plot(m_RWocc, what = "mu", terms = "ga(~ti(Year, Month))",
          surface.gam = FALSE)

# Terms for pvc
# Get predicted values, see term.plot
# terms <- lpred(m_RWocc, what = "mu", type = "terms", se.fit = TRUE,
#       terms = "pvc(Month, by = Site)")
# The pvc is the term #3 in the model, see the object modterms (which = 3)
term1 <- getSmo(m_RWocc, what = "mu", which = 3)
# gamlss:::plot.pvc
source("./Code/plot.pvc.tailwinds.R")
# Default plotting
plot(term1, scheme = "lines")
d = plot.pvc.tailwinds(term1, scheme = "lines")
# Plot with ggplot for better handling of factors
ggplot(d, aes(x = x, y = fv)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
    labs(y = "Partial effect of Month", x = "Month") +
    facet_wrap(~ by) +
    # set the x-axis labels
    scale_x_continuous(breaks = seq(2, 12, by = 2))


# Terms for sigma
term.plot(m_RWocc, what = "sigma")

# Model summary ----
# although the ga() term doesn't show up in the summary, it is significant
summary(m_RWocc)

# Comment out the ga() term to see its effect
m_RWocc_noYM <- gamlss(PercentOccurrence / 100 ~
                           # ga(~ti(Year, Month)) +
                           scs(Month, df = 7, control = cs.control(cv = FALSE)) +
                           # ga(~s(Month, bs = "cp")) +
                           pb(Year) +
                           # YearCat + # seems like year as continuous is better
                           DeviceType
                       # pvc(Month, by = Site)
                       ,sigma.formula = ~pb(Month) #+ pb(Year)
                       ,family = BEINF, #ZINBI,
                       control = gamlss.control(c.crit = 0.01, n.cyc = 100),
                       data = RWocc)
LR.test(m_RWocc_noYM, m_RWocc)


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
