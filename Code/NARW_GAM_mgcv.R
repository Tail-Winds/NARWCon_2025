
library(mgcv)
set.seed(123)
m_RWocc_mgcv <- gamm(PercentOccurrence ~
                         s(Year, bs = "ts", k = 5) +
                         # DeviceType +
                         s(Month, bs = "cc", k = 7) +
                         ti(Year, Month, bs = c("ts", "cc"))
                     ,correlation = corARMA(p = 0, q = 1, form = ~ Date | Site)
                     ,
                     # The 'negbin' family accounts for overdispersion in count data.
                     # This is the closest analog in mgcv to the GAMLSS 'ZINBI' family,
                     # but it does NOT explicitly model the zero-inflation part.
                     family = nb,
                     data = RWocc,
                     method = "REML",
                     control = list(maxit = 100)
)

m_RWocc_mgcv <- gam(PercentOccurrence ~
                        # PercentOccurrence_l1 +
                         s(Year, bs = "ts", k = 5) +
                         # DeviceType +
                         s(Month, bs = "cc", k = 7) +
                         ti(Year, Month, bs = c("ts", "cc"))
                     # ,correlation = corAR1(form = ~ Date | Site)
                     ,
                     # The 'negbin' family accounts for overdispersion in count data.
                     # This is the closest analog in mgcv to the GAMLSS 'ZINBI' family,
                     # but it does NOT explicitly model the zero-inflation part.
                     family = nb,
                     data = RWocc,
                     method = "REML",
                     control = list(maxit = 100)
)

# Print the model summary
summary(m_RWocc_mgcv)
summary(m_RWocc_mgcv$lme)
summary(m_RWocc_mgcv$gam)

# Plot the smooth terms to visualize the effects
# ?plot.gam
plot(m_RWocc_mgcv$gam,
     rug = TRUE,
     shade = TRUE, shade.col = "lightblue",
     scheme = 2,
     pages = 1)

# Check model diagnostics
gam.check(m_RWocc_mgcv$gam)

# ?residuals.gam
# ?residuals.lme
r = residuals(m_RWocc_mgcv)
r = residuals(m_RWocc_mgcv$lme, type = "normalized")



plot(fitted(m_RWocc_mgcv$lme), r)
abline(h = 0, lty = 2)

acf(r)
shapiro.test(r)
# QQ-plot
stats::qqnorm(r)
stats::qqline(r)


library(gratia)
appraise(m_RWocc_mgcv)
draw(m_RWocc_mgcv)
library(DHARMa)
sim_res <- simulateResiduals(m_RWocc_mgcv)
plotSimulatedResiduals(sim_res)
# Check for autocorrelation in residuals
acf(residuals(m_RWocc_mgcv))
library(mgcViz)
v <- getViz(m_RWocc_mgcv)
plot(v)
# Check for concurvity (non-linear collinearity) among smooth terms
concurvity(m_RWocc_mgcv, full = TRUE)
# Check for concurvity (non-linear collinearity) among smooth terms
concurvity(m_RWocc_mgcv, full = FALSE)
# Compare AIC with other models if available
# AIC(m_RWocc_mgcv)
# Note: For a more comprehensive analysis, consider checking for zero-inflation



