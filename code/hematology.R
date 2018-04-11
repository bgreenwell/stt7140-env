# Hematologic data for 9 patients with aplastic anemia. The independent variable
# is the % of reticulocytes and the number of lymphocytes (per mm^2)
reticulocytes <- c(3.6, 2.0, 0.3, 0.3, 0.2, 3.0, 0.0, 1.0, 2.2)
lymphocytes <- c(1700, 3078, 1820, 2706, 2086, 2299, 676, 2088, 2013)

# Scatterplot of the data
plot(reticulocytes, lymphocytes, xlab = "Reticulocytes (%)",
     ylab = "Lymphocytes (per mm^2)", pch = 19)

# Fit a simple linear regression model
fit <- lm(lymphocytes ~ reticulocytes)

# Print a summary of the fitted model
summary(fit)

# Add the fitted regression line to the previous scatterplot
plot(reticulocytes, lymphocytes, xlab = "Reticulocytes (%)",
     ylab = "Lymphocytes (per mm^2)", pch = 19)
abline(lm(lymphocytes ~ reticulocytes), col = "red", lwd = 2)

# Diagnostic plots
par(mfrow = c(1, 2))
plot(fit, which = 1:2, pch = 19)

# 95% confidence intervals for the intercept and slope parameters
confint(fit, level = 0.95)
