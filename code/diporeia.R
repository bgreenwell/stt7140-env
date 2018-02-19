# The population of the shrimp-like crustacean called Diporeia has been 
# virtually wiped out in large areas of the lakes. The cause of this elimination 
# is thought to be due to the relatively recent introduction of the Zebra mussel 
# into these lakes. In studying this problem Nalepa et al. (2000) examine the 
# effect of the depth (in meters) of water in southern Lake Michigan where the 
# Diporeia were found on the dry weight (in mg) of this crustacean. 

# The data
depth <- c(35, 35, 38, 45, 50, 52, 54, 62, 75, 75, 95)
weight <- c(0.59, 0.66, 0.82, 0.63, 0.60, 0.68, 0.62, 0.47, 0.40, 0.50, 0.49)

# Scatterplot
plot(weight ~ depth, 
     pch = 19, 
     xlab = "Depth (m)",
     ylab = "Weight (mg)")

# Simple correlation
cor(depth, weight)

# Fit an SLR model
slr <- lm(weight ~ depth)
abline(slr)

# Print model summary
summary(slr)

# Diagnostic plots
par(mfrow = c(3, 2))
plot(slr, which = 1L:6L)

# Predicted value
predict(slr, newdata = data.frame(depth = 40))

# Confidence interval for the mean response
predict(slr, newdata = data.frame(depth = 40), interval = "confidence")

# Prediction interval for a single new response
predict(slr, newdata = data.frame(depth = 40), interval = "prediction")
