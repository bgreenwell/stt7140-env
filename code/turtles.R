# Install required packages
# install.packages("investr", "RColorBrewer")

# Load required packages
library(investr)

# Better colors
set1 <- RColorBrewer::brewer.pal(9, "Set1")

# Load the data
turtles <- read.table("turtles", header = TRUE)

# Scatterplot
plot(
  formula = clutch ~ length, 
  data = turtles,
  xlab = "Carapace length (mm)",
  ylab = "Number of eggs",
  pch = 19,
  col = set1[2L]
)

# Fit increasingly complex polynomial regression models
fit1 <- lm(clutch ~ length, data = turtles)
fit2 <- lm(clutch ~ length + I(length ^ 2), data = turtles)
fit3 <- lm(clutch ~ length + I(length ^ 2) + I(length ^ 3), data = turtles)
fit4 <- lm(clutch ~ poly(length, degree = 4, raw = TRUE), data = turtles)

# Plot fitted models
par(mfrow = c(2, 2))
plotFit(fit1, xlab = "Carapace length (mm)", ylab = "Number of eggs",
        pch = 19, col = set1[2L], interval = "prediction")
plotFit(fit2, xlab = "Carapace length (mm)", ylab = "Number of eggs",
        pch = 19, col = set1[2L], interval = "prediction")
plotFit(fit3, xlab = "Carapace length (mm)", ylab = "Number of eggs",
        pch = 19, col = set1[2L], interval = "prediction")
plotFit(fit4, xlab = "Carapace length (mm)", ylab = "Number of eggs",
        pch = 19, col = set1[2L], interval = "prediction")

# Be cautious when using polynomial regression
plotFit(lm(clutch ~ poly(length, degree = 17, raw = TRUE), data = turtles), 
        xlab = "Carapace length (mm)", ylab = "Number of eggs",
        pch = 19, col = set1[2L], interval = "prediction")

# Print model summaries
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

# Residual plots
par(mfrow = c(2, 2))
plot(fit1, which = 1L:2L)
plot(fit2, which = 1L:2L)

# Compare models
anova(fit2, fit4)
