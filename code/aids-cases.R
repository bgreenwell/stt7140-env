y <- c(12, 14, 33, 50, 67, 74, 123, 141, 165, 204, 253, 246, 240)
t <- 1:13
plot(t + 1980, y, xlab = "Year", ylab = "# of new AIDS cases", 
     ylim = c(0, 280), pch = 19, col = "dodgerblue2", las = 1)

# Simple model
m0 <- glm(y ~ t, family = poisson(link = "log"))
summary(m0)  # print model summary

new.t <- seq(1, 13, length = 100)
fv <- predict(m0, data.frame(t = new.t), se = TRUE)
plot(t + 1980, y, xlab = "Year", ylab = "# of new AIDS cases", ylim = c(0, 280))
lines(new.t + 1980, exp(fv$fit))

# Standard residual plots
par(mfrow = c(1, 2))
plot(m0, which = 1:2)

# More flexible model
m1 <- glm(y ~ t + I(t ^ 2), family = poisson(link = "log"))
summary(m1)  # print model summary

# Standard residual plots
par(mfrow = c(1, 2))
plot(m1, which = 1:2)

# Compare models
anova(m0, m1, test = "Chisq")

beta.1 <- summary(m1)$coefficients[2,]
ci <- c(beta.1[1] - 1.96*beta.1[2], beta.1[1] + 1.96*beta.1[2])
ci ## print 95% CI for beta_1

# Plot fitted mean response
par(mfrow = c(1, 1))
new.t <- seq(1, 13, length = 100)
fv <- predict(m1, data.frame(t = new.t), se = TRUE)
plot(t + 1980, y, xlab = "Year", ylab = "# of new AIDS cases", ylim = c(0, 280))
lines(new.t + 1980, exp(fv$fit))
lines(new.t + 1980, exp(fv$fit + 2*fv$se.fit), lty = 2)
lines(new.t + 1980, exp(fv$fit - 2*fv$se.fit), lty = 2)
