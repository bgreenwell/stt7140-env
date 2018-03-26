# Inbreeding occurs naturally within plant populations. A study was conducted to 
# study the effect of plant inbreeding on the resistance and tolerance of the 
# plant to native herbivores in Napa County, California using the plant Yellow 
# Monkeyflower. The response variable y of interest in this study was whether or
# not the plant produced flowers (0 for no and 1 for yes). Flower production is 
# needed for reproduction. The primary explanatory variable of interest was the 
# indicator variable indicating whether the plant was inbred (value of 1) or 
# cross-bred (with a value of 0). Two other covariates were also recorded: 
# Herbivore damage to the plants due to spittlebugs, adult and larval beetles, 
# slugs, deer, etc. was recorded as a percentage on a discretized scale (12 
# categories); and the dried above ground biomass of the plant (in grams).

# Load the data
inbreeding <- read.table(
  file = "/Users/bgreenwell/Dropbox/teaching/stt7140-env/data/inbreeding.dat",
  col.names = c("damage", "inbred", "y", "biomass")
)
head(inbreeding)  # print first few observations

# Scatterplot matrix
pairs(inbreeding, pch = 19, col = adjustcolor("dodgerblue2", alpha.f = 0.5))

# Fit a logistic regression model
fit <- glm(
  formula = y ~ as.factor(inbred) + biomass + damage, 
  data = inbreeding,
  family = binomial(link = "logit")
)
summary(fit)  # print model summary

# Fit a logistic regression model
fit2 <- update(fit, y ~ biomass)
anova(fit2, fit, test = "Chisq")  # chi-squared test comparing models

# Print summary of final model
summary(fit2)

# Plot fitted model
investr::plotFit(fit2, las = 1)
