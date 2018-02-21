# Source: http://lib.stat.cmu.edu/DASL/Datafiles/MercuryinBass.html
#
# Largemouth bass were studied in 53 different Florida lakes to examine the 
# factors that influence the level of mercury contamination. Water samples were 
# collected from the surface of the middle of each lake in August 1990 and then 
# again in March 1991. The pH level, the amount of chlorophyll, calcium, and 
# alkalinity were measured in each sample. The average of the August and March 
# values were used in the analysis. Next, a sample of fish was taken from each 
# lake with sample sizes ranging from 4 to 44 fish. The age of each fish and 
# mercury concentration in the muscle tissue was measured. (Note: Since fish 
# absorb mercury over time, older fish will tend to have higher concentrations). 
# Thus, to make a fair comparison of the fish in different lakes, the 
# investigators used a regression estimate of the expected mercury concentration 
# in a three year old fish as the standardized value for each lake. Finally, in 
# 10 of the 53 lakes, the age of the individual fish could not be determined and 
# the average mercury concentration ofthe sampled fish was used instead of the 
# standardized value.

# Install required packages
# install.packages("car", "corrplot")

# Load the data
url <- paste0("https://raw.githubusercontent.com/bgreenwell/",
              "eesR/master/R/Data/HgBass.txt")
hg_bass <- read.table(url, header = TRUE)

# Scatterplot matrix (histograms would be useful too!)
pairs(hg_bass[, c(3L:7L)], col = adjustcolor("purple", alpha.f = 0.5), pch = 19)

# A better scatterplot matrix
car::spm(
  ~ Avg.Mercury + Alkalinity + pH + Calcium + Chlorophyll, 
  data = hg_bass
)

# An even better scatterplot matrix
car::spm(
  ~ log(Avg.Mercury) + log(Alkalinity) + pH + log(Calcium) + log(Chlorophyll), 
  data = hg_bass,
  smoother = FALSE, 
  lwd = 3,
  col = "purple",
  pch = 1
)

# Correlation matrix (and plots thereof)
cor_mat <- cor(hg_bass[, c(3L:7L)])
heatmap(cor_mat)
corrplot::corrplot(
  cor_mat, 
  method = "circle",
  type = "full",
  diag = TRUE,
  order = "hclust"
)

# Full model
full_model <- lm(
  log(Avg.Mercury) ~ log(Alkalinity) + pH + log(Calcium) + log(Chlorophyll), 
  data = hg_bass
)

# Print model summary
summary(full_model)

# Reduced model
reduced_model <- lm(
  log(Avg.Mercury) ~ log(Alkalinity) + log(Chlorophyll), 
  data = hg_bass
)

# Partial F-test
anova(reduced_model, full_model)

# Using R-squared formula
full_R2 <- summary(full_model)$r.squared
reduced_R2 <- summary(reduced_model)$r.squared
full_df <- nobs(full_model) - length(coef(full_model)) 
reduced_df <- nobs(reduced_model) - length(coef(reduced_model)) 
F_obs <- ((full_R2 - reduced_R2) / (reduced_df - full_df)) / 
  ((1 - full_R2) / full_df)  # F statistic
1 - pf(F_obs, df1 = reduced_df - full_df, df2 = reduced_df)  # p-value

# Stepwise selection
stepwise <- step(full_model, direction = "backward")

# We can also test individual coefficients this way
anova(  # compare to t-test from summary() function!
  lm(log(Avg.Mercury) ~ log(Alkalinity), data = hg_bass),
  lm(log(Avg.Mercury) ~ log(Alkalinity) + log(Chlorophyll), data = hg_bass)
)
