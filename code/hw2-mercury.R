################################################################################
# Load the data into R
################################################################################
url <- paste0("https://raw.githubusercontent.com/bgreenwell/",
              "stt7140-env/master/datasets/mercury-in-eggs")
hg <- scan(url)
head(hg)  # look at first six observations


################################################################################
# Obtain a simple random sample of size n = 20
################################################################################
set.seed(101)
hg_sample <- sample(hg, size = 20, replace = FALSE)

# ALWAYS PLOT YOUR DATA!
par(mfrow = c(1, 3))  # setup for three side-by-side plots
boxplot(hg_sample)    # boxplot
hist(hg_sample)       # histogram
qqnorm(hg_sample)     # Q-Q plot (to help assess normality assumption)
qqline(hg_sample)     # add reference line to Q-Q plot


################################################################################
# Compute the sample mean and sample standard deviation
################################################################################
mean(hg)
sd(hg)
n <- 20
N <- 1000
tval <- qt(0.975, df = 19)
