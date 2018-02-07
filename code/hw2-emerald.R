################################################################################
# Load the data into R
################################################################################
url <- paste0("https://raw.githubusercontent.com/bgreenwell/",
              "stt7140-env/master/datasets/emerald-ash-borer")
trees <- scan(url)
head(trees)  # look at first six observations


################################################################################
# Obtain a simple random sample of size n = 20
################################################################################
set.seed(101)
trees_sample <- sample(trees, size = 50, replace = FALSE)

# ALWAYS PLOT YOUR DATA!
barplot(table(trees_sample))


################################################################################
# Compute the sample proportion
################################################################################
mean(trees_sample)  # sample proportion
n <- 50
N <- length(trees)  # N = 2000
zval <- qnorm(0.975)
