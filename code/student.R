# Load the Ohio county data
students <- read.csv("../datasets/students.csv")

# Print the first 10 rows
head(students, n = 10)

# Reformat the data
counts <- students$num_students
names(counts) <- students$county

# True total
true_total <- sum(counts)

# Draw a histogram of the population
hist(counts, breaks = 20, freq = FALSE)
lines(density(counts), lwd = 1, col = "red2")  # add a nonparametric density

# Function to compute total
est_total <- function(x, N = nrow(students)) {
  N * mean(x)
}

# Function to simulate estimated totals
sim_totals <- function(n, nsim = 10000) {
  replicate(nsim, {
    x <- sample(counts, size = n, replace = FALSE)
    est_total(x)
  })
}

# Simulate estimated totals
total_5 <- sim_totals(n = 5)
total_10 <- sim_totals(n = 10)
total_25 <- sim_totals(n = 25)
total_50 <- sim_totals(n = 50)

# Plot sampling distributions
par(mfrow = c(2, 2))
hist(total_5, breaks = 50)
abline(v = true_total, col = "red2", lwd = 2)
hist(total_10, breaks = 50)
abline(v = true_total, col = "red2", lwd = 2)
hist(total_25, breaks = 50)
abline(v = true_total, col = "red2", lwd = 2)
hist(total_50, breaks = 50)
abline(v = true_total, col = "red2", lwd = 2)