# Load required packages
library(ggplot2)  # install.packages("ggplot2")

# Data from WSU grad student John Brooker (1997) from Biological Sciences investigating
# the effect of thermol pollution on growth of Corbicula Fluminea (asiatic clam).
# Clams were collected from 3 sites:
#
#   1 = intake site
#
#   2 = discharge site
#
#   3 = site near Interstate 55
#
# Load the data
url <- paste0("https://raw.githubusercontent.com/bgreenwell/",
              "stt7140-env/master/datasets/clams.csv")
clams <- read.delim(url, header = TRUE, sep = "")  

# Only consider intake and discharge sites
clams <- clams[clams$site %in% c(1, 2), c("length", "site")]

# Relabel site variable
clams$site <- ifelse(clams$site == 1, yes = "intake", no = "discharge")

# Sanity check
head(clams, n = 10)

# Boxplots
boxplot(length ~ site, data = clams)

# Histograms
ggplot(clams, aes(x = length, fill = site)) +
  geom_histogram(binwidth = 0.25, alpha = 0.5, color = "black") +
  theme_light()


# t-test (the usual approach) --------------------------------------------------

# Carry out t-test via the usual approach
t.test(length ~ site, data = clams)


# t-test (linear model) --------------------------------------------------------

# Carry out t-test via a linear model
mod <- lm(length ~ site, data = clams)
summary(mod)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(mod)
