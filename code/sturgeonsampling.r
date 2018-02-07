## Use the simulated data on sturgeon fish from 3 lakes to illustrate
# and compare different sampling techniques: 
# Simple random sampling
# Stratified sampling and the
# Ratio estimator
#
# The data set simsturgeon.dat has 3 columns:
# Column 1: Strata for 3 different lakes
# Column 2: Length of the fish (cm)
# Column 3: mercury level in the fish (mg/g)
#

# Load required packages
library(dplyr)

# Load required data
simsturgeon <- read.table("datasets/simsturgeon.dat", header = TRUE)

# Population size
N <- nrow(simsturgeon)

# Sample size
n <- 100

# Compute size, mean, and standard deviation of mercury within each stratum, 
# then use theresults to compute proportional allocation
strata <- simsturgeon %>%
  group_by(lake) %>%
  summarize(N_h = n(), sigma_h = sd(mercury)) %>%
  mutate(n_pro = round(n * N_h / N)) %>%
  mutate(n_opt = round(n * (N_h * sigma_h) / sum(N_h * sigma_h)))

# Print results
strata



dat <- simsturgeon

lake1=dat[dat[,1]==1,]
lake2=dat[dat[,1]==2,]
lake3=dat[dat[,1]==3,]
N1=dim(lake1)[1]
N2=dim(lake2)[1]
N3=dim(lake3)[1]
N=N1+N2+N3
sigma1=sd(lake1[,3])
sigma2=sd(lake2[,3])
sigma3=sd(lake3[,3])

mu = mean(dat[,3]) # Population mean Hg levels
mu 
mux=mean(dat[,2]) # Population mean length for the fish
mux 

n=100 # sample size

# proportional allociation sample sizes
n1pro=round(n*N1/N)
n2pro=round(n*N2/N)
n3pro=n-n1pro-n2pro

# Optimal allocation sample sizes
n1opt=N1*sigma1
n2opt=N2*sigma2
n3opt=N3*sigma3
total=n1opt+n2opt+n3opt
n1opt=round(n*N1*sigma1/total)
n2opt=round(n*N2*sigma2/total)
n3opt=n-n1opt-n2opt

# Run a simulation to compare the different estimators
results=NULL
nsim=10000
for (isim in 1:nsim){
  # perform a simple random sample
   srs=mean(dat[sample(1:N, n, replace=F),3])

  # Stratified random sample using proportional allocation
   samp1=sample(1:N1, n1pro, replace=F)
   samp2=sample(1:N2, n2pro, replace=F)
   samp3=sample(1:N3, n3pro, replace=F)
   tau1=N1*mean(lake1[samp1,3])
   tau2=N2*mean(lake2[samp2,3])
   tau3=N3*mean(lake3[samp3,3])
   ybarpr=(tau1+tau2+tau3)/N 
 
 # optimal allocation  
  tau1=N1*mean(lake1[sample(1:N1, n1opt,replace=F),3])
  tau2=N2*mean(lake2[sample(1:N2, n2opt,replace=F),3])
  tau3=N3*mean(lake3[sample(1:N3, n3opt,replace=F),3])
  ybaropt=(tau1+tau2+tau3)/N

# Ratio Estimator
 rsamp=sample(1:N, n, replace=F)
 x=dat[rsamp,2]
 y=dat[rsamp,3]
 ybar=mean(y)
 xbar=mean(x)
 ybarratio=ybar/mean(x)*mux


results=rbind(results, c(srs, ybarpr, ybaropt, ybarratio))

}

apply(results,2,mean)
apply(results,2,sd)

plot(density(results[,1]), lwd=2, ylim=c(0,30),
 main="Sturgeon Fish Sampling Simulation", xlab="Mercury Level (mg/g)")
lines(density(results[,2]), lwd=2, col=2, lty=2)
lines(density(results[,3]), lwd=2, col=3, lty=3)
abline(v=mu, col=4, lwd=3)

legend("topleft", c("SRS Estimator", "Optimal Allocation", 
   "Proportional Allocation"),  
   col=c(1,2,3), lwd=c(2,2,2),
  lty=c(1,2,3))

windows()

plot(density(results[,1]), lwd=2, ylim=c(0,35),
 main="Sturgeon Fish Sampling: Ratio Estimator", xlab="Mercury Level (mg/g)")
lines(density(results[,4]), lwd=2, col=2, lty=2)
abline(v=mu, col=4, lwd=3)

legend("topright", c("SRS Estimator", "Ratio Estimator"),  
   col=c(1,2), lwd=c(2,2),
  lty=c(1,2))

windows()
# Plot Hg versus length
 plot(dat[,2],dat[,3], main="Sturgeon Fish: Mercury Level vs Length",
   ylab="Mercury Level (mg/g)", xlab="Length (cm)")


####

# Run a simulation to compare SRS and ratio estimator
results=NULL
nsim=1000
for (isim in 1:nsim){
  # perform a simple random sample
   srs=mean(dat[sample(1:N, n, replace=F),3])

 
# Ratio Estimator
 rsamp=sample(1:N, n/2, replace=F)
 x=dat[rsamp,2]
 y=dat[rsamp,3]
 ybar=mean(y)
 xbar=mean(x)
 ybarratio=ybar/mean(x)*mux


results=rbind(results, c(srs, ybarratio))

}

windows()
apply(results,2,sd)
plot(density(results[,1]), lwd=2, ylim=c(0,35),
 main="Sturgeon Fish Sampling: Ratio Estimator", xlab="Mercury Level (mg/g)")
lines(density(results[,2]), lwd=2, col=2, lty=2)
abline(v=mu, col=4, lwd=3)

legend("topright", c("SRS Estimator", "Ratio Estimator"),  
   col=c(1,2), lwd=c(2,2),
  lty=c(1,2))








