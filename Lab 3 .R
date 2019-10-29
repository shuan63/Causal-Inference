setwd('X')

# Understanding the sampling distribution ---------------------------------

data <- X('stool_heights.csv', stringsAsFactors = FALSE)
data

# Explore the data
names(data)
hist(data$po.nostool, breaks = 10)

# Calculate the true ATE
true.effect <- 8
data$po.wstool <- data$po.nostool + true.effect 

mean(data$po.nostool)
mean(data$po.wstool)

mean(data$po.wstool) - mean(data$po.nostool) # True ATE


# What if we did an experiment?
set.seed(321)
num.people <- nrow(data)

treatment <- sample(rep(c(1,0), num.people/2))
treatment

outcome <- ifelse(treatment == 1, data$po.wstool, data$po.nostool)

mean(outcome[treatment == 1]) - mean(outcome[treatment == 0])


# What if we had gotten a different randomization?
treatment <- sample(rep(c(1,0), num.people/2))
outcome <- ifelse(treatment == 1, data$po.wstool, data$po.nostool)
mean(outcome[treatment == 1]) - mean(outcome[treatment == 0])


# Write our own function that "runs one experiment"
rand.est.ate <- function() {
  treatment <- sample(rep(c(1,0), num.people/2))
  outcome <- ifelse(treatment == 1, data$po.wstool, data$po.nostool)
  return(mean(outcome[treatment == 1]) - mean(outcome[treatment == 0]))
}
rand.est.ate()
rand.est.ate()
rand.est.ate()


# Samping distribution
samp.dist <- replicate(10000, rand.est.ate())
hist(samp.dist, breaks = 100, col = rgb(0,0,1,0.5))
abline(v=true.effect, lwd = 6)


dev.off()

# Switching gears to what we do in practice --------------------------------------------

#The above assumes we can see all the POs...
#but we can't, and we'll only have 1 estimate.
#What can we do with that?

# First, let's create the data we would have from a particular experiment.
set.seed(246)
actual.treatment <- sample(rep(c(1,0), num.people/2)) #Single randomization
outcome <- ifelse(actual.treatment == 1, data$po.wstool, data$po.nostool) #Leads to single set of observed values.
obsdata <- data.frame(data$fullname, actual.treatment, outcome) #Observed data does not contain potential outcomes.
obsdata

#And we can estimate a single ATE:
our.ate <- mean(obsdata$outcome[obsdata$actual.treatment == 1]) -
  mean(obsdata$outcome[obsdata$actual.treatment == 0])
our.ate


# Our goal is to prove the skeptic wrong. So let's assume the skeptic is right.
# How likely would we be to see this effect estimate by chance?
obsdata$outcome

rand.est.ate <- function(outcomes) {
  some.fake.treatment.for.jonty <- sample(rep(c(1,0), num.people/2))
  return(mean(outcomes[some.fake.treatment.for.jonty == 1]) - 
           mean(outcomes[some.fake.treatment.for.jonty == 0]))
}
rand.est.ate(obsdata$outcome)

distribution.under.sharp.null <- replicate(100000, rand.est.ate(obsdata$outcome))
distribution.under.sharp.null


#Compare our single ATE estimate against this distribution
hist(distribution.under.sharp.null, breaks = 100, col = rgb(1,0,0,0.5))
abline(v = our.ate, lwd = 3, col="red")

#What's the probability we would have gotten a value that high under the null hypothesis?
mean(distribution.under.sharp.null >= our.ate)


dev.off()


# Standard errors and confidence intervals ----------------------------------------------------
num.people <- 100
po.nostool <- round(runif(X, min = X, max = X))
po.wstool <- po.nostool + 8
treatment <- sample(rep(c(1,0), X))
outcome <- ifelse(treatment == 1, X, X)
treatment
outcome

est.ate.rand <- function(treatment) mean(po.wstool[X]) - mean(po.nostool[X])
samp.dist.100 <- replicate(X, est.ate.rand(sample(rep(c(1,0), num.people/2))))
X(X, breaks = 100, col = rgb(0,0,1,0.5))
abline(v=8, lwd = 3)
X(samp.dist.100)

# One standard error:
abline(v = 8 + sd(samp.dist.100), lwd = 3, col = 'red')
abline(v = 8 - sd(samp.dist.100), lwd = 3, col = 'red')

# Confidence interval
abline(v = 8 + X, lwd = 3, col = 'blue')
abline(v = 8 - X, lwd = 3, col = 'blue')

# 95% of the time, estimate within this range
mean(samp.dist.100 > 8 - X & samp.dist.100 < 8 + X)




