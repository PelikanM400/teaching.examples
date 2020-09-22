# traditional and bayesian t test: a data simulation 
###########################################################



# preparations control analysis 
###########################################################
# set working dir 
setwd("DIR ON YOUR COMPUTER")



# load necessary libraries 
if(!require("rjags")) install.packages("rjags")
if(!require("R2WinBUGS")) install.packages("R2WinBUGS")
if(!require("bayesplot")) install.packages("bayesplot")



# generate hypothesis data 
###########################################################
set.seed(2)
control <- rnorm(100, 140, 10)
control
set.seed(3)
intervention <- rnorm(100, 130, 10)
intervention



# descriptive analysis 
##########################################################
hist(control)
quantile(control)
mean(control)
sd(control)
hist(intervention)
quantile(intervention)
mean(intervention)
sd(intervention)



# traditional analysis: frequency methods 
##########################################################
mean(control) - mean(intervention)
t.test(control, intervention, paired = FALSE)
result.unpaired.t <- c(t.test(control, intervention, paired = FALSE)[["conf.int"]][1], mean(control) - mean(intervention), t.test(control, intervention, paired = FALSE)[["conf.int"]][2])
result.unpaired.t



# bayesian method: unpaired
##########################################################
# set initials 
inits <- function() {
  list(tau.intervention = 0.001)
  list(tau.intervention = 0.01)
  list(tau.intervention = 0.1)
}
inits

# import data for BUGS 
ls.model <- list("control" = control, "intervention" = intervention, "count" = length(control))
ls.model

# build the model 
bugs.model <- function() { # build the model 
  for (i in 1 : count) {
    intervention[i] ~ dnorm(mean.intervention[i], tau.intervention); 
    mean.intervention[i] <- control[i] - d; 
  } 
  d ~ dnorm(0, 0.0001); 
  tau.intervention ~ dgamma(0.001, 0.001); 
}
write.model(bugs.model, "simulation.bayesian.t.bug")

# run the model 
results.bugs <- jags.model(file = "simulation.bayesian.t.bug", 
                           data = ls.model, 
                           inits = inits, 
                           n.chains = 3, 
                           n.adapt = 10000)
update(results.bugs, 20000)
para <- coda.samples(results.bugs, c("d"), n.iter = 10000)
summary(para)
gelman.diag(para)
result.bugs <- c(summary(para)[["quantiles"]][1], summary(para)[["quantiles"]][3], summary(para)[["quantiles"]][5])
result.bugs



# comparing the results 
###########################################################
result.unpaired.t
result.bugs



# ploting the results 
###########################################################
mcmc_hist(para, binwidth = 0.05)


