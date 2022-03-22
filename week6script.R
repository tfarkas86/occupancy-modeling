dd <- read.csv("~/Dropbox/Occupancy Modelling/EAB_data.csv")

sink("~/Dropbox/Occupancy Modelling/eab_model.txt")
cat("model {

# Define likelihoods

for (i in 1:sites) {


  # true abundance
  
  enn[i] ~ dpois(lambda[i])
  log(lambda[i]) <- a.plot[plot[i]]
  
  # detection probability
  
  logit(p[i]) <- b0 + b.trap * trap[i]

  # observation
  
  for (j in 1:surveys) {

    y[i,j] ~ dbin(p[i], enn[i])

  }
}

## priors

for (i in 1:3) {
  a.plot[i] ~ dnorm(0, 0.001)
}

b0 ~ dnorm(0, 0.001)
b.trap ~ dnorm(0, 0.001)

}", fill=TRUE)
sink()

### JAGS time

library(R2jags)

dat <- dd[,-(1:4)]

jags.data <- list(sites=nrow(dat), surveys=ncol(dat), y=dat, trap=dd$trap.type,
                  plot=dd$plot.type)

inits <- function(){
  list(enn=apply(dat, 1, max))
}

params <- c("enn", "lambda", "p", "a.plot", "b0", "b.trap")

fit = jags(data=jags.data,
           model.file="~/Dropbox/Occupancy Modelling/eab_model.txt",
           inits = inits,
           parameters.to.save=params,
           n.burnin=1000,
           n.iter=5000,
           n.thin=2,
           n.chains=3)
