# load and format bobcat data

bob <- read.csv("~/Dropbox/Occupancy Modelling/bobcat.csv") # all data
bd <- bob[,(ncol(bob)-3):ncol(bob)] # observations
rd <- bob[,8:11]

# JAGS model

sink("~/Dropbox/Occupancy Modelling/bobcat_jags.txt")
cat("model {
# model structure
# define likelihoods

for(j in 1:sites) {

  z[j] ~ dbern(psi[j])
  logit(psi[j]) <- a0 + a_urban*urban[j] + a_wetland*wetland[j] + a_rabbit*rabbit[j]

  for (k in 1:survs) {

    y[j,k] ~ dbern(pz[j,k])
    pz[j,k] <- z[j] * p[j,k]
    logit(p[j,k]) <- b0 + b_urban*urban[j] + b_wetland*wetland[j] + 
                     b_rabbit*rabbit[j] + b_rain*rain[j,k]

  }
}

# define priors

a0 ~ dnorm(0, 0.001)
a_urban ~ dnorm(0, 0.001)
a_wetland ~ dnorm(0, 0.001)
a_rabbit ~ dnorm(0, 0.001)
b0 ~ dnorm(0, 0.001)
b_urban ~ dnorm(0, 0.001)
b_wetland ~ dnorm(0, 0.001)
b_rabbit ~ dnorm(0, 0.001)
b_rain ~ dnorm(0, 0.001)

    }", fill=TRUE)
sink()

# JAGS time

library(R2jags)

# scale the variables 

s.urb <- scale(bob$urban)[,1]
s.wet <- scale(bob$wetland)[,1]
s.rab <- scale(bob$rabbit)[,1]

# make jags list

jags.data <- list(y=bd, sites=nrow(bd), survs=ncol(rd), urban=s.urb, 
                  wetland=s.wet, rabbit=s.rab, rain=rd)

inits <- function() list(z=apply(bd, 1, max)) # inital values for z

params <- c("a0", "a_urban", "a_wetland", "a_rabbit", "b0", "b_urban", "b_wetland", 
            "b_rabbit", "b_rain")

fit <- jags(data=jags.data, 
            model.file="~/Dropbox/Occupancy Modelling/bobcat_jags.txt",
            parameters.to.save=params,
            inits=inits, 
            n.chains=3, 
            n.iter=3000, 
            n.burnin=1000, 
            n.thin=2)

post.wet <- fit$BUGSoutput$sims.list$a_wetland # get posterior for wetland
wet_ecdf <- ecdf(post.wet) # p.wet = 0.093

pocc <- plogis(.217) + plogis(1.479)*mean(s.rab) + plogis(-1.533)*.75 +
  plogis(.585)*mean(s.wet) # .68 occupancy if 75% occupancy

pdet <- plogis(-.875) + plogis(.267)*mean(s.rab) + plogis(-1.442)*0 + 
  plogis(1.063)*.75 + plogis(.118)*mean(s.wet)

