##########################
## Week 7 Exercise Script
##########################
library(R2jags)

##################
## Load the data

load("~/Dropbox/Occupancy Modelling/yrwa_multiseason.RData")
##################

  ##----------------------------------------------------------------------#
  ##***       Q1. SET WORKING DIRECTORY AND LOAD THE DATA              ***#
    
  ##----------------------------------------------------------------------#

##################################
## Write a JAGS model text file
##################################
sink("~/Dropbox/Occupancy Modelling/dynamic_yrwa.txt")
  cat("model{
        ##---------------------------------------------------------------------------------#
        ##***               Q3. WRITE THE JAGS CODE FOR THE LIKELIHOOD                  ***#
          ###########################
          ##Ecological process model
          ###########################
            ##loop through the sites
              for(i in 1:n.sites){
                ##Occupancy state in the first year
                  
                  z[i,1] ~ dbern(psi[i,1])
                  logit(psi[i,1]) <- a0 + a_elev*elev[i]


                ##occupancy state in all other years
                ##using a loop through years 2 to 6
                ##phi is the survival rate
                ##gamma is the colonization rate
                  for(t in 2:n.years){
                  
                    z[i,t] ~ dbern(z[i,t-1]*phi[i,t-1] + (1 - z[i,t-1])*gam[i,t-1])
                    logit(phi[i,t-1]) <- b0 + b_fire*fire[i,t]
                    logit(gam[i,t-1]) <- c0 + c_fire*fire[i,t]

                  }
              }

          ############################
          ##Observation process model
          ############################

            ##loop through the sites
            for(i in 1:n.sites){
              ##loop through the replicate surveys
              
                ##loop through the years
                for(t in 1:n.years){

                  zp[i,t] <- z[i,t] * p[t] 
                  
                    for(j in 1:n.reps){                  
                    y[i,j,t] ~ dbern(zp[i,t])
                 
                }
              }
            }
        ##---------------------------------------------------------------------------------#    

    ##------------------------------------------------##
    ##***         Q4. DEFINE THE PRIORS            ***##



      ##PUT ALL PRIORS OTHER THAN P OUTSIDE THE LOOP

a0 ~ dnorm(0, .001)
a_elev ~ dnorm(0, .001)
b0 ~ dnorm(0, .001)
c0 ~ dnorm(0, .001)
b_fire ~ dnorm(0, .001)
c_fire ~ dnorm(0, .001)

      for(t in 1:n.years){
      
        ##PUT PRIOR FOR P IN THE LOOP

p[t] ~ dunif(0,1)

      }
    ##------------------------------------------------##
  }", fill = T)
sink()

##########################################
## Format the data and run the JAGS model
##########################################
  ##---------------------------------------------------------##
  ##*** Q5. CREATE A LIST OF DATA OBJECTS TO PASS TO JAGS ***##  

s.elev <- scale(yrwa.data$siteCovs$elev)[,1]
jags.data <- with(yrwa.data, list(y=y, elev=siteCovs$elev, fire=fa, n.years=5, 
                                  n.sites=459, n.reps=3))
  ##---------------------------------------------------------##

  ##initial values function
    z.est = apply(yrwa.data$y, c(1,3), max, na.rm=T)
    z.est[z.est == -Inf] = 0
    inits = function(){list(z = z.est)}


  ##---------------------------------------------------------##
  ##*** Q6. CREATE A VECTOR OF PARAMETER NAMES TO MONITOR ***##
  
params <- c("a0", "a_elev", "b0", "b_fire", "c0", "c_fire", "phi", "gam", "p", 
            "psi")

  ##---------------------------------------------------------##

  ##---------------------------------------------------------##
  ##***                Q7. RUN THE JAGS MODEL             ***##
  fit <- jags(data=jags.data, 
              model.file="~/Dropbox/Occupancy Modelling/dynamic_yrwa.txt",
              parameters.to.save=params,
              inits=inits, 
              n.chains=3, 
              n.iter=3000, 
              n.burnin=1000, 
              n.thin=2)
            



  ##---------------------------------------------------------##


  ##-----------------------------------------------------------------------##
  ##*** Q7a. RELATIONSHIP BETWEEN INITIAL OCCUPANCY AND ELEVATION       ***##

load("~/Dropbox/Occupancy Modelling/yrwa_dynamic_occupancy.RData")
elevpost <- fit$BUGSoutput$sims.list$b.psi.elev
sum(elevpost>0)/length(elevpost) # .996
##calculate the proportion of the posterior that is positive


  ##-----------------------------------------------------------------------##



  ##-----------------------------------------------------------------------##
  ##***           Q7b. RELATIONSHIP BETWEEN PHI AND FIRE AGE            ***##
    
  
  
    ##calculate the proportion of the posterior that is negative
  
phifirepost <- fit$BUGSoutput$sims.list$b.phi.fa
sum(phifirepost < 0)/length(phifirepost) # .761
  ##-----------------------------------------------------------------------##

  ##-----------------------------------------------------------------------##
  ##***         Q7c. RELATIONSHIP BETWEEN GAMMA AND FIRE AGE            ***##


gamfirepost <- fit$BUGSoutput$sims.list$b.gam.fa
sum(gamfirepost > 0)/length(gamfirepost) # .814
    ##calculate the proportion of the posterior that is positive


  ##-----------------------------------------------------------------------##


  ##-----------------------------------------------------------------------##
  ##***       Q7d. COLONIZATION PROBABILITY AT AVERAGE SITE             ***## 

plogis(-1.245 + (.148*mean(yrwa.data$fa))) # .23

  ##-----------------------------------------------------------------------##


  ##-----------------------------------------------------------------------##
  ##***         Q7e. EXTINCTION PROBABILTIY AT AVERAGE SITE             ***##

1-plogis(.187 + (-.17*mean(yrwa.data$fa))) # .543

  ##-----------------------------------------------------------------------##


  ##--------------------------------------------------------------------------------##
  ##***       Q7f. AVERAGE PROABILITY SITE IS OCCUPIED DURING INITIAL YEAR       ***##
  
plogis(-.970 + (.404)*mean(yrwa.data$siteCovs$elev)) # .28

  ##--------------------------------------------------------------------------------##


  ##--------------------------------------------------------------------------------##
  ##***           Q7g. DIFFERENCES IN DETECTION PROBABILITY AMONG YEARS          ***##

sapply(1:5, function(x) {
  quantile(fit$BUGSoutput$sims.list$p[,x], probs=c(.975, .025))
})





  ##--------------------------------------------------------------------------------##
