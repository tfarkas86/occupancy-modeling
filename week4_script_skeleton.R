###########################################################
## Week 4 Excercise code
## Estimates of occupancy and detection probability using
## a simple Bayesian occupancy model
###########################################################
  ##load the libraries
    library(R2jags)
  
  ##load the data
    #------------------------------------------------#
    yrwa <- read.csv("~/Dropbox/Occupancy Modelling/yrwa_detection.csv")[,-1]    
    #------------------------------------------------#
  
######################################################
## SECTION 1
## Create a text file describing the occupancy model
## in JAGS syntax
######################################################
  ##the sink function creates a text file of the model
  ##the cat function formats the text entered between the parentheses and quotes as text
  sink("~/Dropbox/Occupancy Modelling/simple_occupancy_model.txt")
    cat("model{
      #########################
      ## Define the likelihood
      #########################
        ##loop through all of the sites (N)
          for(j in 1:N){
            ########################################################
            ## State process (i.e., the true occupancy of the site)
            ########################################################
              #---------------------------------#
              z[j] ~ dbern(psi)
              #---------------------------------#
              
              ##Caluclate z * p.  In JAGS this can't be done
              ##inside the dbern statement below
                          #---------------------#
                p.z[j] <- p * z[j]
                          #---------------------#
      
            #######################
            ## Observation process
            #######################
              ##loop through the repeat surveys (T)
                for(k in 1:T){
                  #-----------------------------------#
                  y[j,k] ~ dbern(p.z[j])
                  #-----------------------------------#
                }##close k loop
        }##close the j loop

      ######################
      ## Define the priors
      ######################
        #-----------------------------#
        psi ~ dunif(0,1)
        #-----------------------------#

        #---------------------------#
        p ~ dunif(0,1)
        #---------------------------#

      #########################################################
      ## Derived quantities
      ## derived in the JAGS model so that we get a posterior
      ## distribution for these quantities
      ##########################################################
        ##Estimated number of occupied sites
          #-----------------------------------------------#
          n <- sum(z[])
          #-----------------------------------------------#
    
    }", fill = T) ##close the model and the cat statement
  sink() ##close the sink function
  

####################################################
## SECTION 2
## Format the data and pass it to JAGS for analysis
####################################################
  
library(R2jags)

  ##Put all the data in a list objects to be passed to JAGS
  ##Note the names of each element of the list (i.e., y, T, N)
  ##must be the same names used in the JAGS model
    #------------------------------#
    dd <- list(y=yrwa, T=ncol(yrwa), N=nrow(yrwa))
    #------------------------------#

  ##Initial values for whether a site is occupied.
  ##You pass a function rather than simple a vector of values
  ##so that JAGS can create multiple chains.
    #---------------------------#
    init <- function() {list(z=apply(yrwa, 1, max))}
    #---------------------------#

  ##Parameters to monitor
    #-------------------------------------------#
    jnames <- c("psi", "p", "n")
    #-------------------------------------------#

  ##fit the model with JAGS
    #--------------------------------#
jmod <- jags(data = dd,
    model.file = "~/Dropbox/Occupancy Modelling/simple_occupancy_model.txt",
    inits = init,
    parameters.to.save = jnames,
    n.burnin = 1000,
    n.iter = 5000,
    n.thin = 2,
    n.chains = 3)
    #--------------------------------#
  
##############################
## sECTION 3
## Look at the model results
##############################
  ##look at the traceplots to see if the chains converged
    #---------------------------------#
    ***3.1. ADD traceplot() FUNCTION***
    #---------------------------------#

  ##look at the summary of the fit object
  ##mu.vect is the mean of the posterior
  ##the interval between 2.5% and 97.5% is the 95% credibel interval
  ##Rhat close to one suggests the chain convereged
  ##n.eff close to the number of chains also suggests the chains converged
    #------------------------------#
    ***3.2. VIEW OBJECT FROM 3.1.***
    #------------------------------#

  ########################################################
  ##plot the posterior estimates for all three parameters
  ########################################################
    ##extract the data for each parameter
              #-------------------------------------------------------#
      psi.p = ***3.3. EXTRACT THE DATA FOR PSI FROM YOUR JAGS OBJECT***
              #-------------------------------------------------------#
    
            #-----------------------------------------------------#
      p.p = ***3.3. EXTRACT THE DATA FOR p FROM YOUR JAGS OBJECT***
            #-----------------------------------------------------#
    
            #-----------------------------------------------------#
      n.p = ***3.3. EXTRACT THE DATA FOR n FROM YOUR JAGS OBJECT***
            #-----------------------------------------------------#

  
    ##plot the data for each parameter
      l = layout(matrix(1:3, nrow = 1)) ##create a plot layout
      layout.show(l) ##preview the layout
  
      ##plot psi
        hist(psi.p, 
             xlab = "psi", 
             main = "", 
             col = "grey")
        abline(v = mean(fit$BUGSoutput$sims.list$psi),
               lwd = 3,
               col = "red")
  
      ##plot p
        hist(p.p, 
             xlab = "p", 
             main = "",
             col = "grey")
        abline(v = mean(fit$BUGSoutput$sims.list$p),
               lwd = 3,
               col = "red")
      
      ##plot number of occupied sites
        hist(n.p, 
             xlab = "Number of occupied sites", 
             main = "",
             col = "grey")
        abline(v = mean(fit$BUGSoutput$sims.list$n.est),
               lwd = 3,
               col = "red")
