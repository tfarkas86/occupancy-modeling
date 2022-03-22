###########################################################
## Week 5 Excercise code
## Estimates of occupancy and detection probability using
## a Bayesian occupancy model with covariates on psi and p
###########################################################
  ##load the libraries
    library(R2jags)
  
  ##load the data
    #-------------------------------------------------#
    #***ADD CODE TO LOAD DATA AND REMOVE SITE NAMES***#    
      setwd("~/Dropbox/Occupancy Modelling/")
      yrwa.d = read.csv("yrwa_detection.csv")
      yrwa.d = yrwa.d[,-1] ##remove the site names for convenience
      covar = read.csv("~/Dropbox/Occupancy Modelling//site_covariates.csv")
    #-------------------------------------------------#
  
######################################################
## SECTION 1
## Create a text file describing the occupancy model
## in JAGS syntax
######################################################
  ##the sink function creates a text file of the model
  ##the cat function formats the text entered between the parentheses and quotes as text
  sink("covariate_occupancy_model.txt")
    cat("model{
      #########################
      ## Define the likelihood
      #########################
        ##loop through all of the sites (N)
          for(j in 1:N){
            ########################################################
            ## State process (i.e., the true occupancy of the site)
            ########################################################

              #-----------------------------------#
              z[j] ~ dbern(psi[j])
              logit(psi[j]) <- a0 + a.elev*elev[j] + a.burn*burn[j] + a.canopy*canopy[j]
              #-----------------------------------#

            #######################
            ## Observation process
            #######################
              ##loop through the repeat surveys (T)
                for(k in 1:T){

                  #-------------------------------------#
                  y[j,k] ~ dbern(p.z[j,k])
                  p.z[j,k] <- z[j]*p[j,k]
                  logit(p[j,k]) <- b0 + b.jday*jday[j]
                  #-------------------------------------#

                }##close k loop
        }##close the j loop

      ######################
      ## Define the priors
      ######################

        #------------------------------------------------#
        #***1.3. DEFINE PRIOR FOR THE INTERCEPT AND   ***#
        #***3 COEFFICIENTS IN THE OCCUPANCY LIKELIHOOD***#
        a0 ~ dnorm(0, 0.001)
        a.elev ~ dnorm(0, 0.001)
        a.burn ~ dnorm(0, 0.001)
        a.canopy ~ dnorm(0, 0.001)
        #------------------------------------------------#

        #-----------------------------------------------#
        #***1.4. DEFINE PRIOR FOR THE INTERCEPT AND  ***#
        #***COEFFICIENT OF THE OBSERVATION LIKELIHOOD***#
        b0 ~ dnorm(0, 0.001)
        b.jday ~ dnorm(0, 0.001)
        #-----------------------------------------------#
    
    }", fill = T) ##close the model and the cat statement
  sink() ##close the sink function
  
####################################################
## SECTION 2
## Format the data and pass it to JAGS for analysis
####################################################
  library(R2jags)

  ##Put all the data in a list objects to be passed to JAGS
  ##Note the names of each element of the list (e.g., y, T, N)
  ##must be the same names used in the JAGS model

    #------------------------------#
    #***2.1 SCALE THE COVARIATES***#
      elev.scale = scale(covar$elev)[,1]
      burn.scale = scale(covar$burn.sev)[,1]
      canopy.scale = scale(covar$canopy.cover)[,1]
      date.scale = scale(covar$jday)[,1]
    #------------------------------#


    #--------------------------------#
    #***2.2. CREATE A LIST OF DATA***#
    jags.data <- list(y=yrwa.d, N=nrow(yrwa.d), T=ncol(yrwa.d), elev=elev.scale, 
                      burn=burn.scale, canopy=canopy.scale, 
                      jday=date.scale)
    #--------------------------------#

  ##Initial values for whether a site is occupied.
  ##You pass a function rather than simple a vector of values
  ##so that JAGS can create multiple chains.

    #-----------------------------#
    inits <- function(){
      list(z=apply(yrwa.d, 1, max))
    }
    #-----------------------------#

  ##Parameters to monitor
    #---------------------------------------------#
    params <- c("a0", "a.elev", "a.burn", "a.canopy", "b0", "b.jday")
    #---------------------------------------------#

  ##fit the model with JAGS
    #--------------------------------------#
    fit = jags(data=jags.data,
           model.file="covariate_occupancy_model.txt",
           inits = inits,
           parameters.to.save=params,
           n.burnin=1000,
           n.iter=5000,
           n.thin=2,
           n.chains=3)


    #--------------------------------------#
  
##############################
## sECTION 3
## Look at the model results
##############################
  ##look at the traceplots to see if the chains converged
    #-----------------------------------#
    traceplot(fit)
  #-----------------------------------#

  ##look at the summary of the fit object
  ##mu.vect is the mean of the posterior
  ##the interval between 2.5% and 97.5% is the 95% credibel interval
  ##Rhat close to one suggests the chain convereged
  ##n.eff close to the number of sites also suggests the chains converged
    #--------------------------------#
    #***3.2. VIEW OBJECT FROM 3.1.***#
    #--------------------------------#

    #----------------------------------------------------------------------#
    #***    3.3 OCCUPANCY AND DETECTION PROABILITY AT AN AVERAGE SITE   ***#
    plogis(quantile(fit$BUGSoutput$sims.list$a0, c(0.025, .5, 0.975)))
    plogis(quantile(fit$BUGSoutput$sims.list$b0, c(0.025, .5, 0.975)))
    #----------------------------------------------------------------------#

    ##Plot the significant relationships
      #---------------------------------------------------------------------------------------------#
      #               ***3.4 SAVE THE POSTERIORS FOR THE COEFFICIENTS AS R OBJECTS***               #
          p.b0 = fit$BUGSoutput$sims.list$a0
          p.a0 = fit$BUGSoutput$sims.list$b0
          p.elev = fit$BUGSoutput$sims.list$a.elev
          p.burn = fit$BUGSoutput$sims.list$a.burn
          p.date = fit$BUGSoutput$sims.list$b.jday
      #---------------------------------------------------------------------------------------------#
        
      ##Create a sequence of values in the range of the scaled covariates
        elev = seq(min(elev.scale), max(elev.scale), length = 100)
        burn = seq(min(burn.scale), max(burn.scale), length = 100)
        date = seq(min(date.scale), max(date.scale), length = 100)

      ##Make the predictions on the logit scale with scaled parameters
      ##not including the non-focal variables in the predictions is 
      ##like saying that these variables are held constant at their
      ##mean value for the prediction (because the mean of a scaled
      ##variable is 0)
        pred.elev = plogis(mean(p.b0) + 
                           mean(p.elev)*elev)

        pred.burn = plogis(mean(p.b0) + 
                           mean(p.burn)*burn)
        
        pred.date = plogis(mean(p.a0) + 
                           mean(p.date)*date)

       ##plot all three relatioships
          l = layout(matrix(1:3, nrow = 1))

          plot(pred.elev~elev,
               type = "l",
               axes = F,
               ylab = "Occupancy",
               xlab = "Elevation (m)")
          at = -2:2
          axis(side = 1, at = at,
              labels = round(at*sd(covar$elev)+mean(covar$elev)))
          axis(side = 2)
          box()

          plot(pred.burn~burn,
               type = "l",
               axes = F,
               ylab = "Occupancy",
               xlab = "Burn Severity")
          at = seq(-1,1.5, by = 0.5)
          axis(side = 1, at = at,
               labels = round(at*sd(covar$burn.sev)+mean(covar$burn.sev)))
          axis(side = 2)
          box()

          plot(pred.date~date,
               type = "l",
               axes = F,
               ylab = "Detection",
               xlab = "Julian date")
          at = seq(-1,1.5, by = 0.5)
          axis(side = 1, at = at,
               labels = round(at*sd(covar$jday)+mean(covar$jday)))
          axis(side = 2)
          box()

