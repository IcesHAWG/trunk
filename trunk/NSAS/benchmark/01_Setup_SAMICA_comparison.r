################################################################################
# FLSAM_Setup_Objects
#
# $Rev: 606 $
# $Date: 2011-11-10 19:50:13 +0100 (do, 10 nov 2011) $
#
# Author: HAWG model devlopment group
#
# Sets up the default FLSAM control object necessary to perform a 
# stock assessment. Script is intended to be called
# from other external sources.
#
# Developed with:
#   - R version 2.13.1
#   - FLCore 2.4
#
# To be done:
#
# Notes: Have fun running this assessment!
#
################################################################################

### ============================================================================
### Setup default assessment configuration
### ============================================================================
#Setup configuration - creates an empty control object with appropriate structure
NSH.ctrl <- FLSAM.control(NSH,NSH.tun)

#Fishing mortality random walk coupling
NSH.ctrl@states["catch",] <- c(1:10)            #Couple age 7+ Fs

##Log N random walk variances
NSH.ctrl@logN.vars <- c(1,rep(2,9)) #give freedom to recruits

##Log f random walk variances
NSH.ctrl@f.vars["catch",] <- c(rep(1,2),rep(2,7),3)  #barplot(apply(apply(log(NSH@stock.n[,ac(1985:2005)]),1,diff),2,var))    #not seperably period selected

#NSH.ctrl@fleets["MLAI"] <- 4
#NSH.ctrl@power.law.exps["MLAI","0"] <- 1

##Update
NSH.ctrl <- update(NSH.ctrl)

