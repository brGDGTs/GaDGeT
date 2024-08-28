#******************************************************   brGDGTS FAs  ********************************************************
#******************************************************   FUNCTIONS    ********************************************************
#******************************************************       TS       ********************************************************

#-------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------- AUTHOR -----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------

# Author: Tobias Schneider
# Date: 05.12.2020
# Last modification: 19. June 2023
# Email: tobiaschnei@gmail.com, www.drtobiasschneider.com


# DISCLAIMER

# The author does not guarantee for the functionality of this script. Please do not hesitate to get back to the author
# to report problems, or for troubleshooting.

#-------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------- SCRIPT DESCRIPTION --------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------

# This script contains the functions used for the main script to calculate different kinds of fractional abndances of brGDGTs.
# The script contains the following seven functions:

#---------------------------------------------------------------
#--------- FUNCTION CALLS: -------------------------------------
#---------------------------------------------------------------

#------ brGDGTs ----------

#          1. brGDGT_FA

#          2. brGDGT_MI_FA
#          3. brGDGT_METH_5Mep_FA
#          4. brGDGT_METH_6Mep_FA
#          5. brGDGT_METH_5Me_FA
#          6. brGDGT_METH_6Me_FA
#          7. brGDGT_METH_FA

#          8. brGDGT_CYCL_FA
#          9. brGDGT_CYCL_5Me_FA
#          10. brGDGT_CYCL_6Me_FA


#------ isoGDGTs ---------

#         11. fGDGT0
#         12. fGDGT1
#         13. fGDGT2
#         13. fGDGT3
#         14. fGDGT4
#         15. fGDGT4.2


#------ OHDGTs ----------

#         17. fOH.0
#         18. fOH.1
#         19. fOH.2


#------ GMGTs ----------

#         20. fGMGT1
#         21. fGMGT2a
#         22. fGMGT2b
#         23. fGMGT2c
#         24. fGMGT3a
#         25. fGMGT3b
#         26. fGMGT3c

#---------------------------------------------------------------
#--------- CALCULATION-DESCRIPTIONS: ---------------------------
#---------------------------------------------------------------

#------ brGDGTs ----------

#          1. Fractional Abundances, calculated acc. Raberg et al. (2021) FULL:             Ia/SUM(TOT brGDGT)

#          2. Fractional Abundances, calculated acc. Raberg et al. (2021) MI:               Ia/(Ia+IIa+IIIa+IIa'+IIIa'), same for b and c
#          3. Fractional Abundances, calculated acc. Raberg et al. (2021) METH-5Me+:        Ia/(Ia+IIa+IIIa), same for b and c
#          4. Fractional Abundances, calculated acc. Raberg et al. (2021) METH-6Me+:        Ia/(Ia+IIa'+IIIa'), same for b and c
#          5. Fractional Abundances, calculated acc. Raberg et al. (2021) METH-5Me:         IIa/(IIa+IIIa), same for b and c
#          6. Fractional Abundances, calculated acc. Raberg et al. (2021) METH-6Me:         IIa´/(IIa'+IIIa'), same for b and c
#          7. Fractional Abundances, calculated acc. Raberg et al. (2021) METH:             METH-5Me+ and METH-6Me

#          8. Fractional Abundances, calculated acc. Raberg et al. (2021) CI:               IIa/(IIa+IIb+IIc+IIa'+IIb'+IIc'), same for I and III
#          9. Fractional Abundances, calculated acc. Raberg et al. (2021) CYC-5Me+:         IIa/(IIa+IIb+IIc), same for I and III
#          10. Fractional Abundances, calculated acc. Raberg et al. (2021) CYC-6Me+:        IIa'/(IIa'+IIb'+IIc'), same for I and III


#------ isoGDGTs ---------

#         11. Fractional Abundances of isoGDGTs, calculated as follows:                     GDGT0 / (GDGT0 + GDGT1 + GDGT2 + GDGT3 +GDGT4 + GDGT4.2)
#         12. Fractional Abundances of isoGDGTs, calculated as follows:                     GDGT1 / (GDGT0 + GDGT1 + GDGT2 + GDGT3 +GDGT4 + GDGT4.2)
#         13. Fractional Abundances of isoGDGTs, calculated as follows:                     GDGT2 / (GDGT0 + GDGT1 + GDGT2 + GDGT3 +GDGT4 + GDGT4.2)
#         14. Fractional Abundances of isoGDGTs, calculated as follows:                     GDGT3 / (GDGT0 + GDGT1 + GDGT2 + GDGT3 +GDGT4 + GDGT4.2)
#         15. Fractional Abundances of isoGDGTs, calculated as follows:                     GDGT4 / (GDGT0 + GDGT1 + GDGT2 + GDGT3 +GDGT4 + GDGT4.2)
#         16. Fractional Abundances of isoGDGTs, calculated as follows:                     GDGT4.2 / (GDGT0 + GDGT1 + GDGT2 + GDGT3 +GDGT4 + GDGT4.2)


#------ OHDGTs ----------

#         17. Fractional Abundances of isoGDGTs, calculated as follows:                     OHGDGT.0 / (OHGDGT.0 + OHGDGT.1 + OHGDGT.2)
#         18. Fractional Abundances of isoGDGTs, calculated as follows:                     OHGDGT.1 / (OHGDGT.0 + OHGDGT.1 + OHGDGT.2)
#         19. Fractional Abundances of isoGDGTs, calculated as follows:                     OHGDGT.2 / (OHGDGT.0 + OHGDGT.1 + OHGDGT.2)


#------ GMGTs ----------

#         20. Fractional Abundances of fGMGT1 (H1048), calculated as follows:                H1048  / (H1048 + H1034a + H1034b + H1034c + H1020a + H1020b + H1020c)
#         21. Fractional Abundances of fGMGT2a (H1034a), calculated as follows:              H1034a / (H1048 + H1034a + H1034b + H1034c + H1020a + H1020b + H1020c)
#         22. Fractional Abundances of fGMGT2b (H1034b), calculated as follows:              H1034b / (H1048 + H1034a + H1034b + H1034c + H1020a + H1020b + H1020c)
#         23. Fractional Abundances of fGMGT2c (H1034c), calculated as follows:              H1034c / (H1048 + H1034a + H1034b + H1034c + H1020a + H1020b + H1020c)
#         24. Fractional Abundances of fGMGT3a (H1020a), calculated as follows:              H1020a / (H1048 + H1034a + H1034b + H1034c + H1020a + H1020b + H1020c)
#         25. Fractional Abundances of fGMGT3b (H1020b), calculated as follows:              H1020b / (H1048 + H1034a + H1034b + H1034c + H1020a + H1020b + H1020c)
#         26. Fractional Abundances of fGMGT3c (H1020c), calculated as follows:              H1020c / (H1048 + H1034a + H1034b + H1034c + H1020a + H1020b + H1020c)




############################################################################################################################
#################################################### FA - FUNCTIONS ########################################################
############################################################################################################################

###----------------------------------------------------------------------------------------------------------------------###
###---------------------------------------- 1. brGDGTs FA COMMON-FUNCTION -----------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

#Initialize Function
brGDGT_FA <- function(brGDGTs){
  
  # initialize FA matrix
  brGDGT.FA    <- brGDGTs
  
  # for loops browsing through rows and cols and calculating the FAs and saving them in the matrix
  for(c in 1: ncol(brGDGT.FA)){
    
    brGDGT.FA[,c] <- brGDGTs[,c]/rowSums(brGDGTs)
    
  }
  
  ###---------------------------------------------- FA TOTAL PRINT --------------------------------------------------------###
  
  # put GDGTs in correct order for follow up script
  brGDGT.FA<-cbind(rownames(brGDGTs),
                   brGDGT.FA[,c("Ia", "Ib", "Ic")],
                   brGDGT.FA[,c("IIa.5Me", "IIa.6Me", "IIb.5Me", "IIb.6Me", "IIc.5Me", "IIc.6Me")],
                   brGDGT.FA[,c("IIIa.5Me", "IIIa.6Me", "IIIb.5Me", "IIIb.6Me", "IIIc.5Me", "IIIc.6Me")])
  
  colnames(brGDGT.FA)[1] <- "Label"
  
  return(brGDGT.FA)
}

###----------------------------------------------- FUNCTION ENDS ---------------------------------------------------------###











###----------------------------------------------------------------------------------------------------------------------###
###----------------------------------- 2. brGDGTs FA MI METHYLIZATION-FUNCTION ------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###


#Initialize Function
brGDGT_MI_FA <- function(brGDGTs){
  
  # prepare data subsets
  brGDGT.MI.a    <- brGDGTs[,c("Ia","IIa.5Me","IIa.6Me","IIIa.5Me","IIIa.6Me")]
  brGDGT.MI.b    <- brGDGTs[,c("Ib","IIb.5Me","IIb.6Me","IIIb.5Me","IIIb.6Me")]
  brGDGT.MI.c    <- brGDGTs[,c("Ic","IIc.5Me","IIc.6Me","IIIc.5Me","IIIc.6Me")]
  
  # initialize FA matrices
  brGDGT.FA.MI.a <- brGDGT.MI.a
  brGDGT.FA.MI.b <- brGDGT.MI.b
  brGDGT.FA.MI.c <- brGDGT.MI.c
  
  # for loops browsing through rows and cols and calculating the FAs and saving them in the matrix
  for(c in 1: ncol(brGDGT.FA.MI.a)){
    
    brGDGT.FA.MI.a[,c] <- brGDGT.MI.a[,c]/rowSums(brGDGT.MI.a)
    brGDGT.FA.MI.b[,c] <- brGDGT.MI.b[,c]/rowSums(brGDGT.MI.b)
    brGDGT.FA.MI.c[,c] <- brGDGT.MI.c[,c]/rowSums(brGDGT.MI.c)
    
  }
  
  
  ###---------------------------------------------- FA METH PRINT -----------------------------------------------------###
  
  # put GDGTs in correct order for follow up scripts
  brGDGT.FA.MI <- cbind(rownames(brGDGTs), brGDGT.FA.MI.a[,1], brGDGT.FA.MI.b[,1], brGDGT.FA.MI.c[,1],
                        brGDGT.FA.MI.a[,2:3], brGDGT.FA.MI.b[,2:3], brGDGT.FA.MI.c[,2:3],
                        brGDGT.FA.MI.a[,4:5], brGDGT.FA.MI.b[,4:5], brGDGT.FA.MI.c[,4:5])
  
  colnames(brGDGT.FA.MI)[1:4] <- c("Label","Ia","Ib","Ic")
  
  
  return(brGDGT.FA.MI)
}

###----------------------------------------------- FUNCTION ENDS ------------------------------------------------------###



###----------------------------------------------------------------------------------------------------------------------###
###--------------------------------- 3. brGDGTs FA METHYLIZATION 5Me+-FUNCTION ------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###


#Initialize Function
brGDGT_METH_5Mep_FA <- function(brGDGTs){
  
  # prepare data subsets
  brGDGT.5Mep.a    <- brGDGTs[,c("Ia","IIa.5Me","IIIa.5Me")]
  brGDGT.5Mep.b    <- brGDGTs[,c("Ib","IIb.5Me","IIIb.5Me")]
  brGDGT.5Mep.c    <- brGDGTs[,c("Ic","IIc.5Me","IIIc.5Me")]
  
  # initialize FA matrices
  brGDGT.FA.5Mep.a <- brGDGT.5Mep.a
  brGDGT.FA.5Mep.b <- brGDGT.5Mep.b
  brGDGT.FA.5Mep.c <- brGDGT.5Mep.c
  
  # for loops browsing through rows and cols and calculating the FAs and saving them in the matrix
  for(c in 1: ncol(brGDGT.FA.5Mep.a)){
    
    brGDGT.FA.5Mep.a[,c] <- brGDGT.5Mep.a[,c]/rowSums(brGDGT.5Mep.a)
    brGDGT.FA.5Mep.b[,c] <- brGDGT.5Mep.b[,c]/rowSums(brGDGT.5Mep.b)
    brGDGT.FA.5Mep.c[,c] <- brGDGT.5Mep.c[,c]/rowSums(brGDGT.5Mep.c)
    
  }
  
  
  ###---------------------------------------------- FA METH PRINT -------------------------------------------------------###
  
  # put GDGTs in correct order for follow up scripts
  brGDGT.FA.5Mep<-cbind(rownames(brGDGTs), brGDGT.FA.5Mep.a[,1], brGDGT.FA.5Mep.b[,1], brGDGT.FA.5Mep.c[,1],
                        brGDGT.FA.5Mep.a[,2:3], brGDGT.FA.5Mep.b[,2:3], brGDGT.FA.5Mep.c[,2:3])
  
  colnames(brGDGT.FA.5Mep)[1:4] <- c("Label","Ia","Ib","Ic")
  
  return(brGDGT.FA.5Mep)
}

###----------------------------------------------- FUNCTION ENDS --------------------------------------------------------###











###----------------------------------------------------------------------------------------------------------------------###
###--------------------------------- 4. brGDGTs FA METHYLIZATION 6Me+-FUNCTION ------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###


#Initialize Function
brGDGT_METH_6Mep_FA <- function(brGDGTs){
  
  # prepare data subsets
  brGDGT.6Mep.a    <- brGDGTs[,c("Ia","IIa.6Me","IIIa.6Me")]
  brGDGT.6Mep.b    <- brGDGTs[,c("Ib","IIb.6Me","IIIb.6Me")]
  brGDGT.6Mep.c    <- brGDGTs[,c("Ic","IIc.6Me","IIIc.6Me")]
  
  # initialize FA matrices
  brGDGT.FA.6Mep.a <- brGDGT.6Mep.a
  brGDGT.FA.6Mep.b <- brGDGT.6Mep.b
  brGDGT.FA.6Mep.c <- brGDGT.6Mep.c
  
  # for loops browsing through rows and cols and calculating the FAs and saving them in the matrix
  for(c in 1: ncol(brGDGT.FA.6Mep.a)){
    
    brGDGT.FA.6Mep.a[,c] <- brGDGT.6Mep.a[,c]/rowSums(brGDGT.6Mep.a)
    brGDGT.FA.6Mep.b[,c] <- brGDGT.6Mep.b[,c]/rowSums(brGDGT.6Mep.b)
    brGDGT.FA.6Mep.c[,c] <- brGDGT.6Mep.c[,c]/rowSums(brGDGT.6Mep.c)
    
  }
  
  
  ###---------------------------------------------- FA 6Mep PRINT -------------------------------------------------------###
  
  # put GDGTs in correct order for follow up scripts
  brGDGT.FA.6Mep<-cbind(rownames(brGDGTs), brGDGT.FA.6Mep.a[,1], brGDGT.FA.6Mep.b[,1], brGDGT.FA.6Mep.c[,1],
                        brGDGT.FA.6Mep.a[,2:3], brGDGT.FA.6Mep.b[,2:3], brGDGT.FA.6Mep.c[,2:3])
  
  colnames(brGDGT.FA.6Mep)[1:4] <- c("Label","Ia","Ib","Ic")
  
  return(brGDGT.FA.6Mep)
}

###----------------------------------------------- FUNCTION ENDS --------------------------------------------------------###











###----------------------------------------------------------------------------------------------------------------------###
###--------------------------------- 5. brGDGTs FA METHYLIZATION 5Me-FUNCTION ------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###


#Initialize Function
brGDGT_METH_5Me_FA <- function(brGDGTs){
  
  # prepare data subsets
  brGDGT.5Me.a    <- brGDGTs[,c("IIa.5Me","IIIa.5Me")]
  brGDGT.5Me.b    <- brGDGTs[,c("IIb.5Me","IIIb.5Me")]
  brGDGT.5Me.c    <- brGDGTs[,c("IIc.5Me","IIIc.5Me")]
  
  # initialize FA matrices
  brGDGT.FA.5Me.a <- brGDGT.5Me.a
  brGDGT.FA.5Me.b <- brGDGT.5Me.b
  brGDGT.FA.5Me.c <- brGDGT.5Me.c
  
  # for loops browsing through rows and cols and calculating the FAs and saving them in the matrix
  for(c in 1: ncol(brGDGT.FA.5Me.a)){
    
    brGDGT.FA.5Me.a[,c] <- brGDGT.5Me.a[,c]/rowSums(brGDGT.5Me.a)
    brGDGT.FA.5Me.b[,c] <- brGDGT.5Me.b[,c]/rowSums(brGDGT.5Me.b)
    brGDGT.FA.5Me.c[,c] <- brGDGT.5Me.c[,c]/rowSums(brGDGT.5Me.c)
    
  }
  
  
  ###---------------------------------------------- FA METH PRINT -------------------------------------------------------###
  
  # put GDGTs in correct order for follow up scripts
  brGDGT.FA.5Me<-cbind(rownames(brGDGTs), brGDGT.FA.5Me.a, brGDGT.FA.5Me.b, brGDGT.FA.5Me.c)
  
  colnames(brGDGT.FA.5Me)[1] <- "Label"
  
  return(brGDGT.FA.5Me)
}

###----------------------------------------------- FUNCTION ENDS --------------------------------------------------------###











###----------------------------------------------------------------------------------------------------------------------###
###--------------------------------- 6. brGDGTs FA METHYLIZATION 6Me-FUNCTION ------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###


#Initialize Function
brGDGT_METH_6Me_FA <- function(brGDGTs){
  
  # prepare data subsets
  brGDGT.6Me.a    <- brGDGTs[,c("IIa.6Me","IIIa.6Me")]
  brGDGT.6Me.b    <- brGDGTs[,c("IIb.6Me","IIIb.6Me")]
  brGDGT.6Me.c    <- brGDGTs[,c("IIc.6Me","IIIc.6Me")]
  
  # initialize FA matrices
  brGDGT.FA.6Me.a <- brGDGT.6Me.a
  brGDGT.FA.6Me.b <- brGDGT.6Me.b
  brGDGT.FA.6Me.c <- brGDGT.6Me.c
  
  # for loops browsing through rows and cols and calculating the FAs and saving them in the matrix
  for(c in 1: ncol(brGDGT.FA.6Me.a)){
    
    brGDGT.FA.6Me.a[,c] <- brGDGT.6Me.a[,c]/rowSums(brGDGT.6Me.a)
    brGDGT.FA.6Me.b[,c] <- brGDGT.6Me.b[,c]/rowSums(brGDGT.6Me.b)
    brGDGT.FA.6Me.c[,c] <- brGDGT.6Me.c[,c]/rowSums(brGDGT.6Me.c)
    
  }
  
  
  ###---------------------------------------------- FA 6Me PRINT -------------------------------------------------------###
  
  # put GDGTs in correct order for follow up scripts
  brGDGT.FA.6Me<-cbind(rownames(brGDGTs), brGDGT.FA.6Me.a, brGDGT.FA.6Me.b, brGDGT.FA.6Me.c)
  
  colnames(brGDGT.FA.6Me)[1] <- "Label"
  
  return(brGDGT.FA.6Me)
}

###----------------------------------------------- FUNCTION ENDS --------------------------------------------------------###











###----------------------------------------------------------------------------------------------------------------------###
###--------------------------------- 7. brGDGTs FA METHYLATION FUNCTION -------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###


#Initialize Function
brGDGT_METH_FA <- function(brGDGTs){
  
  # prepare data subsets
  brGDGT.METH.a    <- brGDGTs[,c("Ia","IIa.5Me","IIIa.5Me")]
  brGDGT.METH.a2   <- brGDGTs[,c("IIa.6Me","IIIa.6Me")]
  
  brGDGT.METH.b    <- brGDGTs[,c("Ib","IIb.5Me","IIIb.5Me")]
  brGDGT.METH.b2   <- brGDGTs[,c("IIb.6Me","IIIb.6Me")]
  
  brGDGT.METH.c    <- brGDGTs[,c("Ic","IIc.5Me","IIIc.5Me")]
  brGDGT.METH.c2   <- brGDGTs[,c("IIc.6Me","IIIc.6Me")]
  
  # initialize FA matrices
  brGDGT.FA.METH.a <- brGDGT.METH.a
  brGDGT.FA.METH.b <- brGDGT.METH.b
  brGDGT.FA.METH.c <- brGDGT.METH.c
  
  # for loops browsing through rows and cols and calculating the FAs and saving them in the matrix
  for(c in 1: ncol(brGDGT.FA.METH.a)){
    
    brGDGT.FA.METH.a[,c] <- brGDGT.METH.a[,c]/rowSums(brGDGT.METH.a)
    brGDGT.FA.METH.b[,c] <- brGDGT.METH.b[,c]/rowSums(brGDGT.METH.b)
    brGDGT.FA.METH.c[,c] <- brGDGT.METH.c[,c]/rowSums(brGDGT.METH.c)
    
  }
  
  # initialize FA matrices
  brGDGT.FA.METH.a2 <- brGDGT.METH.a2
  brGDGT.FA.METH.b2 <- brGDGT.METH.b2
  brGDGT.FA.METH.c2 <- brGDGT.METH.c2
  
  # for loops browsing through rows and cols and calculating the FAs and saving them in the matrix
  for(c in 1: ncol(brGDGT.FA.METH.a2)){
    
    brGDGT.FA.METH.a2[,c] <- brGDGT.METH.a2[,c]/rowSums(brGDGT.METH.a2)
    brGDGT.FA.METH.b2[,c] <- brGDGT.METH.b2[,c]/rowSums(brGDGT.METH.b2)
    brGDGT.FA.METH.c2[,c] <- brGDGT.METH.c2[,c]/rowSums(brGDGT.METH.c2)
    
  }
  
  
  
  ###---------------------------------------------- FA METH PRINT -------------------------------------------------------###
  
  # put GDGTs in correct order for follow up scripts
  brGDGT.FA.METH  <- cbind(rownames(brGDGTs),  brGDGT.FA.METH.a, brGDGT.FA.METH.a2, brGDGT.FA.METH.b, brGDGT.FA.METH.b2, brGDGT.FA.METH.c, brGDGT.FA.METH.c2)
  
  colnames(brGDGT.FA.METH)[1] <- "Label"
  return(brGDGT.FA.6Me)
}

###----------------------------------------------- FUNCTION ENDS --------------------------------------------------------###











###----------------------------------------------------------------------------------------------------------------------###
###---------------------------------- 8. brGDGTs FA CYCLIZATION-FUNCTION ------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###


#Initialize Function
brGDGT_CYCL_FA <- function(brGDGTs){
  
  # prepare data subsets
  brGDGT.CYCL.I     <- brGDGTs[,c("Ia","Ib","Ic")]
  brGDGT.CYCL.II    <- brGDGTs[,c("IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me","IIc.5Me","IIc.6Me")]
  brGDGT.CYCL.III   <- brGDGTs[,c("IIIa.5Me","IIIa.6Me","IIIb.5Me","IIIb.6Me","IIIc.5Me","IIIc.6Me")]
  
  # initialize FA matrices
  brGDGT.FA.CYCL.I   <- brGDGT.CYCL.I
  brGDGT.FA.CYCL.II  <- brGDGT.CYCL.II
  brGDGT.FA.CYCL.III <- brGDGT.CYCL.III
  
  # for loops browsing through rows and cols and calculating the FAs and saving them in the matrix
  for(c in 1: ncol(brGDGT.FA.CYCL.I)){
    
    brGDGT.FA.CYCL.I[,c]   <- brGDGT.CYCL.I[,c]/rowSums(brGDGT.CYCL.I) #needs to have own loop, since only 3 cols
  }
  
  for(c in 1: ncol(brGDGT.FA.CYCL.II)){
    
    brGDGT.FA.CYCL.II[,c]  <- brGDGT.CYCL.II[,c]/rowSums(brGDGT.CYCL.II)
    brGDGT.FA.CYCL.III[,c] <- brGDGT.CYCL.III[,c]/rowSums(brGDGT.CYCL.III)
    
  }
  
  
  ###---------------------------------------------- FA CYCL PRINT --------------------------------------------------------###
  
  # put GDGTs in correct order for follow up scripts
  brGDGT.FA.CYCL<-cbind(rownames(brGDGTs), brGDGT.FA.CYCL.I, brGDGT.FA.CYCL.II, brGDGT.FA.CYCL.III)
  
  colnames(brGDGT.FA.CYCL)[1] <- c("Label")
  
  return(brGDGT.FA.CYCL)
}

###----------------------------------------------- FUNCTION ENDS --------------------------------------------------------###











###----------------------------------------------------------------------------------------------------------------------###
###-------------------------------- 9. brGDGTs FA CYCLIZATION 5Me-FUNCTION ----------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###


#Initialize Function
brGDGT_CYCL_5Me_FA <- function(brGDGTs){
  
  # prepare data subsets
  brGDGT.CYCL.I     <- brGDGTs[,c("Ia","Ib","Ic")]
  brGDGT.CYCL.II    <- brGDGTs[,c("IIa.5Me","IIb.5Me","IIc.5Me")]
  brGDGT.CYCL.III   <- brGDGTs[,c("IIIa.5Me","IIIb.5Me","IIIc.5Me")]
  
  # initialize FA matrices
  brGDGT.FA.CYCL.I   <- brGDGT.CYCL.I
  brGDGT.FA.CYCL.II  <- brGDGT.CYCL.II
  brGDGT.FA.CYCL.III <- brGDGT.CYCL.III
  
  # for loops browsing through rows and cols and calculating the FAs and saving them in the matrix
  for(c in 1: ncol(brGDGT.FA.CYCL.I)){
    
    brGDGT.FA.CYCL.I[,c]   <- brGDGT.CYCL.I[,c]/rowSums(brGDGT.CYCL.I) #needs to have own loop, since only 3 cols
  }
  
  for(c in 1: ncol(brGDGT.FA.CYCL.II)){
    
    brGDGT.FA.CYCL.II[,c]  <- brGDGT.CYCL.II[,c]/rowSums(brGDGT.CYCL.II)
    brGDGT.FA.CYCL.III[,c] <- brGDGT.CYCL.III[,c]/rowSums(brGDGT.CYCL.III)
    
  }
  
  
  ###---------------------------------------------- FA CYCL PRINT --------------------------------------------------------###
  
  # put GDGTs in correct order for follow up scripts
  brGDGT.FA.CYCL<-cbind(rownames(brGDGTs), brGDGT.FA.CYCL.I, brGDGT.FA.CYCL.II, brGDGT.FA.CYCL.III)
  
  colnames(brGDGT.FA.CYCL)[1] <- c("Label")
  
  return(brGDGT.FA.CYCL)
}

###----------------------------------------------- FUNCTION ENDS --------------------------------------------------------###











###----------------------------------------------------------------------------------------------------------------------###
###-------------------------------- 10. brGDGTs FA CYCLIZATION 6Me-FUNCTION ----------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###


#Initialize Function
brGDGT_CYCL_6Me_FA <- function(brGDGTs){
  
  # prepare data subsets
  brGDGT.CYCL.I     <- brGDGTs[,c("Ia","Ib","Ic")]
  brGDGT.CYCL.II    <- brGDGTs[,c("IIa.6Me","IIb.6Me","IIc.6Me")]
  brGDGT.CYCL.III   <- brGDGTs[,c("IIIa.6Me","IIIb.6Me","IIIc.6Me")]
  
  # initialize FA matrices
  brGDGT.FA.CYCL.I   <- brGDGT.CYCL.I
  brGDGT.FA.CYCL.II  <- brGDGT.CYCL.II
  brGDGT.FA.CYCL.III <- brGDGT.CYCL.III
  
  # for loops browsing through rows and cols and calculating the FAs and saving them in the matrix
  for(c in 1: ncol(brGDGT.FA.CYCL.I)){
    
    brGDGT.FA.CYCL.I[,c]   <- brGDGT.CYCL.I[,c]/rowSums(brGDGT.CYCL.I) #needs to have own loop, since only 3 cols
  }
  
  for(c in 1: ncol(brGDGT.FA.CYCL.II)){
    
    brGDGT.FA.CYCL.II[,c]  <- brGDGT.CYCL.II[,c]/rowSums(brGDGT.CYCL.II)
    brGDGT.FA.CYCL.III[,c] <- brGDGT.CYCL.III[,c]/rowSums(brGDGT.CYCL.III)
    
  }
  
  
  ###---------------------------------------------- FA CYCL PRINT --------------------------------------------------------###
  
  # put GDGTs in correct order for follow up scripts
  brGDGT.FA.CYCL<-cbind(rownames(brGDGTs), brGDGT.FA.CYCL.I, brGDGT.FA.CYCL.II, brGDGT.FA.CYCL.III)
  
  colnames(brGDGT.FA.CYCL)[1] <- c("Label")
  
  return(brGDGT.FA.CYCL)
}

###----------------------------------------------- FUNCTION ENDS --------------------------------------------------------###











###----------------------------------------------------------------------------------------------------------------------###
###----------------------------------------------- 11-16. GDGTs FA-FUNCTION ----------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###


#Initialize Function
fGDGTs <- function(isoGDGTs){
  
  # Calculate the fGDGTs per GDGT
  fGDGT0    <-   GDGTs[,"GDGT.0"]   / (rowSums(GDGTs[,c("GDGT.0", "GDGT.1","GDGT.2","GDGT.3", "GDGT.4", "GDGT.4.2")]))
  fGDGT1    <-   GDGTs[,"GDGT.1"]   / (rowSums(GDGTs[,c("GDGT.0", "GDGT.1","GDGT.2","GDGT.3", "GDGT.4", "GDGT.4.2")]))
  fGDGT2    <-   GDGTs[,"GDGT.2"]   / (rowSums(GDGTs[,c("GDGT.0", "GDGT.1","GDGT.2","GDGT.3", "GDGT.4", "GDGT.4.2")]))
  fGDGT3    <-   GDGTs[,"GDGT.3"]   / (rowSums(GDGTs[,c("GDGT.0", "GDGT.1","GDGT.2","GDGT.3", "GDGT.4", "GDGT.4.2")]))
  fGDGT4    <-   GDGTs[,"GDGT.4"]   / (rowSums(GDGTs[,c("GDGT.0", "GDGT.1","GDGT.2","GDGT.3", "GDGT.4", "GDGT.4.2")]))
  fGDGT4.2  <-   GDGTs[,"GDGT.4.2"] / (rowSums(GDGTs[,c("GDGT.0", "GDGT.1","GDGT.2","GDGT.3", "GDGT.4", "GDGT.4.2")]))
  

  ###------------------------------------------ fGDGTs PRINT ------------------------------------------------------------###
  
  # put GDGTs in correct order for follow up scripts
  GDGTs_FA <- cbind(rownames(GDGTs), fGDGT0, fGDGT1, fGDGT2, fGDGT3, fGDGT4, fGDGT4.2)
  
  colnames(GDGTs_FA)[1] <- c("Label")
  
  return(GDGTs_FA)
}

###----------------------------------------------- FUNCTION ENDS --------------------------------------------------------###











###----------------------------------------------------------------------------------------------------------------------###
###----------------------------------------------- 17-19. OHGDGTs FA-FUNCTION -------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###


#Initialize Function
fOHGDGTs <- function(OHGDGTs){
  
  # Calculate the fGDGTs per GDGT
  fOHGDGT0    <-   GDGTs[,"OH-GDGT.0"]   / (rowSums(GDGTs[,c("OH-GDGT.0", "OH-GDGT.1","OH-GDGT.2")]))
  fOHGDGT1    <-   GDGTs[,"OH-GDGT.1"]   / (rowSums(GDGTs[,c("OH-GDGT.0", "OH-GDGT.1","OH-GDGT.2")]))
  fOHGDGT2    <-   GDGTs[,"OH-GDGT.2"]   / (rowSums(GDGTs[,c("OH-GDGT.0", "OH-GDGT.1","OH-GDGT.2")]))

  ###------------------------------------------ fGDGTs PRINT ------------------------------------------------------------###
  
  # put GDGTs in correct order for follow up scripts
  OHGDGTs_FA <- cbind(rownames(GDGTs), fOHGDGT0, fOHGDGT1, fOHGDGT2)
  
  colnames(OHGDGTs_FA)[1] <- c("Label")
  
  return(OHGDGTs_FA)
}

###----------------------------------------------- FUNCTION ENDS --------------------------------------------------------###











###----------------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------- 20-26. GMGTs FA-FUNCTION ------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###


#Initialize Function
fGMGTs <- function(GMGTs){
  
  # Calculate the fGDGTs per GDGT
  fGMGT1     <-   GMGTs[,"H1048"]    / (rowSums(GMGTs[,c("H1048", "H1034a","H1034b","H1034c", "H1020a", "H1020b", "H1020c")]))
  fGMGT2a    <-   GMGTs[,"H1034a"]   / (rowSums(GMGTs[,c("H1048", "H1034a","H1034b","H1034c", "H1020a", "H1020b", "H1020c")]))
  fGMGT2b    <-   GMGTs[,"H1034b"]   / (rowSums(GMGTs[,c("H1048", "H1034a","H1034b","H1034c", "H1020a", "H1020b", "H1020c")]))
  fGMGT2c    <-   GMGTs[,"H1034c"]   / (rowSums(GMGTs[,c("H1048", "H1034a","H1034b","H1034c", "H1020a", "H1020b", "H1020c")]))
  fGMGT3a    <-   GMGTs[,"H1020a"]   / (rowSums(GMGTs[,c("H1048", "H1034a","H1034b","H1034c", "H1020a", "H1020b", "H1020c")]))
  fGMGT3b    <-   GMGTs[,"H1020b"]   / (rowSums(GMGTs[,c("H1048", "H1034a","H1034b","H1034c", "H1020a", "H1020b", "H1020c")]))
  fGMGT3c    <-   GMGTs[,"H1020c"]   / (rowSums(GMGTs[,c("H1048", "H1034a","H1034b","H1034c", "H1020a", "H1020b", "H1020c")]))
  
  ###------------------------------------------ fGDGTs PRINT ------------------------------------------------------------###
  
  # put GDGTs in correct order for follow up scripts
  GMGTs_FA <- cbind(rownames(GDGTs), fGMGT1,fGMGT2a, fGMGT2b, fGMGT2c, fGMGT3a, fGMGT3b, fGMGT3c)
  
  colnames(GMGTs_FA)[1] <- c("Label")
  
  return(GMGTs_FA)
}

###----------------------------------------------- FUNCTION ENDS --------------------------------------------------------###



#**************************************************************************************************************************#
#******************************************************* END **************************************************************#
#**************************************************************************************************************************#