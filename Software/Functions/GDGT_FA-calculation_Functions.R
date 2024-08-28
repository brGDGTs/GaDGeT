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
#          3. brGDGT_METH_5Me_FA
#          4. brGDGT_METH_6Me_FA

#          5. brGDGT_CYCL_FA
#          6. brGDGT_CYCL_5Me_FA
#          7. brGDGT_CYCL_6Me_FA


#------ isoGDGTs ---------

#          8. fGDGT0
#          9. fGDGT1
#         10. fGDGT2
#         11. fGDGT3
#         12. fGDGT4
#         13. fGDGT4.2


#------ OHDGTs ----------

#         14. fOH.0
#         15. fOH.1
#         16. fOH.2


#------ GMGTs ----------

#         17. fGMGT1
#         18. fGMGT2a
#         19. fGMGT2b
#         20. fGMGT2c
#         21. fGMGT3a
#         22. fGMGT3b
#         23. fGMGT3c

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

#          5. Fractional Abundances, calculated acc. Raberg et al. (2021) CI:               IIa/(IIa+IIb+IIc+IIa'+IIb'+IIc'), same for I and III
#          6. Fractional Abundances, calculated acc. Raberg et al. (2021) CYC-5Me+:         IIa/(IIa+IIb+IIc), same for I and III
#          7. Fractional Abundances, calculated acc. Raberg et al. (2021) CYC-6Me+:         IIa'/(IIa'+IIb'+IIc'), same for I and III


#------ isoGDGTs ---------

#          8. Fractional Abundances of isoGDGTs, calculated as follows:                     GDGT0 / (GDGT0 + GDGT1 + GDGT2 + GDGT3 +GDGT4 + GDGT4.2)
#          9. Fractional Abundances of isoGDGTs, calculated as follows:                     GDGT1 / (GDGT0 + GDGT1 + GDGT2 + GDGT3 +GDGT4 + GDGT4.2)
#         10. Fractional Abundances of isoGDGTs, calculated as follows:                     GDGT2 / (GDGT0 + GDGT1 + GDGT2 + GDGT3 +GDGT4 + GDGT4.2)
#         11. Fractional Abundances of isoGDGTs, calculated as follows:                     GDGT3 / (GDGT0 + GDGT1 + GDGT2 + GDGT3 +GDGT4 + GDGT4.2)
#         12. Fractional Abundances of isoGDGTs, calculated as follows:                     GDGT4 / (GDGT0 + GDGT1 + GDGT2 + GDGT3 +GDGT4 + GDGT4.2)
#         13. Fractional Abundances of isoGDGTs, calculated as follows:                     GDGT4.2 / (GDGT0 + GDGT1 + GDGT2 + GDGT3 +GDGT4 + GDGT4.2)


#------ OHDGTs ----------

#         14. Fractional Abundances of isoGDGTs, calculated as follows:                     OHGDGT.0 / (OHGDGT.0 + OHGDGT.1 + OHGDGT.2)
#         15. Fractional Abundances of isoGDGTs, calculated as follows:                     OHGDGT.1 / (OHGDGT.0 + OHGDGT.1 + OHGDGT.2)
#         16. Fractional Abundances of isoGDGTs, calculated as follows:                     OHGDGT.2 / (OHGDGT.0 + OHGDGT.1 + OHGDGT.2)


#------ GMGTs ----------

#         17. Fractional Abundances of fGMGT1 (H1048), calculated as follows:                H1048  / (H1048 + H1034a + H1034b + H1034c + H1020a + H1020b + H1020c)
#         18. Fractional Abundances of fGMGT2a (H1034a), calculated as follows:              H1034a / (H1048 + H1034a + H1034b + H1034c + H1020a + H1020b + H1020c)
#         19. Fractional Abundances of fGMGT2b (H1034b), calculated as follows:              H1034b / (H1048 + H1034a + H1034b + H1034c + H1020a + H1020b + H1020c)
#         20. Fractional Abundances of fGMGT2c (H1034c), calculated as follows:              H1034c / (H1048 + H1034a + H1034b + H1034c + H1020a + H1020b + H1020c)
#         21. Fractional Abundances of fGMGT3a (H1020a), calculated as follows:              H1020a / (H1048 + H1034a + H1034b + H1034c + H1020a + H1020b + H1020c)
#         22. Fractional Abundances of fGMGT3b (H1020b), calculated as follows:              H1020b / (H1048 + H1034a + H1034b + H1034c + H1020a + H1020b + H1020c)
#         23. Fractional Abundances of fGMGT3c (H1020c), calculated as follows:              H1020c / (H1048 + H1034a + H1034b + H1034c + H1020a + H1020b + H1020c)




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
  brGDGT.FA.MI<-cbind(rownames(brGDGTs), brGDGT.FA.MI.a[,1], brGDGT.FA.MI.b[,1], brGDGT.FA.MI.c[,1],
                        brGDGT.FA.MI.a[,2:3], brGDGT.FA.MI.b[,2:3], brGDGT.FA.MI.c[,2:3],
                        brGDGT.FA.MI.a[,4:5], brGDGT.FA.MI.b[,4:5], brGDGT.FA.MI.c[,4:5])
  
  colnames(brGDGT.FA.MI)[1:4] <- c("Label","Ia","Ib","Ic")
  
  
  return(brGDGT.FA.MI)
}

###----------------------------------------------- FUNCTION ENDS ------------------------------------------------------###



###----------------------------------------------------------------------------------------------------------------------###
###--------------------------------- 3. brGDGTs FA METHYLIZATION 5Me-FUNCTION -------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###


#Initialize Function
brGDGT_METH_5Me_I_FA <- function(brGDGTs){
  
  # prepare data subsets
  brGDGT.METH.I.a    <- brGDGTs[,c("Ia","IIa.5Me","IIIa.5Me")]
  brGDGT.METH.I.b    <- brGDGTs[,c("Ib","IIb.5Me","IIIb.5Me")]
  brGDGT.METH.I.c    <- brGDGTs[,c("Ic","IIc.5Me","IIIc.5Me")]
  
  # initialize FA matrices
  brGDGT.FA.METH.I.a <- brGDGT.METH.I.a
  brGDGT.FA.METH.I.b <- brGDGT.METH.I.b
  brGDGT.FA.METH.I.c <- brGDGT.METH.I.c
  
  # for loops browsing through rows and cols and calculating the FAs and saving them in the matrix
  for(c in 1: ncol(brGDGT.FA.METH.I.a)){
    
    brGDGT.FA.METH.I.a[,c] <- brGDGT.METH.I.a[,c]/rowSums(brGDGT.METH.I.a)
    brGDGT.FA.METH.I.b[,c] <- brGDGT.METH.I.b[,c]/rowSums(brGDGT.METH.I.b)
    brGDGT.FA.METH.I.c[,c] <- brGDGT.METH.I.c[,c]/rowSums(brGDGT.METH.I.c)
    
  }
  
  
  ###---------------------------------------------- FA METH PRINT -------------------------------------------------------###
  
  # put GDGTs in correct order for follow up scripts
  brGDGT.FA.METH.I<-cbind(rownames(brGDGTs), brGDGT.FA.METH.I.a[,1], brGDGT.FA.METH.I.b[,1], brGDGT.FA.METH.I.c[,1],
                        brGDGT.FA.METH.I.a[,2:3], brGDGT.FA.METH.I.b[,2:3], brGDGT.FA.METH.I.c[,2:3])
  
  colnames(brGDGT.FA.METH.I)[1:4] <- c("Label","Ia","Ib","Ic")
  
  return(brGDGT.FA.METH.I)
}

###----------------------------------------------- FUNCTION ENDS --------------------------------------------------------###











###----------------------------------------------------------------------------------------------------------------------###
###--------------------------------- 4. brGDGTs FA METHYLIZATION 6Me-FUNCTION -------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###


#Initialize Function
brGDGT_METH_6Me_FA <- function(brGDGTs){
  
  # prepare data subsets
  brGDGT.METH.a    <- brGDGTs[,c("Ia","IIa.6Me","IIIa.6Me")]
  brGDGT.METH.b    <- brGDGTs[,c("Ib","IIb.6Me","IIIb.6Me")]
  brGDGT.METH.c    <- brGDGTs[,c("Ic","IIc.6Me","IIIc.6Me")]
  
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
  
  
  ###---------------------------------------------- FA METH PRINT -------------------------------------------------------###
  
  # put GDGTs in correct order for follow up scripts
  brGDGT.FA.METH<-cbind(rownames(brGDGTs), brGDGT.FA.METH.a[,1], brGDGT.FA.METH.b[,1], brGDGT.FA.METH.c[,1],
                        brGDGT.FA.METH.a[,2:3], brGDGT.FA.METH.b[,2:3], brGDGT.FA.METH.c[,2:3])
  
  colnames(brGDGT.FA.METH)[1:4] <- c("Label","Ia","Ib","Ic")
  
  return(brGDGT.FA.METH)
}

###----------------------------------------------- FUNCTION ENDS --------------------------------------------------------###











###----------------------------------------------------------------------------------------------------------------------###
###---------------------------------- 5. brGDGTs FA CYCLIZATION-FUNCTION ------------------------------------------------###
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
###-------------------------------- 6. brGDGTs FA CYCLIZATION 5Me-FUNCTION ----------------------------------------------###
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
###-------------------------------- 7. brGDGTs FA CYCLIZATION 6Me-FUNCTION ----------------------------------------------###
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
###----------------------------------------------- 8-13. GDGTs FA-FUNCTION ----------------------------------------------###
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
###----------------------------------------------- 14-16. OHGDGTs FA-FUNCTION -------------------------------------------###
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
###-------------------------------------------------- 17-23. GMGTs FA-FUNCTION ------------------------------------------###
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