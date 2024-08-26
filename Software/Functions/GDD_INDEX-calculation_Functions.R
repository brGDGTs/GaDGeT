#****************************************************   GMGTS INDICES   ********************************************************
#****************************************************     FUNCTIONS     ********************************************************
#****************************************************        TS         ********************************************************

#-------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------- AUTHOR -----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------

# Author: Tobias Schneider
# Date: 05.12.2020
# Last modification: 26. August 2024
# Email: tobiaschnei@gmail.com, www.drtobiasschneider.com

# DISCLAIMER

# The author does not guarantee for the functionality of this script. Please do not hesitate to get back to the author
# to report problems, or for troubleshooting.

#-------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------- SCRIPT DESCRIPTION --------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------

# This script contains the functions used for the main script to calculate different GDGT indices.
# The script contains the following function:

#--------- FUNCTION CALL:

#          GDGT_INDICES(GDGTs)

#--------- INDEX-CALCULATION-DESCRIPTIONS:

#           1.  brGMGTI:          Baxter et al. (2019)
#           2.  brGMGTI.MAAT:     Baxter et al. (2019)
#           3.  brGMGT.MAAT2:     Baxter et al. (2019)
#           4.  brGMGT%:          Baxter et al. (2021)
#           5.  DM.brGMGT:        Baxter et al. (2019)


############################################################################################################################
############################################# INDEX CALCULATIONS ###########################################################
############################################################################################################################

###----------------------------------------------------------------------------------------------------------------------###
###-------------------------------------------- INDEX CALC --------------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

GMGT_INDICES <- function(GDGTs){

  # Initialize dataframe with nrows from input file and 20 Index-columns
  
  #enter the amount of Indices here as "n"
  n= 5
  
  GDGT.IND <- data.frame(matrix(nrow = nrow(GDGTs),ncol = n))
  
  # Set rownames, take those from the input file
  row.names(GDGT.IND) <- rownames(GDGTs)
  
  # Set column names
  colnames(GDGT.IND)  <- c("brGMGTI",
                           "brGMGTI.MAAT",
                           "brGMGT.MAAT2",
                           "brGMGT.P",
                           "DMbrGMGT")
  
  GDGT.IND <- data.frame(GDGT.IND)
  
  ###-------------------------------------------- INDEX CALC --------------------------------------------------------------###
  
  ### 1
  #calculate brGMGTI Baxter et al., 2019 GCA
  GDGT.IND$brGMGTI          <-   rowSums(GDGTs[,c("H1020c", "H1034a", "H1034c")]) /
                                 rowSums(GDGTs[,c("H1020b","H1020c","H1034a", "H1034c", "H1048")])
  
  ### 2
  #calculate brGMGTI temp Baxter et al., 2019 GCA 
  GDGT.IND$brGMGTI.MAAT       <-   2.86 + 26.5*GDGT.IND$brGMGTI
  
  
  ### PREP
  fH1034a                   <-    GDGTs[,"H1034a"]/
                                  rowSums(GDGTs[,c("H1048", "H1034a","H1034b","H1034c", "H1020a", "H1020b", "H1020c")])
  
  fH1020a                   <-    GDGTs[,"H1020a"]/
                                  rowSums(GDGTs[,c("H1048", "H1034a","H1034b","H1034c", "H1020a", "H1020b", "H1020c")])
  
  fH1020c                   <-    GDGTs[,"H1020c"]/
                                  rowSums(GDGTs[,c("H1048", "H1034a","H1034b","H1034c", "H1020a", "H1020b", "H1020c")])
  
  ### 3
  #calculate GDGT SFS temp calib (Baxter et al., 2019)
  GDGT.IND$brGMGT.MAAT2       <-  1.18 + (0.47*fH1034a) + (0.12*fH1020a) + (0.5*fH1020c) 
  
  ### 4
  #calculate GDGT SFS temp calib (Baxter et al., 2019)
  GDGT.IND$brGMGT.P     <-  rowSums(GDGTs[,c("H1020a","H1020b","H1020c","H1034a", "H1034b","H1034c", "H1048")])/(rowSums(GDGTs[,c("Ia",
                                                   "Ib","Ic","IIa.5Me","IIb.5Me","IIc.5Me","IIIa.5Me","IIIb.5Me","IIIc.5Me",
                                                   "IIa.6Me","IIb.6Me","IIc.6Me","IIIa.6Me","IIIb.6Me","IIIc.6Me")])+
                                  rowSums(GDGTs[,c("H1020a","H1020b","H1020c","H1034a", "H1034b","H1034c", "H1048")]))
  
  GDGT.IND$DMbrGMGT          <-   rowSums(GDGTs[,c("H1048", "H1034b")]) /
    rowSums(GDGTs[,c("H1020a","H1020b","H1034b", "H1048")])
  
  
  return(GDGT.IND)
}


#**************************************************************************************************************************#
#******************************************************* END **************************************************************#
#**************************************************************************************************************************#