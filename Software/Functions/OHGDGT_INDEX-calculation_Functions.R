#****************************************************  OHGDGTS INDICES  ********************************************************
#****************************************************     FUNCTIONS     ********************************************************
#****************************************************        TS         ********************************************************

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

# This script contains the functions used for the main script to calculate different GDGT indices.
# The script contains the following function:

#--------- FUNCTION CALL:

#          OHGDGT_INDICES(GDGTs)

#--------- INDEX-CALCULATION-DESCRIPTIONS:

#           1.  PERC.OHtot:     Huguet et al. (2013)
#           2.  OH1318.1316:    Fietz et al. (2013)
#           3.  SST.FI:         Fietz et al. (2013)
#           4.  RI.OH:          Lü et al. (2015)
#           5.  RI.OH':         Lü et al. (2015)
#           6.  RI.OH.SST:      Lü et al. (2015)
#           7.  RI.OH'.SST:     Lü et al. (2015)




############################################################################################################################
############################################# INDEX CALCULATIONS ###########################################################
############################################################################################################################

###----------------------------------------------------------------------------------------------------------------------###
###-------------------------------------------- INDEX CALC --------------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

OHGDGT_INDICES <- function(GDGTs){

  # Initialize dataframe with nrows from input file and 20 Index-columns
  
  #enter the amount of Indices here as "n"
  n= 7
  
  GDGT.IND <- data.frame(matrix(nrow = nrow(GDGTs),ncol = n))
  
  # Set rownames, take those from the input file
  row.names(GDGT.IND) <- rownames(GDGTs)
  
  # Set column names
  colnames(GDGT.IND)  <- c("PERC.OH.tot",
                           "OH0.1",
                           "SST.FI",
                           "RI.OH",
                           "RI.OH'",
                           "RI.OH.SST",
                           "RI.OH'.SST")
  
  GDGT.IND <- data.frame(GDGT.IND)
  
  ###-------------------------------------------- INDEX CALC --------------------------------------------------------------###
  
  ### 1
  #calculate percent total OH-GDGTs vs iGDGTs (Fietz et al., 2013)
  GDGT.IND$PERC.OH.tot   <-      100*((rowSums(GDGTs[,c("OH.GDGT.0", "OH.GDGT.1", "OH.GDGT.2")])) /
                                       rowSums(GDGTs[,c("OH.GDGT.0", "OH.GDGT.1", "OH.GDGT.2", "GDGT.1","GDGT.2","GDGT.3", "GDGT.4", "GDGT.4.2")]))
  
  #calculate OHGDGT1318/1316 index (Fietz et al., 2013)
  GDGT.IND$OH0.1         <-      (GDGTs[,c("OH.GDGT.0")] /
                                        rowSums(GDGTs[,c("OH.GDGT.0", "OH.GDGT.1")]))
  
  
  #calculate SST.FI=-131.579×OH1.2+122.368 (Fietz et al., 2013, eq. 7) 
  GDGT.IND$SST.FI         <-      (((-1/0.0076)*GDGT.IND$OH1.2) + (0.93/0.0076))
                                   

  ### 2
  #calculate ring index of OH-GDGTs (Lü et al., 2015 eq. 1)
  GDGT.IND$RI.OH         <-     (GDGTs[,c("OH.GDGT.1")] + (2*GDGTs[,c("OH.GDGT.2")])) / 
                                 rowSums(GDGTs[,c("OH.GDGT.1","OH.GDGT.2")])
  
  ### 3
  #calculate RI-OH' (Lü et al., 2015 eq. 13)
  GDGT.IND$RI.OH.        <-    (GDGTs[,c("OH.GDGT.1")] + (2*GDGTs[,c("OH.GDGT.2")])) / 
                                rowSums(GDGTs[,c("OH.GDGT.0","OH.GDGT.1","OH.GDGT.2")])
  
  ### 6
  #calculate SST based on RI-OH (Lü et al., 2015 eq. 11)
  GDGT.IND$RI.OH.SST     <-    (((1/0.018)*GDGT.IND$RI.OH) - (1.11/0.018))
  
  ### 7
  #calculate SST based RI-OH' (Lü et al., 2015 eq. 14)
  GDGT.IND$RI.OH..SST    <-    (((1/0.0382)*GDGT.IND$RI.OH.) - (0.1/0.0382))
  

  return(GDGT.IND)
}


#**************************************************************************************************************************#
#******************************************************* END **************************************************************#
#**************************************************************************************************************************#