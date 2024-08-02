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

#           1.  PERC.OHtot:     Feitz et al. (2013)
#           2.  OH1318.1316:    Feitz et al. (2013)
#           2.  SST.FI:         Feitz et al. (2013)
#           3.  RI.OH:          Lü et al. (2015)
#           4.  RI.OH':         Lü et al. (2015)
#           5.  fOH.0:          Lü et al. (2015)
#           6.  fOH.2:          Lü et al. (2015)
#           7.  RI.OH.SST:      Lü et al. (2015)
#           8.  RI.OH'.SST:     Lü et al. (2015)
#           9.  OH.2.SST:       Lü et al. (2015)
#          10.  OH.0.SST:       Lü et al. (2015)
#          11.  tot.OH.SST:     Lü et al. (2015)
#          12.  OH.0.SST2:      Lü et al. (2020)
#          13.  RI.OH.SST2:     Lü et al. (2020)



############################################################################################################################
############################################# INDEX CALCULATIONS ###########################################################
############################################################################################################################

###----------------------------------------------------------------------------------------------------------------------###
###-------------------------------------------- INDEX CALC --------------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

OHGDGT_INDICES <- function(GDGTs){

  # Initialize dataframe with nrows from input file and 20 Index-columns
  
  #enter the amount of Indices here as "n"
  n= 14
  
  GDGT.IND <- data.frame(matrix(nrow = nrow(GDGTs),ncol = n))
  
  # Set rownames, take those from the input file
  row.names(GDGT.IND) <- rownames(GDGTs)
  
  # Set column names
  colnames(GDGT.IND)  <- c("PERC.OH.tot",
                           "OH1.2",
                           "SST.FI",
                           "RI.OH",
                           "RI.OH'",
                           "fOH.0",
                           "fOH.2",
                           "RI.OH.SST",
                           "RI.OH'.SST",
                           "OH.2.SST",
                           "OH.0.SST",
                           "tot.OH.SST",
                           "OH.0.SST2",
                           "RI.OH.SST2")
  
  GDGT.IND <- data.frame(GDGT.IND)
  
  ###-------------------------------------------- INDEX CALC --------------------------------------------------------------###
  
  ### 1
  #calculate percent total OH-GDGTs vs iGDGTs (Feitz et al., 2013)
  GDGT.IND$PERC.OH.tot   <-      100*((rowSums(GDGTs[,c("OH.GDGT.0", "OH.GDGT.1", "OH.GDGT.2")])) /
                                       rowSums(GDGTs[,c("OH.GDGT.0", "OH.GDGT.1", "OH.GDGT.2", "GDGT.1","GDGT.2","GDGT.3", "GDGT.4", "GDGT.4.2")]))
  
  #calculate OHGDGT1318/1316 index (Feitz et al., 2013)
  GDGT.IND$OH1.2         <-      (rowSums(GDGTs[,c("OH.GDGT.0")]) /
                                        rowSums(GDGTs[,c("OH.GDGT.0", "OH.GDGT.1", "OH.GDGT.2")]))
  
  
  #calculate SST.FI=-131.579×OH1.2+122.368 (Feitz et al., 2013)
  GDGT.IND$SST.FI         <-      ((-131.579*GDGT.IND$OH1.2)+122.368)
                                   

  ### 2
  #calculate ring index of OH-GDGTs (Lü et al., 2015 eq. 1)
  GDGT.IND$RI.OH         <-     (GDGTs[,c("OH.GDGT.1")] + (2*GDGTs[,c("OH.GDGT.2")])) / 
                                 rowSums(GDGTs[,c("OH.GDGT.1","OH.GDGT.2")])
  
  ### 3
  #calculate RI-OH' (Lü et al., 2015 eq. 13)
  GDGT.IND$RI.OH.        <-    (GDGTs[,c("OH.GDGT.1")] + (2*GDGTs[,c("OH.GDGT.2")])) / 
                                rowSums(GDGTs[,c("OH.GDGT.0","OH.GDGT.1","OH.GDGT.2")])
  
  ### 4
  #calculate ratio OH-0/total OHs (Lü et al., 2015)
  GDGT.IND$fOH.0         <-     GDGTs[,c("OH.GDGT.0")] / 
                                rowSums(GDGTs[,c("OH.GDGT.0","OH.GDGT.1","OH.GDGT.2")])
  
  ### 5
  #calculate ratio OH-2/total OHs (Lü et al., 2015)
  GDGT.IND$fOH.2         <-     GDGTs[,c("OH.GDGT.2")] / 
                                rowSums(GDGTs[,c("OH.GDGT.0","OH.GDGT.1","OH.GDGT.2")])
  
  ### 6
  #calculate SST based on RI-OH (Lü et al., 2015 eq. 2)
  GDGT.IND$RI.OH.SST     <-    (GDGT.IND$RI.OH-0.92) / 0.028
  
  ### 7
  #calculate SST based RI-OH' (Lü et al., 2015 eq. 14)
  GDGT.IND$RI.OH..SST    <-    (GDGT.IND$RI.OH.-0.1) / 0.0382
  
  ### 8
  #calculate SST based on OH-2 to total OHs (Lü et al., 2015 eq. 3)
  GDGT.IND$OH.2.SST      <-    (GDGT.IND$fOH.2 + 0.25) / 0.029
  
  ### 9
  #calculate SST based on OH-0 to total OHs (Lü et al., 2015 eq. 4)
  GDGT.IND$OH.0.SST      <-   (GDGT.IND$fOH.0 - 0.14) / (-0.004)
  
  ### 10
  #calculate SST based on total OHs to total OH+isoGDGTs (Lü et al., 2015 eq. 5)
  GDGT.IND$tot.OH.SST    <-   ((GDGT.IND$PERC.OH.tot/100) - 0.14) / (-0.004)
  
  ### 11
  ##calculate SST based on OH-0 to total OHs (Lü et al., 2020 eq. 1)
  GDGT.IND$OH.0.SST2     <-   (GDGT.IND$OH.0.SST-0.6138) / (-0.1636)
 
  ### 12
  #calculate SST based RI-OH' (Lü et al., 2020 eq. 2)
  GDGT.IND$RI.OH.SST2   <-   (GDGT.IND$RI.OH.-0.5061) / 0.2210
  
  
  return(GDGT.IND)
}


#**************************************************************************************************************************#
#******************************************************* END **************************************************************#
#**************************************************************************************************************************#