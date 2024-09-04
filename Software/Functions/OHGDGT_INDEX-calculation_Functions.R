#****************************************************  OHGDGTS INDICES  ********************************************************
#****************************************************     FUNCTIONS     ********************************************************
#****************************************************        TS         ********************************************************

#-------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------- AUTHOR ------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------

# Author: Tobias Schneider
# Date: 05.12.2020
# Last modification: 29. August 2024
# Contact: tobiaschnei@gmail.com, www.drtobiasschneider.com

# Reference: Schneider, T., & Castaneda, I.S. (2024). "GaDGeT – GDGT calculations simplified: an adaptable R-toolbox 
# for rapid GDGT index calculations." Organic Geochemistry. DOI: xxxx/yyyy



# DISCLAIMER

#   This script is provided "as is" without any warranties.Please 
#   contact the author for troubleshooting or modifications.

#-------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------- SCRIPT DESCRIPTION --------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------

# This script contains the functions used for the main script to calculate different GDGT indices.
#   For more information, please read the corresponding article in 
#   Organic Geochemistry "GaDGeT – GDGT calculations simplified: an 
#   adaptable R-toolbox for rapid GDGT index calculations" and the 
#   manual by Schneider and Castaneda (2024). And the corresponding 
#   software manual.
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
#           8.  MAAT.WU1:       Wu et al. (2024)
#           9.  MAAT.WU2:       Wu et al. (2024)
#          10.  OHC:            Varma et al. (2024)
#          11.  SST:VA1:        Varma et al. (2024)
#          12.  SST:VA2:        Varma et al. (2024)


############################################################################################################################
############################################# INDEX CALCULATIONS ###########################################################
############################################################################################################################

###----------------------------------------------------------------------------------------------------------------------###
###-------------------------------------------- INDEX CALC --------------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

OHGDGT_INDICES <- function(GDGTs){

  # Initialize dataframe with nrows from input file and 20 Index-columns
  
  #enter the amount of Indices here as "n"
  n= 12
  
  GDGT.IND <- data.frame(matrix(nrow = nrow(GDGTs),ncol = n))
  
  # Set rownames, take those from the input file
  row.names(GDGT.IND) <- rownames(GDGTs)
  
  # Set column names
  colnames(GDGT.IND)  <- c("PERC.OH.tot",
                           "OH0.1",
                           "SST.FI",
                           "RI.OH",
                           "RI.OH.",
                           "RI.OH.SST",
                           "RI.OH..SST",
                           "MAAT.WU1",
                           "MAAT.WU2",
                           "OHC",
                           "SST.VA1",
                           "SST.VA2")
  
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
  GDGT.IND$SST.FI         <-      ((-1/0.0076)*GDGT.IND$OH0.1) + (0.93/0.0076)
                                   

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
  
  ### 8
  #calculate MAAT.WU 1 Wu et al. (2024)
  GDGT.IND$MAAT.WU1    <-    28.55*GDGT.IND$RI.OH - 37.84
  
  ### 9
  #calculate MAAT.WU 2 Wu et al. (2024)
  GDGT.IND$MAAT.WU2    <-    32.39*GDGT.IND$RI.OH - 43.17
  
  ### 10
  #calculate SST.VA1 Varma et al. (2024)
  GDGT.IND$OHC       <-     (rowSums(GDGTs[,c("GDGT.2", "GDGT.3", "GDGT.4.2")])- GDGTs[,c("OH.GDGT.0")]) /
                             rowSums(GDGTs[,c("OH.GDGT.0", "OH.GDGT.1", "OH.GDGT.2", "GDGT.1","GDGT.2","GDGT.3", "GDGT.4.2")])
 
  ### 11
  #calculate SST.VA1 Varma et al. (2024)
  GDGT.IND$SST.VA1    <-    12.8261 + 21.7391*GDGT.IND$OHC
  
  ### 12
  #calculate SST.VA1 Varma et al. (2024)
  GDGT.IND$SST.VA1    <-    43.75- 1.25*sqrt(641-800*GDGT.IND$OHC)
  
  return(GDGT.IND)
}


#**************************************************************************************************************************#
#******************************************************* END **************************************************************#
#**************************************************************************************************************************#