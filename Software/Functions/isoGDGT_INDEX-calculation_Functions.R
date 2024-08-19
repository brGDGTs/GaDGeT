#****************************************************  isoGDGTS INDICES ********************************************************
#****************************************************     FUNCTIONS     ********************************************************
#****************************************************        TS         ********************************************************

#-------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------- AUTHOR -----------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------

# Author: Tobias Schneider
# Date: 05.12.2020
# Last modification: 19. August 2024
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

#          isoGDGT_INDICES(GDGTs)

#--------- INDEX-CALCULATION-DESCRIPTIONS:

#           1.  TEX86:          Schouten et al. (2002)
#           2.  TEX.CASC:       Castaneda and Schouten (2011, 2015)
#           3.  TEX.PW:         Powers et al. (2010)
#           4.  TEX.TIER:       Tierney et al. (2010)
#           5.  TEX.KIM:        Kim et al. (2008)
#           6.  TEX.L.86:       Kim et al (2010)
#           7.  TEX.H.86:       Kim et al (2010)
#           8.  TEX.L.SST:      Kim et al (2010)
#           9.  TEX.H.SST:      Kim et al (2010)
#          10.  TEX.OH.86:      Varma et al (2024)
#          11.  TEX.OH.86.SST:  Varma et al (2024) 
#          12.  RI.SMPL:        Zhang et al. (2016)
#          13.  RI.TEX:         Zhang et al. (2016)
#          14.  Delta.RI:       Zhang et al. (2016)
#          15.  M.IND:          Zhang et al. (2011)
#          16.  ANOX:           Blaga et al. (2009)
#          17.  EXDEPTH:        Taylor et al. (2013)
#          18.  GDGT2.GDGT4.2:  
#          19.  PERC.GDGT2:  




############################################################################################################################
############################################# INDEX CALCULATIONS ###########################################################
############################################################################################################################

###----------------------------------------------------------------------------------------------------------------------###
###-------------------------------------------- INDEX CALC --------------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

isoGDGT_INDICES <- function(GDGTs){

  # Initialize dataframe with nrows from input file and 20 Index-columns
  
  #enter the amount of Indices here as "n"
  n= 19
  
  GDGT.IND <- data.frame(matrix(nrow = nrow(GDGTs),ncol = n))
  
  # Set rownames, take those from the input file
  row.names(GDGT.IND) <- rownames(GDGTs)
  
  # Set column names
  colnames(GDGT.IND)  <- c("TEX86",
                           "TEX.CASC",
                           "TEX.PW",
                           "TEX.TIER",
                           "TEX.KIM",
                           "TEX.L.86",
                           "TEX.H.86",
                           "TEX.L.SST",
                           "TEX.H.SST",
                           "TEX.OH.86",
                           "TEX.OH.86.SST",
                           "RI.SMPL",
                           "RI.TEX",
                           "D.RI",
                           "M.IND",
                           "ANOX",
                           "EXDEPTH",
                           "GDGT2.GDGT4.2",
                           "PERC.GDGT2")
  
  GDGT.IND <- data.frame(GDGT.IND)
  
  ###-------------------------------------------- INDEX CALC --------------------------------------------------------------###
  
  ### 1
  #calculate TEX86 (Schouten et al., 2002)
  GDGT.IND$TEX86           <-   rowSums(GDGTs[,c("GDGT.2", "GDGT.3", "GDGT.4.2")]) /
                                rowSums(GDGTs[,c("GDGT.1","GDGT.2", "GDGT.3", "GDGT.4.2")])
  
  ### 2
  #calculate TEX temp C and S calib (Castaneda & Schouten 2011; 2015)
  GDGT.IND$TEX.CASC        <-   49.032*GDGT.IND$TEX86-10.989
  
  ### 3
  #calculate TEX temp Powers calib (Powers et al., 2010)
  GDGT.IND$TEX.PW          <-   55.781*GDGT.IND$TEX86-13.949
  
  ### 4
  #calculate TEX temp Tierney calib (Tierney et al., 2010)
  GDGT.IND$TEX.TIER        <-   39.781*GDGT.IND$TEX86-4.0133
  
  ### 5
  #calculate TEX temp Kim et al. (2008) calib (Kim et al., 2008)
  GDGT.IND$TEX.KIM         <-   56.2*GDGT.IND$TEX86-10.78
  
  ### 6
  #calculate TEX Kim et al. (2010) GDGT Index 1 (low temp)
  GDGT.IND$TEX.L.86      <-    log(GDGTs[,c("GDGT.2")] /
                                     rowSums(GDGTs[,c("GDGT.1","GDGT.2","GDGT.3")]),
                                     base = 10)
  
  ### 7
  #calculate TEX Kim et al. (2010) GDGT Index 2 (high temp)
  GDGT.IND$TEX.H.86      <-    log(rowSums(GDGTs[,c("GDGT.2", "GDGT.3", "GDGT.4.2")]) /
                                     rowSums(GDGTs[,c("GDGT.1","GDGT.2","GDGT.3", "GDGT.4.2")]),
                                     base = 10)
  ### 8
  #calculate temp Kim et al. (2010) low temp calib
  GDGT.IND$TEX.L.SST       <-    67.5*GDGT.IND$TEX.L.86+46.9
  
  ### 9
  #calculate TEX temp Kim et al. (2010) high temp
  GDGT.IND$TEX.H.SST       <-    68.4*GDGT.IND$TEX.H.86+38.6
  
  ### 10
  #calculate RingIndex Sample (Zhang et al., 2016)
  GDGT.IND$RI.SMPL            <-   (0*GDGTs[,"GDGT.0"]+1*GDGTs[,"GDGT.1"]+2*GDGTs[,"GDGT.2"]+3*GDGTs[,"GDGT.3"]+4*GDGTs[,"GDGT.4"]+4*GDGTs[,"GDGT.4.2"]) / 
                                rowSums(GDGTs[,c("GDGT.0","GDGT.1","GDGT.2","GDGT.3","GDGT.4","GDGT.4.2")])

  ### 11
  #calculate RingIndex TEX (Zhang et al., 2016)
  GDGT.IND$RI.TEX          <-   -0.77*GDGT.IND$TEX86+(3.32*(GDGT.IND$TEX86)^2)+1.59
 
  ### 12
  #calculate DeltaRI (Zhang et al., 2016)
  GDGT.IND$D.RI        <-   abs(GDGT.IND$RI.TEX-GDGT.IND$RI.SMPL)
  
  ### 13
  #calculate Methane Index (Zhang et al. 2011)
  GDGT.IND$M.IND           <-   rowSums(GDGTs[,c("GDGT.1", "GDGT.2", "GDGT.3")]) /
                                rowSums(GDGTs[,c("GDGT.1","GDGT.2","GDGT.3", "GDGT.4", "GDGT.4.2")])
  
  ### 14
  #calculate GDGT0/GDGT4, anoxia
  GDGT.IND$ANOX            <-   GDGTs[,"GDGT.0"] / GDGTs[,"GDGT.4"]
  
  
  ### 15
  #calculate GDGT 2/3 Taylor et al., 2013
  GDGT.IND$EXDEPTH     <-   GDGTs[,"GDGT.2"] / GDGTs[,"GDGT.3"]
  
  ### 16
  #calculate GDGT 2/cren 
  GDGT.IND$GDGT2.GDGT4.2   <-   GDGTs[,"GDGT.2"] / GDGTs[,"GDGT.4.2"]
  
  ### 17
  #calculate percent GDGT.2
  GDGT.IND$PERC.GDGT2      <-   GDGTs[,"GDGT.2"] / 
                                rowSums(GDGTs[,c("GDGT.0","GDGT.1","GDGT.2","GDGT.3","GDGT.4","GDGT.4.2")]) *100
  
  return(GDGT.IND)
}


#**************************************************************************************************************************#
#******************************************************* END **************************************************************#
#**************************************************************************************************************************#