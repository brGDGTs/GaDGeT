#****************************************************   GDDs INDICES    ********************************************************
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

#           1.  RI.GDD:           Hingley et al. (2024), eq. 2
#           2.  GDD%:             Hingley et al. (2024), eq. 3
#           3.  GDD%tot:          Hingley et al. (2024), eq. 4



############################################################################################################################
############################################# INDEX CALCULATIONS ###########################################################
############################################################################################################################

###----------------------------------------------------------------------------------------------------------------------###
###-------------------------------------------- INDEX CALC --------------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

GDD_INDICES <- function(GDGTs){

  # Initialize dataframe with nrows from input file and 20 Index-columns
  
  #enter the amount of Indices here as "n"
  n= 3
  
  GDGT.IND <- data.frame(matrix(nrow = nrow(GDGTs),ncol = n))
  
  # Set rownames, take those from the input file
  row.names(GDGT.IND) <- rownames(GDGTs)
  
  # Set column names
  colnames(GDGT.IND)  <- c("RI.GDD",
                           "GDD.P",
                           "GDD.Ptot")
  
  GDGT.IND <- data.frame(GDGT.IND)
  
  ###-------------------------------------------- INDEX CALC --------------------------------------------------------------###
  
  ### 1
  #calculate RI.GDD (Hingley et al., 2024)
  GDGT.IND$RI.GDD          <-   (GDGTs$isoGDD1+2*GDGTs$isoGDD2+3*GDGTs$isoGDD3+4*GDGTs$isoGDDCren) /
                                 rowSums(GDGTs[,c("isoGDD0","isoGDD1","isoGDD2", "isoGDD3", "isoGDDCren")])

  ### 2
  #calculate GDD% (Hingley et al., 2024) 
  GDGT.IND$GDD.P           <-   rowSums(GDGTs[,c("isoGDD0","isoGDD1","isoGDD2", "isoGDD3", "isoGDDCren")]) /
                                (rowSums(GDGTs[,c("isoGDD0","isoGDD1","isoGDD2", "isoGDD3", "isoGDDCren")]) +  
                                           rowSums(GDGTs[,c("GDGT.0","GDGT.1","GDGT.2","GDGT.3","GDGT.4","GDGT.4.2")]))
  
  ### 3
  #calculate GDD% total (Hingley et al., 2024)
  GDGT.IND$GDD.Ptot       <-   rowSums(GDGTs[,c("isoGDD0","isoGDD1","isoGDD2", "isoGDD3", "isoGDDCren")]) /
                              (rowSums(GDGTs[,c("isoGDD0","isoGDD1","isoGDD2", "isoGDD3", "isoGDDCren")]) +  
                                       rowSums(GDGTs[,c("GDGT.0","GDGT.1","GDGT.2","GDGT.3","GDGT.4","GDGT.4.2")]) +
                                       rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me","IIc.5Me","IIc.6Me","IIIa.5Me","IIIa.6Me","IIIb.5Me","IIIb.6Me","IIIc.5Me","IIIc.6Me")]))

  
  
  return(GDGT.IND)
}


#**************************************************************************************************************************#
#******************************************************* END **************************************************************#
#**************************************************************************************************************************#