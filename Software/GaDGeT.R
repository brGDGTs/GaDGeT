#*************************************************      GaDGeT      **************************************************************
#*************************************************   CALCULATIONS   **************************************************************
#*************************************************        TS        **************************************************************


#-------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------- AUTHOR ------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------

# Author: Tobias Schneider
# Date: 05.12.2020
# Last modification: 09. September 2024
# Contact: tobiaschnei@gmail.com, www.drtobiasschneider.com

# Reference: Schneider, T., & Castaneda, I.S. (2024). "GaDGeT – GDGT calculations simplified: an adaptable R-toolbox 
# for rapid GDGT index calculations." Organic Geochemistry. DOI: xxxx/yyyy



# DISCLAIMER

#   This script is provided "as is" without any warranties.Please 
#   contact the author for troubleshooting or modifications.


#-------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------- SCRIPT DESCRIPTION --------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------

#   This script calculates fractional abundances and GDGT-indices 
#   from brGDGT and other GDGT data. It processes data from Excel 
#   files in the 'Input' directory and outputs CSV files containing 
#   the calculated results into the 'Output' directory.

#   For more information, please read the corresponding article in 
#   Organic Geochemistry "GaDGeT – GDGT calculations simplified: an 
#   adaptable R-toolbox for rapid GDGT index calculations" and the 
#   manual by Schneider and Castaneda (2024). And the corresponding 
#   software manual.


# Requirements:
#   - R version 3.5 or above
#   - Packages: stringr, readxl


#--------- TO PROVIDE BY THE USER ------------------


# Provide an excel file containing the different GDGT-peaks in the Input directory.
# If the calculation of amounts and concentration is wished, also the dry sediment weight, and the area and amount 
# added of the internal standard (IS) has to be provided.

# The best would be if you follow the structure of the example file.
# Make sure that the sheet (the tab in the excel file) is called "GDGTs".
# Make sure that you do not change the header names, the Code will extract all the info from the header.
# You can add as many files as you want to the "Input" directory, the algorithm will browse automatically through 
# the directory "Input" and find the data.

# The data will be written in .csv format into the "Output" folder.


# !!! start the script by double clicking on the GaDGeT.R file. Do not change folder names, nor move
# any files from the "Functions"-folder.



######################################################################################################################################################################
############################################# SCRIPT TABLE OF CONTENT ######################################################################################################
######################################################################################################################################################################

#*******************************
# A. WORKSPACE PREPARATION
#*******************************

#*******************************
# I. DATA PREPARATION
#*******************************

#*******************************
# II. FRACTIONAL ABUNDANCES
#*******************************

#*******************************
# III. INDEX-CALCULATIONS
#*******************************

#*******************************
# IV. CONCENTRATION-CALCULATIONS
#*******************************


#********************************************************************************************************************************************************************* 
#***************************************************** GADGET SCRIPT STARTS ******************************************************************************************
#*********************************************************************************************************************************************************************


######################################################################################################################################################################
############################################# A. WORKSPACE PREPARATION ###############################################################################################
######################################################################################################################################################################

# Clean workspace
rm(list = ls(all = TRUE))
cat("\014")  # Clear console
graphics.off()  # Close all graphics windows


# Set working directory
workingdir <- getwd() # Use default working directory

#uncomment below and add the workingdir mannually
#workingdir<-"C:/Users/..."

setwd(workingdir)

# ================ Load Required Packages ===================

packs <- c("stringr", "readxl", "readr")

# Install missing packages
install.packages(setdiff(packs, installed.packages()[, "Package"]))

# Load the packages
invisible(lapply(packs, library, character.only = TRUE))


# ================ Load Custom Functions ====================

# List of function files to source
function_files <- c("GDGT_FA-calculation_Functions.R", 
                    "brGDGT_INDEX-calculation_Functions.R",
                    "isoGDGT_INDEX-calculation_Functions.R",
                    "OHGDGT_INDEX-calculation_Functions.R",
                    "GMGT_INDEX-calculation_Functions.R",
                    "GDD_INDEX-calculation_Functions.R",
                    "Helper_Functions.R")

# Source all files in the list
invisible(lapply(function_files, function(file) source(file.path("Functions", file))))


######################################################################################################################################################################
########################################## I. DATA PREPARATION #######################################################################################################
######################################################################################################################################################################

# Get list of Excel files in the 'Input' directory
GDGT.files <- list.files(path = paste0(workingdir, "/Input/"), pattern = "\\.(xlsx|csv)$")

if (length(GDGT.files) == 0) {
  stop("No input files found in the 'Input' directory. Please add input files according to the template.")
}

###----------------------------------------------------------------------------------------------------------------------###
###------------------------------------------- READ DATA ----------------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

# Initialize list for data compilation
files_info <- read_and_process_files(GDGT.files, workingdir)# read in datafiles based on the helper function

data.sets <- files_info$data_sets
data.sets.names <- files_info$data_sets_names #lists all available datasets


###----------------------------------------------------------------------------------------------------------------------###
###------------------------------------- MAIN PROCESSING LOOP -----------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

# Set global precision for numeric outputs
options(digits = 15)

# build a loop to browse through all files and calculate all the FAs for all Excel sheets

for(f in 1:length(data.sets.names)){

# Choose the file according to the list provided above
data.sets.name <- data.sets.names[f]

###----------------------------------------------------------------------------------------------------------------------###
###---------------------------------------- PREPARE DATA ----------------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

# extract data from list, convert it into numeric matrix for calculations
GDGT.temp <- matrix(unlist(data.sets[[f]]),ncol = ncol(data.sets[[f]]), byrow = F)
GDGT.temp <- mapply(GDGT.temp, FUN = as.numeric)
GDGT.temp <- matrix(GDGT.temp,ncol=ncol(data.sets[[f]]))

# label columns and rows of new matrix 
rownames(GDGT.temp) <- unlist(data.sets[[f]][,1])
colnames(GDGT.temp) <- colnames(data.sets[[f]])



###----------------------------------------------------------------------------------------------------------------------###
###------------------------------------------- SEPARATION OF COMPOUNDS --------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

# === Select Relevant Data Columns ===

# Define the sets of compounds to extract
brGDGTs_cols <- c("IIIa.5Me", "IIIa.6Me","IIIa.7Me", "IIIb.5Me", "IIIb.6Me","IIIb.7Me", "IIIc.5Me", 
                  "IIIc.6Me","IIIc.7Me", "IIa.5Me", "IIa.6Me","IIa.7Me","IIb.5Me", "IIb.6Me","IIb.7Me", 
                  "IIc.5Me", "IIc.6Me","IIc.7Me", "Ia", "Ib", "Ic")

GDGTs_cols   <- c("GDGT.0", "GDGT.1", "OH-GDGT.0", "GDGT.2", "OH-GDGT.1", "2OH-GDGT.0", 
                "GDGT.3", "OH-GDGT.2", "GDGT.4","GDGT.4.2")

GMGTs_cols   <- c("H1048", "H1034a", "H1034b","H1034c", "H1020a", "H1020b", "H1020c")

GDDs_cols    <- c("isoGDD0", "isoGDD1","isoGDD2","isoGDD3", "isoGDDCren")

IS_cols      <- c("Label", "DEPTH", "AGE", "EXTRACTEDSAMPLEWEIGHT", "IS_AREA","IS_AMOUNT")


# column check, are all required columns available?

if (!all(c(brGDGTs_cols,GDGTs_cols,GMGTs_cols,GDDs_cols, IS_cols) %in% colnames(GDGT.temp))) {
  stop("The input file does not contain the required columns. Please use the column header names as provided in the template.")
}



# === Extract relevant data, filling NAs with 0 ===

brGDGTs <- GDGT.temp[, brGDGTs_cols, drop = FALSE]
GDGTs <- GDGT.temp[, GDGTs_cols, drop = FALSE]
GMGTs <- GDGT.temp[, GMGTs_cols, drop = FALSE]
GDDs <- GDGT.temp[, GDDs_cols, drop = FALSE]
IS <- GDGT.temp[, IS_cols, drop = FALSE]

brGDGTs[is.na(brGDGTs)] <- 0
GDGTs[is.na(GDGTs)] <- 0
GMGTs[is.na(GMGTs)] <- 0
GDDs[is.na(GDDs)] <- 0
IS[is.na(IS)] <- 0

#compile the compounds for concentration calculation later on
GDGTs.conc <- cbind(GDGTs,brGDGTs)



###----------------------------------------------------------------------------------------------------------------------###
###------------------------------------ CREATE STORAGE FOLDERS ----------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

# === Create Output Directories ===

base_dir <- paste0(workingdir, "/Output/", data.sets.name)
create_dir(base_dir)

# Directories for outputs
DirFA.br <- paste0(base_dir, "/FAs/brGDGTs/")
DirFA <- paste0(base_dir, "/FAs/")
DirIND <- paste0(base_dir, "/GDGT-INDICES/")
DirCONC <- paste0(base_dir, "/GDGT-CONCENTRATIONS/")

# Create directories if they do not exist
create_dir(DirFA.br)
create_dir(DirFA)
create_dir(DirIND)
create_dir(DirCONC)



######################################################################################################################################################################
############################################# II. FRACTIONAL ABUNDANCEs ###############################################################################################
######################################################################################################################################################################

###----------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------- FA CALCULATIONS -------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

#------ brDGTs ----------

# calculate the FA following 1. brGDGT_FA
brGDGT.FA            <- brGDGT_FA(brGDGTs = brGDGTs)

# calculate the FA following 1a. brGDGT.7Me_FA
brGDGT.7Me.FA        <- brGDGT.7Me_FA(brGDGTs = brGDGTs)

# calculate the FA following 2. brGDGT_MI_FA
brGDGT.MI.FA         <- brGDGT_MI_FA(brGDGTs = brGDGTs)

# calculate the FA following 3. brGDGT_METH_5MeP_FA
brGDGT.METH.5Mep.FA  <- brGDGT_METH_5Mep_FA(brGDGTs = brGDGTs)

# calculate the FA following 4. brGDGT_METH_6MeP_FA
brGDGT.METH.6Mep.FA  <- brGDGT_METH_6Mep_FA(brGDGTs = brGDGTs)

# calculate the FA following 4. brGDGT_METH_5Me_FA
brGDGT.METH.5Me.FA   <- brGDGT_METH_5Mep_FA(brGDGTs = brGDGTs)

# calculate the FA following 5. brGDGT_METH_6Me_FA
brGDGT.METH.6Me.FA   <- brGDGT_METH_6Me_FA(brGDGTs = brGDGTs)

# calculate the FA following 7. brGDGT_METH_FA
brGDGT.METH.FA       <- brGDGT_METH_FA(brGDGTs = brGDGTs)

# calculate the FA following 8. brGDGT_CYCL_FA
brGDGT.CYCL.FA       <- brGDGT_CYCL_FA(brGDGTs = brGDGTs)

# calculate the FA following 9. brGDGT_CYCL_5Me_FA
brGDGT.CYCL.5Me.FA   <- brGDGT_CYCL_5Me_FA(brGDGTs = brGDGTs)

# calculate the FA following 10. brGDGT_CYCL_6Me_FA
brGDGT.CYCL.6Me.FA   <- brGDGT_CYCL_6Me_FA(brGDGTs = brGDGTs)


#------ isoGDGTs ---------

# calculate the FA following 11
isoGDGTs.FA           <- isoGDGT_FA(isoGDGTs = GDGTs)


#------ OHDGTs ----------

# calculate the FA following 12.
OHGDGTs.FA         <- OHGDGT_FA(OHGDGTs = GDGTs)


#------ GMGTs ----------

# calculate the FA following 13.
GMGTs.FA         <- GMGT_FA(GMGTs = GMGTs)


###-------------------------------------------------------------------------------------------------------------------------###
###----------------------------------------- PRINT FA OUTPUT FILES ---------------------------------------------------------###
###-------------------------------------------------------------------------------------------------------------------------###


# Define a list of datasets and corresponding filenames
data_sets <- list(
  brGDGT.FA = brGDGT.FA,
  brGDGT.7Me.FA = brGDGT.7Me.FA,
  brGDGT.MI.FA = brGDGT.MI.FA,
  brGDGT.METH.5Mep.FA = brGDGT.METH.5Mep.FA,
  brGDGT.METH.6Mep.FA = brGDGT.METH.6Mep.FA,
  brGDGT.METH.5Me.FA = brGDGT.METH.5Me.FA,
  brGDGT.METH.6Me.FA = brGDGT.METH.6Me.FA,
  brGDGT.METH.FA = brGDGT.METH.FA,
  brGDGT.CYCL.FA = brGDGT.CYCL.FA,
  brGDGT.CYCL.5Me.FA = brGDGT.CYCL.5Me.FA,
  brGDGT.CYCL.6Me.FA = brGDGT.CYCL.6Me.FA,
  isoGDGTs.FA = isoGDGTs.FA,
  OHGDGTs.FA = OHGDGTs.FA,
  GMGTs.FA = GMGTs.FA
)

output_directory <- list(
  DirFA.br = DirFA.br,
  DirFA = DirFA
)

# write csv files into output directory using helper function.
export_data_to_csv(data_sets, output_directory, data.sets.name)


######################################################################################################################################################################
############################################### III. INDEX-CALCULATIONS ################################################################################################
######################################################################################################################################################################

###----------------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------- PREPARE DATASET ---------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

#cut out and shape the GDGTs ad the brGDGT Fractional Abundances for Index calculation

#rename the 7Me fractional abundances to avoid confusion for the software.
colnames(brGDGT.7Me.FA) <- paste0("7Me.",colnames(brGDGT.7Me.FA), sep="")

GDGTs                  <-   cbind(IS, GDGTs, GDDs, GMGTs, apply(brGDGT.FA[,-1],2,as.double),apply(brGDGT.7Me.FA[,-1],2,as.double))
GDGTs[is.na(GDGTs)]    <-   0
GDGTs[GDGTs=="NaN"]    <-   0
GDGTs                  <-   data.frame(GDGTs)


###----------------------------------------------------------------------------------------------------------------------###
###------------------------------------------------ CALCULATE INDICES ---------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

brGDGT.IND   <- brGDGT_INDICES(GDGTs = GDGTs)
isoGDGT.IND  <- isoGDGT_INDICES(GDGTs = GDGTs)
OHGDGT.IND   <- OHGDGT_INDICES(GDGTs = GDGTs)
GMGT.IND    <- GMGT_INDICES(GDGTs = GDGTs)
GDD.IND    <- GDD_INDICES(GDGTs = GDGTs)

###----------------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------- INDICES-PRINT -----------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###


# Define a list of data frames and corresponding file suffixes
indices.print <- list(
  list(data = brGDGT.IND, suffix = "BR-GDGT_INDICES"),
  list(data = isoGDGT.IND, suffix = "ISO-GDGT_INDICES"),
  list(data = OHGDGT.IND, suffix = "OH-GDGT_INDICES"),
  list(data = GMGT.IND, suffix = "GMGT_INDICES"),
  list(data = GDD.IND, suffix = "GDD_INDICES")
)

# Iterate over the list to prepare and write CSV files
lapply(indices.print, function(ind) {
  # Prepare the print file by combining the relevant columns
  ind_print <- cbind(
    Label = rownames(ind$data),
    mid.depth = GDGTs$DEPTH,
    Age = GDGTs$AGE,
    ind$data
  )
  
  # Write the CSV file into the Output directory
  write.csv(ind_print, row.names = FALSE, 
            file = paste0(DirIND, "/", data.sets.name, "_", ind$suffix, "_", Sys.Date(), ".csv"))
})


######################################################################################################################################################################
############################################### IV. CONCENTRATION-CALCULATIONS ########################################################################################
######################################################################################################################################################################

# here we calculate the concentration per GDGT compound based on the added internal standard with known amount.

###----------------------------------------------------------------------------------------------------------------------###
###----------------------------------------- CALCULATE AMOUNT AND CONCENTRATIONS ----------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

# calculate the concentration factor (concentration/area) of the Internal Standard IS
IS.factor <- IS[,"IS_AMOUNT"] / IS[,"IS_AREA"]

#set all inf values to NA
IS.factor[is.infinite(IS.factor)] <- NA

#Calculate the amount per substance and sample based on the Internal standard and add the sample information
GDGTs.amount <- GDGTs.conc*IS.factor

#Calculate the concentration per dry sediment mass
GDGTs.conc <- GDGTs.amount/IS[,"EXTRACTEDSAMPLEWEIGHT"]


# Prepare and save amounts and concentrations
GDGTs.amount.IS <- cbind(rownames(IS), IS[, 2:6], IS.factor, GDGTs.amount)
colnames(GDGTs.amount.IS)[1] <- "Label"
write.csv(GDGTs.amount.IS, file = paste0(DirCONC, data.sets.name, "_AMOUNT_", Sys.Date(), ".csv"), row.names = FALSE)

GDGTs.conc.IS <- cbind(rownames(IS), IS[, 2:6], IS.factor, GDGTs.conc)
colnames(GDGTs.conc.IS)[1] <- "Label"
write.csv(GDGTs.conc.IS, file = paste0(DirCONC, data.sets.name, "_CONC_", Sys.Date(), ".csv"), row.names = FALSE)



###----------------------------------------------------------------------------------------------------------------------###
###--------------------------------------- SAVE SESSION INFO FOR REPRODUCIBILITY ----------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###
# Save session info for reproducibility, no need to change anything

save_session_info("Output", data.sets.name)

}


#**************************************************************************************************************************#
#******************************************************* SCRIPT ENDS ******************************************************#
#**************************************************************************************************************************#