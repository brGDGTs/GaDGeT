#*************************************************      GaDGeT      **************************************************************
#*************************************************   CALCULATIONS   **************************************************************
#*************************************************        TS        **************************************************************


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

#   This script calculates fractional abundances and GDGT-indices 
#   from brGDGT and other GDGT data. It processes data from Excel 
#   files in the 'Input' directory and outputs CSV files containing 
#   the calculated results into the 'Output' directory.

#   For more information, please read the corresponding article in 
#   Organic Geochemistry "GaDGeT – GDGT calculations simplified: an 
#   adaptable R-toolbox for rapid GDGT index calculations" and the 
#   manual by Schneider and Castaneda (2024).


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
# any files from the "brGDGTs_Functions"-folder.


#-------------------------------- FUNCTION DESCRIPTION -----------------------------------------------------------

# the calculation functions are sourced from two files, below.


#---------------------------------------------------------------
#--------- FRACTIONAL ABUNDANCE: -------------------------------
#---------------------------------------------------------------

#          contains 13 Functions:

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
#         10. brGDGT_CYCL_6Me_FA


#------ isoGDGTs ---------

#         11. fGDGTs


#------ OHDGTs ----------

#         12. fOHGDGTs


#------ GMGTs ----------

#         13. fGMGTs


#---------------------------------------------------------------
#--------- GDGT INDICES: -------------------------------
#---------------------------------------------------------------

#          contains 5 function files:

#--------- FUNCTION CALL:

#         1. brGDGT_INDICES(brGDGTs)
#         2. isoGDGT_INDICES(isoGDGTs)
#         3. OHGDGT_INDICES (OHGDGTs)
#         4. GMGT_INDICES(GMGTs)
#         5. GDD_INDICES(GDDs)

#--------- GDGT AMOUNTS and CONCENTRATIONS ------------------

#          these calculations are directly implemented in this master file:

#--------- FUNCTION CALL:

#          no extra call needed, implemented in the master file.


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


# Set working directory (you can change to `choose.dir()` for flexibility)
workingdir <- getwd() # Use default working directory
workingdir<-"C:/Users/tobia/Dropbox/UMASS/Papers/ongoing/GaDGeT/GaDGeT/GaDGeT/Software/"

setwd(workingdir)


# ================ Load Required Packages ===================


packs<-c("stringr", "readxl")

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
                    "GDD_INDEX-calculation_Functions.R")

# Source all files in the list
invisible(lapply(function_files, function(file) source(file.path("Functions", file))))


# ================ Helper Functions =========================
# Function to create directories if they don't exist
create_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}



######################################################################################################################################################################
########################################## I. DATA PREPARATION #######################################################################################################
######################################################################################################################################################################

# Get list of Excel files in the 'Input' directory
GDGT.files <- list.files(path = paste0(workingdir, "/Input/"), pattern = "\\.xlsx$")

if (length(GDGT.files) == 0) {
  stop("No input files found in the 'Input' directory. Please add input files according to the template.")
}

###----------------------------------------------------------------------------------------------------------------------###
###------------------------------------------- READ DATA ----------------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

# Initialize list for data compilation
data.sets<-list()

# Loop through each file and read data
for (i in seq_along(GDGT.files)) {
  file_path <- paste0(workingdir, "/Input/", GDGT.files[i])
  
  # Read data from the specified sheet
  table.temp <- tryCatch({
    read_xlsx(path = file_path, sheet = "GDGTs")
  }, error = function(e) {
    stop("Error reading Excel file: ", file_path, "\n", e)
  })
  
  data.sets[[i]] <- table.temp
}

# Get dataset names by removing the file extension
data.sets.names <- str_remove(string = GDGT.files, pattern = ".xlsx")

###----------------------------------------------------------------------------------------------------------------------###
###------------------------------------- MAIN PROCESSING LOOP -----------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

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

if (!all(c("IIIa.5Me", "Ia", "cum.depth") %in% colnames(GDGT.temp))) {
  stop("The input file does not contain the required columns. Please check the input format.")
}

###----------------------------------------------------------------------------------------------------------------------###
###------------------------------------------- SEPARATION OF COMPOUNDS --------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

# === Select Relevant Data Columns ===

# Define the sets of compounds to extract
brGDGTs_cols <- c("IIIa.5Me", "IIIa.6Me", "IIIb.5Me", "IIIb.6Me", "IIIc.5Me", "IIIc.6Me",
                  "IIa.5Me", "IIa.6Me","IIb.5Me", "IIb.6Me", "IIc.5Me",
                  "IIc.6Me", "Ia", "Ib", "Ic")

GDGTs_cols   <- c("GDGT.0", "GDGT.1", "OH-GDGT.0", "GDGT.2", "OH-GDGT.1", "2OH-GDGT.0", 
                "GDGT.3", "OH-GDGT.2", "GDGT.4","GDGT.4.2")

GMGTs_cols   <- c("H1048", "H1034a", "H1034b","H1034c", "H1020a", "H1020b", "H1020c")

GDDs_cols    <- c("isoGDD0", "isoGDD1","isoGDD2","isoGDD3", "isoGDDCren")

IS_cols      <- c("Label", "cum.depth", "Age", "SEDIEXTR", "IS_AREA","IS_AMOUNT")


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

# calculate the FA following 11-16. fGDGTs0-4.2
fGDGTs.FA           <- fGDGTs(isoGDGTs = GDGTs)


#------ OHDGTs ----------

# calculate the FA following 17-19. fOHGDGTs0-2
fOHGDGTs.FA         <- fOHGDGTs(OHGDGTs = GDGTs)


#------ GMGTs ----------

# calculate the FA following 20-26. fGMGTs1-3
fGMGTs.FA         <- fGMGTs(GMGTs = GMGTs)


###-------------------------------------------------------------------------------------------------------------------------###
###----------------------------------------- PRINT FA OUTPUT FILES ---------------------------------------------------------###
###-------------------------------------------------------------------------------------------------------------------------###


# Define a list of datasets and corresponding filenames
csv_exports <- list(
  list(data = brGDGT.FA, 
       file = paste(DirFA.br, "/", data.sets.name, "_FA-FULL_", Sys.Date(), ".csv", sep = "")),
  
  list(data = brGDGT.MI.FA[, c(1,2,5,6,11,12,3,7,8,13,14,4,9,10,15,16)], 
       file = paste(DirFA.br, "/", data.sets.name, "_FA-MI_", Sys.Date(), ".csv", sep = "")),
  
  list(data = brGDGT.METH.5Mep.FA[, c(1,2,5,6,3,7,8,4,9,10)], 
       file = paste(DirFA.br, "/", data.sets.name, "_FA-METH-5Mep_", Sys.Date(), ".csv", sep = "")),
  
  list(data = brGDGT.METH.6Mep.FA[, c(1,2,5,6,3,7,8,4,9,10)], 
       file = paste(DirFA.br, "/", data.sets.name, "_FA-METH-6Mep_", Sys.Date(), ".csv", sep = "")),
  
  list(data = brGDGT.METH.5Me.FA, 
       file = paste(DirFA.br, "/", data.sets.name, "_FA-METH-5Me_", Sys.Date(), ".csv", sep = "")),
  
  list(data = brGDGT.METH.6Me.FA, 
       file = paste(DirFA.br, "/", data.sets.name, "_FA-METH-6Me_", Sys.Date(), ".csv", sep = "")),
  
  list(data = brGDGT.METH.FA, 
       file = paste(DirFA.br, "/", data.sets.name, "_FA-METH_", Sys.Date(), ".csv", sep = "")),
  
  list(data = brGDGT.CYCL.FA, 
       file = paste(DirFA.br, "/", data.sets.name, "_FA-CI_", Sys.Date(), ".csv", sep = "")),
  
  list(data = brGDGT.CYCL.5Me.FA, 
       file = paste(DirFA.br, "/", data.sets.name, "_FA-CYC-5Me_", Sys.Date(), ".csv", sep = "")),
  
  list(data = brGDGT.CYCL.6Me.FA, 
       file = paste(DirFA.br, "/", data.sets.name, "_FA-CYC-6Me_", Sys.Date(), ".csv", sep = "")),
  
  list(data = brGDGT.FA, 
       file = paste(DirFA, "/", data.sets.name, "_FAs_brGDGTs_", Sys.Date(), ".csv", sep = "")),
  
  list(data = fGDGTs.FA, 
       file = paste(DirFA, "/", data.sets.name, "_FAs_isoGDGTs_", Sys.Date(), ".csv", sep = "")),
  
  list(data = fOHGDGTs.FA, 
       file = paste(DirFA, "/", data.sets.name, "_FAs_OHGDGTs_", Sys.Date(), ".csv", sep = "")),
  
  list(data = fGMGTs.FA, 
       file = paste(DirFA, "/", data.sets.name, "_FAs_GMGTs_", Sys.Date(), ".csv", sep = ""))
)

# Use lapply to iterate over the list and write the CSV files
lapply(csv_exports, function(x) {
  write.csv(x$data, row.names = FALSE, file = x$file)
})


######################################################################################################################################################################
############################################### III. INDEX-CALCULATIONS ################################################################################################
######################################################################################################################################################################

###----------------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------- PREPARE DATASET ---------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

#cut out and shape the GDGTs ad the brGDGT Fractional Abundances

GDGTs                  <-   cbind(IS, GDGTs, GDDs, GMGTs, apply(brGDGT.FA[,-1],2,as.double))
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
  list(data = brGDGT.IND, suffix = "BR_INDICES"),
  list(data = isoGDGT.IND, suffix = "ISO_INDICES"),
  list(data = OHGDGT.IND, suffix = "OH_INDICES"),
  list(data = GMGT.IND, suffix = "GMGT_INDICES"),
  list(data = GDD.IND, suffix = "GDD_INDICES")
)

# Iterate over the list to prepare and write CSV files
lapply(indices.print, function(ind) {
  # Prepare the print file by combining the relevant columns
  ind_print <- cbind(
    Label = rownames(ind$data),
    mid.depth = GDGTs$cum.depth,
    Age = GDGTs$Age,
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
GDGTs.conc <- GDGTs.amount/IS[,"SEDIEXTR"]


# Prepare and save amounts and concentrations
GDGTs.amount.IS <- cbind(rownames(IS), IS[, 2:4], IS.factor, GDGTs.amount)
colnames(GDGTs.amount.IS)[1] <- "Label"
write.csv(GDGTs.amount.IS, file = paste0(DirCONC, data.sets.name, "_AMOUNT_", Sys.Date(), ".csv"), row.names = FALSE)

GDGTs.conc.IS <- cbind(rownames(IS), IS[, 2:4], IS.factor, GDGTs.conc)
colnames(GDGTs.conc.IS)[1] <- "Label"
write.csv(GDGTs.conc.IS, file = paste0(DirCONC, data.sets.name, "_CONC_", Sys.Date(), ".csv"), row.names = FALSE)



###----------------------------------------------------------------------------------------------------------------------###
###--------------------------------------- SAVE SESSION INFO FOR REPRODUCIBILITY ----------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###
# Save session info for reproducibility, no need to change anything

#software version, don't change this
software_version <-"GaDGeT v1.0"

# Prepare a file to store session information including software version
session_info_file <- paste0("Output/SESSION_INFO_",data.sets.name,"_",Sys.Date(),".txt")

# Open the file in write mode
session_info_con <- file(session_info_file, open = "wt")

# Write the software version at the top of the session info file
writeLines(paste("Software Version:", software_version), session_info_con)

# Write the current date and time
writeLines(paste("Date:", Sys.time()), session_info_con)

# Append the session information
writeLines(capture.output(sessionInfo()), session_info_con)

# Close the connection
close(session_info_con)


}


#**************************************************************************************************************************#
#******************************************************* SCRIPT ENDS ******************************************************#
#**************************************************************************************************************************#