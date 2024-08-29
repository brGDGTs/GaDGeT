#*************************************************      GaDGeT      **************************************************************
#*************************************************   CALCULATIONS   **************************************************************
#*************************************************        TS        **************************************************************


#-------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------- AUTHOR ------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------

# Author: Tobias Schneider
# Date: 05.12.2020
# Last modification: 29. August 2024
# COntact: tobiaschnei@gmail.com, www.drtobiasschneider.com

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

#          contains 26 Functions:

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




#---------------------------------------------------------------
#--------- GDGT INDICES: -------------------------------
#---------------------------------------------------------------

#          contains 5 functions:

#--------- FUNCTION CALL:

#         1. brGDGT_INDICES(brGDGTs)
#         2. isoGDGT_INDICES(isoGDGTs)
#         3. OHGDGT_INDICES (OHGDGTs)
#         4. GMGT_INDICES(GMGTs)
#         5. GDD_INDICES(GDDs)

#--------- INDEX-CALCULATION-DESCRIPTIONS:


#------ brGDGTs ----------

#           1.  CBT:            de Jonge et al. (2014)
#           2.  CBT':           de Jonge et al. (2014)
#           3.  CBT'.5Me:       Russell et al. (2018)
#           4.  MBT:            Weijers et al. (2007)
#           5.  MBT':           de Jonge et al. (2014)
#           6.  MBT.5Me:        de Jonge et al. (2014)
#           7.  MBT.6Me:        Dang et al. (2018)
#           8.  IR:             de Jonge et al. (2014)
#           9.  IR6Me:          de Jonge et al. (2015)
#          10.  INDEX1:         de Jonge et al. (2014)
#          11.  pH.DJ:          de Jonge et al. (2014)
#          12.  pH.DJ2:         de Jonge et al. (2014)
#          13.  pH.RB:          Raberg et al. (2021)
#          14.  pH.RU:          Russell et al.(2018)
#          15.  ln(Cond):       Raberg et al. (2021)
#          16.  Conduct:        Raberg et al. (2021)
#          17.  MAT.DJ1:        de Jonge et al. (2014)
#          18.  MAT.DJ2:        de Jonge et al. (2014)
#          19.  MAT.mrs:        de Jonge et al. (2014)
#          20.  GT.DA:          Dang et al. (2018)
#          21.  UKT.HA:         Harning et al. (2020)
#          22.  MAAT1.RU:       Russell et al (2018)
#          23.  MAAT2.RU:       Russell et al (2018)
#          24.  SFS.RU:         Russell et al (2018)
#          25.  MWT.ZH:         Zhao et al (2020)
#          26.  MAF.METH:       Raberg et al (2021)
#          27.  MAF.FULL:       Raberg et al (2021)
#          28.  MAAT.trop:      Zhao et al. (2023)
#          29.  MLR.trop:       Zhao et al. (2023)
#          30.  MAF.highlat:    Zhao et al. (2023)
#          31.  MLR.highlat:    Zhao et al. (2023)
#          32.  MAAT.BA1        Bauersachs et al. (2023)
#          33.  MAAT.BA2        Bauersachs et al. (2023)
#          34.  MAF.BA1         Bauersachs et al. (2023)
#          35.  MAF.BA2         Bauersachs et al. (2023)
#          36.  IIIa.IIIaIIIa:  Raberg et al (2021)
#          37.  DO:             Raberg et al (2021)
#          38.  HP5:            Yao et al (2020)
#          39.  RINGtetra:      Raberg et al (2021) 
#          40.  RINGpenta 5Me:  Raberg et al (2021)
#          41.  RINGpenta 6Me:  Raberg et al (2021)
#          42.  DC:             Raberg et al (2021)
#          43.  DC':            de Jonge et al (2024)
#          44.  IBT:            Ding et al (2015)
#          45.  CI:             Raberg et al (2021)
#          46.  BIT:            Hopmans et al (2004), Dang et al (2016)
#          47.  PI.bones:       Zhao et al (2020)
#          48.  MAP.bones:      Zhao et al (2020)


#------ isoGDGTs ---------

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



#------ OHDGTs ----------

#--------- FUNCTION CALL:

#          OHGDGT_INDICES(GDGTs)

#--------- INDEX-CALCULATION-DESCRIPTIONS:

#           1.  PERC.OHtot:     Feitz et al. (2013)
#           2.  OH1318.1316:    Feitz et al. (2013)
#           2.  SST.FI:         Feitz et al. (2013)
#           3.  RI.OH:          Lü et al. (2015)
#           4.  RI.OH':         Lü et al. (2015)
#           7.  RI.OH.SST:      Lü et al. (2015)
#           8.  RI.OH'.SST:     Lü et al. (2015)



#------ GMGTs ----------

#--------- FUNCTION CALL:

#          GMGT_INDICES(GDGTs)

#--------- INDEX-CALCULATION-DESCRIPTIONS:

#           1.  brGMGTI:          Baxter et al. (2019)
#           2.  brGMGTI.MAAT:     Baxter et al. (2019)
#           3.  brGMGT.MAAT2:     Baxter et al. (2019)
#           4.  brGMGT%:          Baxter et al. (2021)
#           5.  DM.brGMGT:        Baxter et al. (2024)



#------ GDDs ----------

#--------- FUNCTION CALL:

#          GDD_INDICES(GDGTs)

#--------- INDEX-CALCULATION-DESCRIPTIONS:

#           1.  RI.GDD:           Hingley et al. (2024), eq. 2
#           2.  GDD%:             Hingley et al. (2024), eq. 3
#           3.  GDD%tot:          Hingley et al. (2024), eq. 4



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

# Install missing packages (uncomment if needed)
# install.packages(setdiff(packs, installed.packages()[, "Package"]))

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

}


#**************************************************************************************************************************#
#******************************************************* SCRIPT ENDS ******************************************************#
#**************************************************************************************************************************#