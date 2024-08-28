#*************************************************      GaDGeT      **************************************************************
#*************************************************   CALCULATIONS   **************************************************************
#*************************************************        TS        **************************************************************


#-------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------- AUTHOR ------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------

# Author: Tobias Schneider
# Date: 05.12.2020
# Last modification: 27. August 2024
# Email: tobiaschnei@gmail.com, www.drtobiasschneider.com

# DISCLAIMER

# The author does not guarantee for the functionality of this script. Please do not hesitate to get back to the author
# to report problems, or for troubleshooting, or for modifications.


#-------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------- SCRIPT DESCRIPTION --------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------

# This script contains the main script to calculate different kinds of fractional abundances of brGDGTs.
# Furthermore, the script calculates different GDGT-indices as taken from published and peer-reviewed studies.
# Finally, this script will calculate compound amounts and concentrations for the different GDGT fractions.
# Details can be found below


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


######################################################################################################################################################################
############################################# GADGET SOFTWARE STARTS HERE ############################################################################################
######################################################################################################################################################################


######################################################################################################################################################################
############################################# A. PREPARE WORKSPACE ###################################################################################################
######################################################################################################################################################################

# Clean the stored variables.
rm(list=ls(all=TRUE))

# This line deletes the console entries.
cat("\014")

# Clear the plot windows.
graphics.off()

# Get the working directory, or define it by yourself, to the right directory/folder with our data files: 

workingdir<-getwd()#'C://Users//name/Desktop/GaDGeT/'
workingdir<-"C:/Users/tobia/Dropbox/UMASS/Papers/ongoing/GaDGeT/GaDGeT/GaDGeT/Software/"

setwd(workingdir)

# Get and install the right packages

packs<-c("stringr", "RColorBrewer", "readxl")

#install.packages(packs)# only install if you need to install these packages, else comment out this line
lapply(packs, require, character.only = TRUE)


# Load Functions from function files
source("Functions/GDGT_FA-calculation_Functions.R")
source("Functions/brGDGT_INDEX-calculation_Functions.R")
source("Functions/isoGDGT_INDEX-calculation_Functions.R")
source("Functions/OHGDGT_INDEX-calculation_Functions.R")
source("Functions/GMGT_INDEX-calculation_Functions.R")
source("Functions/GDD_INDEX-calculation_Functions.R")

#********************************************************************************************************************************************************************* 
#******************************************************************* SCRIPT STARTS ***********************************************************************************
#*********************************************************************************************************************************************************************

######################################################################################################################################################################
########################################## I. DATA PREPARATION #######################################################################################################
######################################################################################################################################################################

# Check Directory for available data files in "Input"
GDGT.files<-list.files(path = paste(workingdir,"/Input/",sep=""))



###----------------------------------------------------------------------------------------------------------------------###
###------------------------------------------- READ DATA ----------------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

# Initialize list for data-compilation
data.sets<-list()

for (i in 1:length(GDGT.files)){
  
  # Open file and read out information
  table.temp<-read_xlsx(path = paste(workingdir,"/Input/",GDGT.files[i],sep=""),
                        sheet = "GDGTs")
  
  # Append info to a list.
  data.sets[[i]]<-table.temp
}

# Create a list containing the datasets names to be printed (without the ".csv")
data.sets.names<-str_remove(string = GDGT.files,
                            pattern = ".xlsx")



###----------------------------------------------------------------------------------------------------------------------###
###------------------------------------------- NEST LOOP ----------------------------------------------------------------###
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
###------------------------------------ SEPERATE brGDGTs + GDGTs + std --------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

# select only brGDGTs, don't move the order, as it will affect the print later on 

brGDGTs <- c("IIIa.5Me", "IIIa.6Me", "IIIb.5Me", "IIIb.6Me", "IIIc.5Me", "IIIc.6Me",
             "IIa.5Me", "IIa.6Me","IIb.5Me", "IIb.6Me", "IIc.5Me",
             "IIc.6Me", "Ia", "Ib", "Ic")

# select only GDGTs, don't move the order, as it will affect the print later on

GDGTs <- c("GDGT.0", "GDGT.1", "OH-GDGT.0", "GDGT.2", "OH-GDGT.1", "2OH-GDGT.0", 
           "GDGT.3", "OH-GDGT.2", "GDGT.4","GDGT.4.2")


GMGTs <- c("H1048", "H1034a","H1034b","H1034c", "H1020a", "H1020b", "H1020c")


GDDs <- c("isoGDD0", "isoGDD1","isoGDD2","isoGDD3", "isoGDDCren")


# select only std, don't move the order, as it will affect the print later on

IS <- c("Label", "cum.depth", "Age", "SEDIEXTR", "IS_AREA","IS_AMOUNT")



###----------------------------------------------------------------------------------------------------------------------###
###------------------------------------------- GET THE DATA -------------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

# select only brGDGTs, don't move the order, as it will affect the print later on 
brGDGTs <- GDGT.temp[,brGDGTs] # comprises now the 15 columns containing the 15 brGDGTs

# fill NAs with 0
brGDGTs[is.na(brGDGTs)] <-0

# select only brGDGTs, don't move the order, as it will affect the print later on 
GDGTs <- GDGT.temp[,GDGTs] # comprises now the 15 columns containing the 15 brGDGTs

# fill NAs with 0
GDGTs[is.na(GDGTs)] <-0

# select only brGDGTs, don't move the order, as it will affect the print later on 
GMGTs <- GDGT.temp[,GMGTs] # comprises now the 15 columns containing the 15 brGDGTs

# fill NAs with 0
GMGTs[is.na(GMGTs)] <-0

# select only brGDGTs, don't move the order, as it will affect the print later on 
GDDs <- GDGT.temp[,GDDs] # comprises now the 15 columns containing the 15 brGDGTs

# fill NAs with 0
GDDs[is.na(GDDs)] <-0


# select only brGDGTs, don't move the order, as it will affect the print later on 
IS <- GDGT.temp[,IS] # comprises now the 15 columns containing the 15 brGDGTs

# fill NAs with 0
IS[is.na(IS)] <-0

#compile the compounds for concentration calculation later on
GDGTs.conc <- cbind(GDGTs,brGDGTs)



###----------------------------------------------------------------------------------------------------------------------###
###------------------------------------ CREATE STORAGE FOLDERS ----------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

# for Fractional Abundance files according to Raberg et al. 2021
dir.create(path = paste(workingdir,"/Output/",data.sets.name,sep =""))

DirFA.br <- paste(workingdir,"/Output/",data.sets.name,"/FAs/brGDGTs/",sep ="")
DirFA    <- paste(workingdir,"/Output/",data.sets.name,"/FAs/",sep ="")

dir.create(path = DirFA)
dir.create(path = DirFA.br)

# for GDGT INDICES according to the above literature review
DirIND   <- paste(workingdir,"/Output/",data.sets.name,"/","GDGT-INDICES",sep ="")
dir.create(path = DirIND)

# for GDGT concentration files
DirCONC  <- paste(workingdir,"/Output/",data.sets.name,"/","GDGT-CONCENTRATIONS",sep ="")
dir.create(path = DirCONC)








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


#------ brDGTs ----------


# write a csv file into the Output directory containing 1. brGDGT_FA
write.csv(x    = brGDGT.FA,
          row.names = F, 
          file = paste(DirFA.br,"/",data.sets.name,"_FA-FULL_",Sys.Date(),".csv",sep=""))


# write a csv file into the Output directory containing 2. brGDGT_MI_FA, sort them acc. the subsets
write.csv(x    = brGDGT.MI.FA[,c(1,2,5,6,11,12,3,7,8,13,14,4,9,10,15,16)],
          row.names = F, 
          file = paste(DirFA.br,"/",data.sets.name,"_FA-MI_",Sys.Date(),".csv",sep=""))


# write a csv file into the Output directory containing 3. brGDGT_METH_5Mep_FA, sort them acc.the subsets
write.csv(x    = brGDGT.METH.5Mep.FA[,c(1,2,5,6,3,7,8,4,9,10)],
          row.names = F, 
          file = paste(DirFA.br,"/",data.sets.name,"_FA-METH-5Mep_",Sys.Date(),".csv",sep=""))


# write a csv file into the Output directory containing 4. brGDGT_METH_6Mep_FA, sort them acc. the subsets
write.csv(x    = brGDGT.METH.6Mep.FA[,c(1,2,5,6,3,7,8,4,9,10)],
          row.names = F, 
          file = paste(DirFA.br,"/",data.sets.name,"_FA-METH-6Mep_",Sys.Date(),".csv",sep=""))


# write a csv file into the Output directory containing 5. brGDGT_METH_5Me_FA, sort them acc.the subsets
write.csv(x    = brGDGT.METH.5Me.FA,
          row.names = F, 
          file = paste(DirFA.br,"/",data.sets.name,"_FA-METH-5Me_",Sys.Date(),".csv",sep=""))


# write a csv file into the Output directory containing 6. brGDGT_METH_6Me_FA, sort them acc. the subsets
write.csv(x    = brGDGT.METH.6Me.FA,
          row.names = F, 
          file = paste(DirFA.br,"/",data.sets.name,"_FA-METH-6Me_",Sys.Date(),".csv",sep=""))


# write a csv file into the Output directory containing 7. brGDGT_METH_FA, sort them acc. the subsets
write.csv(x    = brGDGT.METH.FA,
          row.names = F, 
          file = paste(DirFA.br,"/",data.sets.name,"_FA-METH_",Sys.Date(),".csv",sep=""))


# write a csv file into the Output directory containing 5. brGDGT_CYCL_FA
write.csv(x    = brGDGT.CYCL.FA,
          row.names = F, 
          file = paste(DirFA.br,"/",data.sets.name,"_FA-CI_",Sys.Date(),".csv",sep=""))


# write a csv file into the Output directory containing 6. brGDGT_CYCL_5Me_FA
write.csv(x    = brGDGT.CYCL.5Me.FA,
          row.names = F, 
          file = paste(DirFA.br,"/",data.sets.name,"_FA-CYC-5Me_",Sys.Date(),".csv",sep=""))


# write a csv file into the Output directory containing 7. brGDGT_CYCL_6Me_FA
write.csv(x    = brGDGT.CYCL.6Me.FA,
          row.names = F, 
          file = paste(DirFA.br,"/",data.sets.name,"_FA-CYC-6Me_",Sys.Date(),".csv",sep=""))


# write a csv file into the Output directory containing 1. brGDGT_FA
write.csv(x    = brGDGT.FA,
          row.names = F, 
          file = paste(DirFA,"/",data.sets.name,"_FAs_brGDGTs_",Sys.Date(),".csv",sep=""))

#------ isoGDGTs ---------

# write a csv file into the Output directory containing 8-13. fGDGT0-4.2
write.csv(x    = fGDGTs.FA,
          row.names = F, 
          file = paste(DirFA,"/",data.sets.name,"_FAs_isoGDGTs_",Sys.Date(),".csv",sep=""))


#------ OHGDGTs ---------

# write a csv file into the Output directory containing 14-16. fOHGDGT0-2
write.csv(x    = fOHGDGTs.FA,
          row.names = F, 
          file = paste(DirFA,"/",data.sets.name,"_FAs_OHGDGTs_",Sys.Date(),".csv",sep=""))


#------ GMGTs ---------

# write a csv file into the Output directory containing 14-16. fOHGDGT0-2
write.csv(x    = fGMGTs.FA,
          row.names = F, 
          file = paste(DirFA,"/",data.sets.name,"_FAs_GMGTs_",Sys.Date(),".csv",sep=""))






######################################################################################################################################################################
############################################### III. INDEX-CALCULATIONS ################################################################################################
######################################################################################################################################################################

###----------------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------- PREPARE DATASET ---------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

#cut out and shape the GDGTs ad the brGDGT Fractional Abundances

GDGTs                  <-   cbind(IS,GDGTs,GMGTs, apply(brGDGT.FA[,-1],2,as.double),GDDs)
GDGTs[is.na(GDGTs)]    <-   0
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


#------ brGDGTs ----------

# prepare the print file

brGDGT.IND.print              <- cbind(rownames(brGDGT.IND),
                                       GDGTs$cum.depth,
                                       GDGTs$Age,
                                       brGDGT.IND)

colnames(brGDGT.IND.print)[1:3] <- c("Label", 
                                   "mid.depth",
                                   "Age")

# print the csv file into the Output directory
write.csv(brGDGT.IND.print,
          row.names = F, 
          file = paste(DirIND,"/",data.sets.name,"_BR_INDICES_",Sys.Date(),".csv",sep=""))


#------ isoGDGTs ----------

# prepare the print file

isoGDGT.IND.print              <- cbind(rownames(isoGDGT.IND),
                                       GDGTs$cum.depth,
                                       GDGTs$Age,
                                       isoGDGT.IND)

colnames(isoGDGT.IND.print)[1:3] <- c("Label", 
                                     "mid.depth",
                                     "Age")

# print the csv file into the Output directory
write.csv(isoGDGT.IND.print,
          row.names = F, 
          file = paste(DirIND,"/",data.sets.name,"_ISO_INDICES_",Sys.Date(),".csv",sep=""))


#------ OHGDGTs ----------

# prepare the print file

OHGDGT.IND.print              <- cbind(rownames(OHGDGT.IND),
                                        GDGTs$cum.depth,
                                        GDGTs$Age,
                                        OHGDGT.IND)

colnames(OHGDGT.IND.print)[1:3] <- c("Label", 
                                      "mid.depth",
                                      "Age")

# print the csv file into the Output directory
write.csv(OHGDGT.IND.print,
          row.names = F, 
          file = paste(DirIND,"/",data.sets.name,"_OH_INDICES_",Sys.Date(),".csv",sep=""))



#------ GMGTs ----------

# prepare the print file

GMGT.IND.print              <- cbind(rownames(GMGT.IND),
                                       GDGTs$cum.depth,
                                       GDGTs$Age,
                                       GMGT.IND)

colnames(GMGT.IND.print)[1:3] <- c("Label", 
                                     "mid.depth",
                                     "Age")

# print the csv file into the Output directory
write.csv(GMGT.IND.print,
          row.names = F, 
          file = paste(DirIND,"/",data.sets.name,"_GMGT_INDICES_",Sys.Date(),".csv",sep=""))



#------ GDDs ----------

# prepare the print file

GDD.IND.print              <- cbind(rownames(GDD.IND),
                                     GDGTs$cum.depth,
                                     GDGTs$Age,
                                     GDD.IND)

colnames(GDD.IND.print)[1:3] <- c("Label", 
                                   "mid.depth",
                                   "Age")

# print the csv file into the Output directory
write.csv(GDD.IND.print,
          row.names = F, 
          file = paste(DirIND,"/",data.sets.name,"_GDD_INDICES_",Sys.Date(),".csv",sep=""))




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


#Prepare the data for printing into file, first amount, then concentration
GDGTs.amount.IS <- cbind(rownames(IS),IS[,2:4],IS.factor,GDGTs.amount)
colnames(GDGTs.amount.IS)[1] <- "Label"

GDGTs.conc.IS <- cbind(rownames(IS),IS[,2:4],IS.factor,GDGTs.conc)
colnames(GDGTs.conc.IS)[1] <- "Label"


# print the csv file containing the amounts into the Output directory
write.csv(GDGTs.amount.IS,
          row.names = F, 
          file = paste(DirCONC,"/",data.sets.name,"_AMOUNT_",Sys.Date(),".csv",sep=""))

# print the csv file containing the concentrations into the Output directory
write.csv(GDGTs.conc.IS,
          row.names = F, 
          file = paste(DirCONC,"/",data.sets.name,"_CONC_",Sys.Date(),".csv",sep=""))

}



#**************************************************************************************************************************#
#******************************************************* SCRIPT ENDS ******************************************************#
#**************************************************************************************************************************#