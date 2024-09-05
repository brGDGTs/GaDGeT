#****************************************************  brGDGTS INDICES  ********************************************************
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

# This script contains the functions used for the main script to calculate different brGDGT indices.
#   For more information, please read the corresponding article in 
#   Organic Geochemistry "GaDGeT – GDGT calculations simplified: an 
#   adaptable R-toolbox for rapid GDGT index calculations" and the 
#   manual by Schneider and Castaneda (2024). And the corresponding 
#   software manual.

# The script contains the following function:

#--------- FUNCTION CALL:

#          brGDGT_INDICES(GDGTs)

#--------- INDEX-CALCULATION-DESCRIPTIONS:

#           1.  CBT:            de Jonge et al. (2014)
#           2.  CBT':           de Jonge et al. (2014)
#           3.  CBT'.5Me:       Russell et al. (2018)
#           4.  MBT:            Weijers et al. (2007)
#           5.  MBT':           de Jonge et al. (2014)
#           6.  MBT.5Me:        de Jonge et al. (2014)
#           7.  MBT.6Me:        Dang et al. (2018)
#           8.  IR:             de Jonge et al. (2014)
#           9.  IR6Me:          de Jonge et al. (2015)
#          10.  IR7Me:          Martin et al. (2019)
#          11.  INDEX1:         de Jonge et al. (2014)
#          12.  pH.DJ:          de Jonge et al. (2014)
#          13.  pH.DJ2:         de Jonge et al. (2014)
#          14.  pH.RB:          Raberg et al. (2021)
#          15.  pH.RB2:         Raberg et al. (2024)
#          16.  pH.RU:          Russell et al.(2018)
#          17.  ln(Cond):       Raberg et al. (2021)
#          18.  Conduct:        Raberg et al. (2021)
#          19.  MAT.DJ1:        de Jonge et al. (2014)
#          20.  MAT.DJ2:        de Jonge et al. (2014)
#          21.  MAT.mrs:        de Jonge et al. (2014)
#          22.  UKT.HA:         Harning et al. (2020)
#          23.  MAAT1.RU:       Russell et al (2018)
#          24.  MAAT2.RU:       Russell et al (2018)
#          25.  SFS.RU:         Russell et al (2018)
#          26.  MWT.ZH:         Zhao et al (2020)
#          27.  MAF.METH:       Raberg et al (2021)
#          28.  MAF.FULL:       Raberg et al (2021)
#          29.  MSST:           Raberg et al (2024)
#          30.  WMST:           Raberg et al (2024)
#          31.  MAST:           Raberg et al (2024)
#          32.  MAAT.trop:      Zhao et al. (2023)
#          33.  MLR.trop:       Zhao et al. (2023)
#          34.  MAF.highlat:    Zhao et al. (2023)
#          35.  MLR.highlat:    Zhao et al. (2023)
#          36.  MAAT.BA1        Bauersachs et al. (2023)
#          37.  MAAT.BA2        Bauersachs et al. (2023)
#          38.  MAF.BA1         Bauersachs et al. (2023)
#          39.  MAF.BA2         Bauersachs et al. (2023)

#          40.  IIIa.IIIaIIIa:  Raberg et al (2021)
#          41.  DO:             Raberg et al (2021)

#          42.  HP5:            Yao et al (2020)
#          43.  RINGtetra:      Raberg et al (2021) 
#          44.  RINGpenta 5Me:  Raberg et al (2021)
#          45.  RINGpenta 6Me:  Raberg et al (2021)
#          46.  DC:             Raberg et al (2021)
#          47.  DC':            de Jonge et al (2024)
#          48.  IBT:            Ding et al (2015)
#          49.  CI:             Raberg et al (2021)
#          50.  BIT:            Hopmans et al (2004), Dang et al (2016)
#          51.  PI.bones:       Zhao et al (2020)
#          52.  MAP.bones:      Zhao et al (2020)

############################################################################################################################
############################################# INDEX CALCULATIONS ###########################################################
############################################################################################################################

###----------------------------------------------------------------------------------------------------------------------###
###-------------------------------------------- INDEX CALC --------------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

brGDGT_INDICES <- function(GDGTs){

  # Initialize dataframe with nrows from input file and 20 Index-columns
  
  #enter the amount of Indices here as "n"
  n= 52
  
  GDGT.IND <- data.frame(matrix(nrow = nrow(GDGTs),ncol = n))
  
  # Set rownames, take those from the input file
  row.names(GDGT.IND) <- rownames(GDGTs)
  
  # Set column names
  colnames(GDGT.IND)  <- c("CBT",
                           "CBT.",
                           "CBT5Me",
                           "MBT",
                           "MBT.", 
                           "MBT.5Me",
                           "MBT.6Me",
                           "IR",
                           "IR6Me",
                           "IR7Me",
                           "INDEX1",
                           "pH.DJ",
                           "pH.DJ2",
                           "pH.RB",
                           "pH.RB2",
                           "pH.RU",
                           "ln.Cond.",
                           "Conduct",
                           "MAT.DJ1",
                           "MAT.DJ2",
                           "MAT.mrs",
                           "UKT.HA",
                           "MAAT1.RU",
                           "MAAT2.RU",
                           "SFS.RU",
                           "MWT.ZH",
                           "MAF.METH",
                           "MAF.FULL",
                           "MSST",
                           "WMST",
                           "MAST",
                           "MAAT.trop",
                           "MLR.trop",
                           "MAF.highlat",
                           "MLR.highlat",
                           "MAAT.BA1",
                           "MAAT.BA2",
                           "MAF.BA1",
                           "MAF.BA2",
                           "IIIa.IIIaIIIa",
                           "DO",
                           "HP5",
                           "RINGtetra",
                           "RINGpenta5",
                           "RINGpenta6",
                           "DC",
                           "DC.",
                           "IBT",
                           "CI",
                           "BIT",
                           "PI.bones",
                           "MAP.bones")
  
  
  ###-------------------------------------------- PREP CALC --------------------------------------------------------------###
  
  #FA calculations for indices for Zhao et al (2023) tropical 9 compounds
  fIa.Zh        <- GDGTs[,c("Ia")] / rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me","IIIa.5Me","IIIa.6Me")])
  fIb.Zh        <- GDGTs[,c("Ib")] / rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me","IIIa.5Me","IIIa.6Me")])
  fIIa.Zh       <- GDGTs[,c("IIa.6Me")] / rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me","IIIa.5Me","IIIa.6Me")])
  fIIIa.Zh      <- GDGTs[,c("IIIa.5Me")] / rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me","IIIa.5Me","IIIa.6Me")])
  
  
  #FA calculations for indices for Loomis et al (2012) 9 compounds
  fI.LO        <- GDGTs[,c("Ia")] / rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIb.5Me","IIc.5Me","IIIa.5Me","IIIb.5Me","IIIc.5Me")])
  fIb.LO       <- GDGTs[,c("Ib")] / rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIb.5Me","IIc.5Me","IIIa.5Me","IIIb.5Me","IIIc.5Me")])
  fII.LO       <- GDGTs[,c("IIa.5Me")] / rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIb.5Me","IIc.5Me","IIIa.5Me","IIIb.5Me","IIIc.5Me")])
  fIIc.LO      <- GDGTs[,c("IIc.5Me")] / rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIb.5Me","IIc.5Me","IIIa.5Me","IIIb.5Me","IIIc.5Me")])
  fIII.LO      <- GDGTs[,c("IIIa.5Me")] / rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIb.5Me","IIc.5Me","IIIa.5Me","IIIb.5Me","IIIc.5Me")])
  
  
  ###-------------------------------------------- INDEX CALC --------------------------------------------------------------###
  
  GDGT.IND <- data.frame(GDGT.IND)

  ### 1
  #calculate CBT; de Jonge et al (2014)
  GDGT.IND$CBT           <-   (-log(rowSums(GDGTs[,c("Ib","IIb.5Me","IIb.6Me")])/
                                    rowSums(GDGTs[,c("Ia","IIa.5Me","IIa.6Me")]),
                                    base = 10))
  
  ### 2
  #calculate CBT'; de Jonge et al (2014)
  GDGT.IND$CBT.          <-   log(rowSums(GDGTs[,c("Ic","IIa.6Me","IIb.6Me","IIc.6Me","IIIa.6Me","IIIb.6Me","IIIc.6Me")])/
                                  rowSums(GDGTs[,c("Ia","IIa.5Me","IIIa.5Me")]),
                                  base = 10)
  
  ### 3
  #calculate CBT'. 5Me; Russell et al (2018)
  GDGT.IND$CBT5Me       <-   (-log(rowSums(GDGTs[,c("Ib","IIb.5Me")])/
                                      rowSums(GDGTs[,c("Ia","IIa.5Me")]),
                                    base = 10))
  
  ### 4
  #calculate MBT; Weijers et al. (2007)
  GDGT.IND$MBT           <-   (rowSums(GDGTs[,c("Ia","Ib","Ic")])/
                                 rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me",
                                                  "IIc.5Me","IIc.6Me","IIIa.5Me","IIIa.6Me","IIIb.5Me","IIIb.6Me",
                                                  "IIIc.5Me","IIIc.6Me")]))
  
  ### 5
  #calculate MBT'; de Jonge et al (2014)
  GDGT.IND$MBT.          <-   (rowSums(GDGTs[,c("Ia","Ib","Ic")])/
                                 rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me","IIc.5Me",
                                                  "IIc.6Me","IIIa.5Me","IIIa.6Me")]))
  
  ### 6
  #calculate MBT'.5Me; de Jonge et al (2014)
  GDGT.IND$MBT.5Me       <-   (rowSums(GDGTs[,c("Ia","Ib","Ic")])/
                                 rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIb.5Me","IIc.5Me","IIIa.5Me")]))
  
  ### 7
  #calculate MBT'.6Me; Dang et al (2018)
  GDGT.IND$MBT.6Me       <-   (rowSums(GDGTs[,c("Ia","Ib","Ic")])/
                                 rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.6Me","IIb.6Me","IIc.6Me","IIIa.6Me")]))
  
  ### 8
  #calculate IR; de Jonge et al (2014)
  GDGT.IND$IR            <-   (rowSums(GDGTs[,c("IIa.6Me","IIIa.6Me")])/
                                 rowSums(GDGTs[,c("IIa.5Me","IIa.6Me","IIIa.5Me","IIIa.6Me")]))
  
  ### 9
  #calculate IR; de Jonge et al (2015)
  GDGT.IND$IR6Me            <-   (rowSums(GDGTs[,c("IIa.6Me","IIb.6Me","IIc.6Me","IIIa.6Me","IIIb.6Me","IIIc.6Me")])/
                                 rowSums(GDGTs[,c("IIa.5Me","IIb.5Me","IIc.5Me","IIIa.5Me","IIIb.5Me","IIIc.5Me","IIa.6Me",
                                                  "IIb.6Me","IIc.6Me","IIIa.6Me","IIIb.6Me","IIIc.6Me")]))
  
  ### 10
  #calculate IR7Me; Martin et al. (2019)
  GDGT.IND$IR7Me            <-   (rowSums(GDGTs[,c("IIa.7Me","IIIa.7Me","IIIb.7Me")])/
                                    rowSums(GDGTs[,c("IIa.5Me","IIb.5Me","IIc.5Me","IIIa.5Me","IIIb.5Me","IIIc.5Me","IIa.6Me",
                                                     "IIb.6Me","IIc.6Me","IIIa.6Me","IIIb.6Me","IIIc.6Me")]))
  
  ### 11
  #calculate INDEX1; deJonge et al (2014)
  GDGT.IND$INDEX1        <-   (log(rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.6Me","IIIa.6Me")])/
                                     rowSums(GDGTs[,c("Ic","IIa.5Me","IIc.5Me","IIIa.5Me","IIIa.6Me")]),
                                   base = 10))
  
  ### 12
  #calculate pH; deJonge et al (2014)
  GDGT.IND$pH.DJ   <-   7.84 - 1.73*GDGT.IND$CBT5Me
 
  ### 13
  #calculate pH; deJonge et al (2014)
  GDGT.IND$pH.DJ2   <-   7.15 + 1.59*GDGT.IND$CBT.
  
  ### 14
  #calculate pH; Raberg et al (2021)
  GDGT.IND$pH.RB    <-   8.93 - (3.84*(as.numeric(brGDGT.CYCL.FA[,"Ia"])^2)) + (2.63*(as.numeric(brGDGT.CYCL.FA[,"IIa.6Me"])))
  
  ### 15
  #calculate pH; Raberg et al (2021)
  GDGT.IND$pH.RB2    <-   5.51 + 0.55*GDGT.IND$CBT. + 2.41*GDGT.IND$IR6Me
  
  ### 16
  #calculate pH; Russell et al. (2018)
  GDGT.IND$pH.RU    <-   8.95 + (2.65*GDGT.IND$CBT.)
  
  ### 17
  #calculate ln(Cond); Raberg et al (2021)
  GDGT.IND$ln.Cond.     <-   6.62 + 8.87*as.numeric(brGDGT.CYCL.FA[,"Ib"]) + 5.12*(as.numeric(brGDGT.CYCL.FA[,"IIa.6Me"])^2) +
                             10.64*(as.numeric(brGDGT.CYCL.FA[,"IIa.5Me"])^2) - 8.59*as.numeric(brGDGT.CYCL.FA[,"IIa.5Me"]) -
                             4.32*(as.numeric(brGDGT.CYCL.FA[,"IIIa.6Me"])^2) - 5.31*(as.numeric(brGDGT.CYCL.FA[,"IIIa.5Me"])^2) -
                             142.67*(as.numeric(brGDGT.CYCL.FA[,"IIIb.5Me"])^2)
  
  ### 18
  #calculate Conduct; Raberg et al. (2021)
  GDGT.IND$Conduct      <-   exp(GDGT.IND$ln.Cond.)
  
  ### 19
  #calculate MAT; deJonge et al (2014)
  GDGT.IND$MAT.DJ1       <-   -8.57 + 31.45*GDGT.IND$MBT.5Me
 
  ### 20
  #calculate MAT; deJonge et al (2014)
  GDGT.IND$MAT.DJ2       <-   5.05+ (14.86*GDGT.IND$INDEX1) 
  
  ### 21
  #calculate MAT.mrs; deJonge et al (2014)
  GDGT.IND$MAT.mrs       <-   5.58 + (17.91*GDGTs$Ia) + (18.77*GDGTs$IIa.5Me)

  ### 22
  #calculate UK37-temp, Harning et al (2020)
  GDGT.IND$UKT.HA       <-   (-0.154*GDGTs$IIIa.5Me)+(0.3538*GDGTs$Ia)+(1.0016*GDGTs$IIIa.6Me)-0.7537
  
  ### 23
  #calculate MAAT; Russell et al (2018)
  GDGT.IND$MAAT1.RU     <-   (-1.21)+(32.42*GDGT.IND$MBT.5Me)
  
  ### 24
  #calculate MAAT; Russell et al (2018)
  GDGT.IND$MAAT2.RU     <-   12.22+(18.79*GDGT.IND$INDEX1)

  ### 25
  #calculate MAAT SFS; Russell et al (2018)
  GDGT.IND$SFS.RU     <-   23.81 -(31.02*GDGTs[,c("IIIa.5Me")])-(41.91*GDGTs[,c("IIb.5Me")])
                            -(51.59*GDGTs[,c("IIb.6Me")])-(24.7*GDGTs[,c("IIa.5Me")])+(68.8*GDGTs[,c("Ib")])
  
  ### 26
  #calculate MWT; Zhao et al. (2020)
  GDGT.IND$MWT.ZH       <-    -1.82 + 56.06*GDGT.IND$MBT.5Me
  
  ### 27
  #calculate MAF.METH; Raberg et al. (2021)
  brGDGT.ME.FA        <- as.matrix(cbind(brGDGT.METH.5Mep.FA,brGDGT.METH.6Me.FA))# for col assignment
  
  
  for(i in 1:nrow(brGDGT.ME.FA)){
  
  GDGT.IND$MAF.METH[i]       <-  (92.9 + (63.84*(as.numeric(brGDGT.ME.FA[i,c("Ib")])^2))-(130.51*(as.numeric(brGDGT.ME.FA[i,c("Ib")])))
                                  -(28.77*(as.numeric(brGDGT.ME.FA[i,c("IIa.5Me")])^2))-(72.28*(as.numeric(brGDGT.ME.FA[i,c("IIb.5Me")])^2))
                                  -(5.88*(as.numeric(brGDGT.ME.FA[i,c("IIc.5Me")])^2))+(20.89*(as.numeric(brGDGT.ME.FA[i,c("IIIa.5Me")])^2))
                                  -(40.54*as.numeric(brGDGT.ME.FA[i,c("IIIa.5Me")]))-(80.47*as.numeric(brGDGT.ME.FA[i,c("IIIb.5Me")])))
  }

  ### 28
  #calculate MAF.FULL; Raberg et al. (2021)
  brGDGT.F  <- as.matrix(brGDGT.FA) #for col assignment
  
  for(r in 1:nrow(brGDGT.F)){
  
  GDGT.IND$MAF.FULL[r]       <-    ((-8.06)+(37.52*as.numeric(brGDGT.F[r,c("Ia")]))-(266.83*(as.numeric(brGDGT.F[r,c("Ib")])^2))
                                    +(133.42*as.numeric(brGDGT.F[r,c("Ib")]))+(100.85*(as.numeric(brGDGT.F[r,c("IIa.6Me")])^2))
                                    +(58.15*(as.numeric(brGDGT.F[r,c("IIIa.6Me")])^2))+(12.79*as.numeric(brGDGT.F[r,c("IIIa.5Me")])))
  }
  
  brGDGT.FA.MI <- as.matrix(brGDGT.FA.MI)
  
  ### 29
  #calculate MSST, Raberg et al. (2024)
  for(i in 1:nrow(brGDGT.FA.MI)){
  GDGT.IND$MSST[i]      <-  as.numeric(brGDGT.FA.MI[i,c("Ic")])
  }
  
  ### 30
  #calculate MSST, Raberg et al. (2024)
  for(i in 1:nrow(brGDGT.FA.MI)){
   GDGT.IND$WMST[i]      <-  as.numeric(brGDGT.FA.MI[i,c("IIIc.5Me")])
  }
  
  ### 31
  #calculate MSST, Raberg et al. (2024)
 
  for(i in 1:nrow(brGDGT.ME.FA)){
   GDGT.IND$MAST[i]     <-  16.3 + 10.6*as.numeric(brGDGT.ME.FA[i,c("Ib")])^2 - 15.1*as.numeric(brGDGT.ME.FA[i,c("Ic")])^2 + 
                        16.3*as.numeric(brGDGT.ME.FA[i,c("Ic")]) - 35.1*as.numeric(brGDGT.ME.FA[i,c("IIa.5Me")]) - 
                        10.3*as.numeric(brGDGT.ME.FA[i,c("IIc.5Me")]) + 23.8*as.numeric(brGDGT.ME.FA[i,c("IIIa.6Me")])^2 - 
                        28.3*as.numeric(brGDGT.ME.FA[i,c("IIIa.6Me")]) - 14.4*as.numeric(brGDGT.ME.FA[i,c("IIIb.6Me")])^2 +
                        12.7*as.numeric(brGDGT.ME.FA[i,c("IIIb.6Me")]) + 12.7*as.numeric(brGDGT.ME.FA[i,c("IIb.6Me")])^2
  }
  
  ### 28
  #calculate MAAT.trop, zhao et al. (2023)
  GDGT.IND$MAAT.trop      <-  (-1.78)+31.01*GDGT.IND$MBT.5Me
  
  ### 29
  #calculate MLR-lowlat, zhao et al. (2023)
  GDGT.IND$MLR.trop       <-  (-4.11)+31.63*fIa.Zh+64.5*fIb.Zh+32.28*fIIa.Zh
  
  ### 30
  #calculate MLR-highlat
  GDGT.IND$MAF.highlat      <-  3.36+17.25*GDGT.IND$MBT.5Me
  
  ### 31
  #calculate MLR-highlat
  GDGT.IND$MLR.highlat      <-  1.44+15.88*fIa.Zh+66.92*fIb.Zh+8.33*fIIa.Zh+7.02*fIIIa.Zh
  
  ### 32
  #calculate MAAT.BA1
  GDGT.IND$MAAT.BA1      <-  (-2.19)+ (31.91*GDGT.IND$MBT.5Me) 
  
  ### 33
  #calculate MAAT.BA2
  GDGT.IND$MAAT.BA2      <-  7.11 + (67.66*GDGTs$Ib) - (13.54*GDGTs$IIIa.5Me)
  
  ### 34
  #calculate MAF.BA1
  GDGT.IND$MAF.BA1      <-  4.81 + (15.64*GDGT.IND$MBT.5Me) 
  
  ### 35
  #calculate MAF.BA2
  GDGT.IND$MAF.BA2      <-  5.91 + (16.22*GDGTs$Ia)
  
  ### 36
  #calculate IIIa/(IIIa+IIIa')
  GDGT.IND$IIIa.IIIaIIIa  <-   GDGTs$IIIa.5Me/ rowSums(GDGTs[,c("IIIa.5Me","IIIa.6Me")])
  
  ### 37
  #calculate DO, Raberg et al. (2021)
  GDGT.IND$DO            <-  (7.6-(12.03*(as.numeric(brGDGT.ME.FA[i,c("Ia")])^2))-(2.1*(as.numeric(brGDGT.ME.FA[i,c("Ic")])^2))
                            -(28.66*(as.numeric(brGDGT.ME.FA[i,c("IIIa.6Me")])^2))+(31.09*(as.numeric(brGDGT.ME.FA[i,c("IIIa.6Me")])))
                            +(36.85*(as.numeric(brGDGT.ME.FA[i,c("IIIa.5Me")])^2))-(35.89*(as.numeric(brGDGT.ME.FA[i,c("IIIa.5Me")])))
                            -(15.29*as.numeric(brGDGT.ME.FA[i,c("IIIb.6Me")])^2)+(15.82*as.numeric(brGDGT.ME.FA[i,c("IIIb.6Me")])))
  
  ### 38
  #calculate HP 5Me
  GDGT.IND$HP5        <-   GDGTs$IIIa.5Me/rowSums(GDGTs[,c("IIIa.5Me","IIa.5Me")])
  
  ### 39
  #calculate RING tetra
  GDGT.IND$RINGtetra    <-   (GDGTs$Ib + 2*GDGTs$Ic)/rowSums(GDGTs[,c("Ia","Ib","Ic")])
  
  ### 40
  #calculate RING penta 5Me
  GDGT.IND$RINGpenta5    <-   (GDGTs$IIb.5Me + 2*GDGTs$IIc.5Me)/rowSums(GDGTs[,c("IIa.5Me","IIb.5Me","IIc.5Me")])
  
  ### 41
  #calculate RING penta 6Me
  GDGT.IND$RINGpenta6    <-   (GDGTs$IIb.6Me + 2*GDGTs$IIc.6Me)/rowSums(GDGTs[,c("IIa.6Me","IIb.6Me","IIc.6Me")])
  
  ### 42
  #calculate DC
  GDGT.IND$DC          <-   (GDGTs$Ib + 2*GDGTs$Ic + GDGTs$IIb.5Me + GDGTs$IIb.6Me)/
                            rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me")])

  ### 43
  #calculate DC'
  GDGT.IND$DC.          <-   (GDGTs$Ib + GDGTs$IIb.5Me + GDGTs$IIb.6Me)/
                              rowSums(GDGTs[,c("Ia","IIa.5Me","IIa.6Me","Ib","IIb.5Me","IIb.6Me")])
  
  ### 44
  #calculate IBT
  GDGT.IND$IBT      <-   (-log(rowSums(GDGTs[,c("IIa.6Me","IIIa.6Me")])/
                                      rowSums(GDGTs[,c("IIa.5Me","IIIa.5Me")]),
                                    base = 10))
  
  ### 45
  #calculate CI
  GDGT.IND$CI      <-   GDGTs$Ia/rowSums(GDGTs[,c("Ia","IIa.5Me","IIIa.5Me")])
 
  ### 46
  #calculate BIT
  GDGT.IND$BIT      <-  rowSums(GDGTs[,c("Ia","IIa.5Me","IIa.6Me","IIIa.5Me","IIIa.6Me")])/rowSums(GDGTs[,c("Ia","IIa.5Me","IIa.6Me","IIIa.5Me","IIIa.6Me","GDGT.4")]) 
  
  ### 47
  #calculate PI.bones
  GDGT.IND$PI.bones      <-  rowSums(GDGTs[,c("Ia","Ib")])/rowSums(GDGTs[,c("Ia","Ib", "IIIa.5Me","IIa.6Me","IIIa.6Me")]) 
  
  
  ### 48
  #calculate MAP
  GDGT.IND$MAP.bones      <-  913.41*GDGT.IND$PI.bones+112 
  
  
  return(GDGT.IND)
}


#**************************************************************************************************************************#
#******************************************************* END **************************************************************#
#**************************************************************************************************************************#