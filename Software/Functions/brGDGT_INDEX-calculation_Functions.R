#****************************************************  brGDGTS INDICES  ********************************************************
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

#          GDGT_INDICES(GDGTs)

#--------- INDEX-CALCULATION-DESCRIPTIONS:

#           1.  CBT:            de Jonge et al. (2014)
#           2.  CBT':           de Jonge et al. (2014)
#           3.  CBT'.5Me:       Russell et al. (2018)
#           4.  MBT:            Weijers et al. (2007)
#           5.  MBT':           Peterse et al. (2012)
#           6.  MBT.5Me:        Dang et al. (2018)
#           7.  MBT.6Me:        Dang et al. (2018)
#           8.  IR:             de Jonge et al. (2014)
#           9.  INDEX1:         de Jonge et al. (2014)
#          10.  pH.DJ:          de Jonge et al. (2014)
#          11.  pH.RB:          Raberg et al. (2021)
#          12.  ln(Cond):       Raberg et al. (2021)
#          13.  Conduct:        Raberg et al. (2021)
#          14.  MAT.DJ:         de Jonge et al. (2014)
#          15.  GT.DA:          Dang et al. (2018)
#          16.  UKT.HA:         Harning et al. (2020)
#          17.  MAAT.SUN:       Sun et al. (2011)
#          18.  MAAT1.RU:       Russell et al (2018)
#          19.  MAAT2.RU:       Russell et al (2018)
#          20.  MAAT1.LO:       Loomis et al. (2012)
#          21.  MAAT2.LO:       Loomis et al. (2012)
#          22.  MAAT3.Lo:       Loomis et al (2012)
#          23.  SFS.RU:         Russell et al (2018)
#          24.  MWT.ZH:         Zhao et al (2020)
#          25.  MAF.METH:       Raberg et al (2021)
#          26.  MAF.FULL:       Raberg et al (2021)
#          27.  MAAT.trop:      Zhao et al. (2023)
#          28.  MLR.trop:       Zhao et al. (2023)
#          29.  MAF.highlat:    Zhao et al. (2023)
#          30.  MLR.highlat:    Zhao et al. (2023)
#          31.  IIIa.IIIaIIIa:  Raberg et al (2021)
#          32.  IIIa.IIa:       Raberg et al (2021)

#          33.  HP5Me:          Yao et al (2020)
#          34.  RINGtetra:      Raberg et al (2021) 
#          35.  RINGpenta 5Me:  Raberg et al (2021)
#          36.  RINGpenta 6Me:  Raberg et al (2021)
#          37.  DC:             Raberg et al (2021)
#          38.  IBT:            Ding et al (2015)
#          39.  CI:             Raberg et al (2021)
#          40.  BIT:            Hopmans et al (2004)


############################################################################################################################
############################################# INDEX CALCULATIONS ###########################################################
############################################################################################################################

###----------------------------------------------------------------------------------------------------------------------###
###-------------------------------------------- INDEX CALC --------------------------------------------------------------###
###----------------------------------------------------------------------------------------------------------------------###

brGDGT_INDICES <- function(GDGTs){

  # Initialize dataframe with nrows from input file and 20 Index-columns
  
  #enter the amount of Indices here as "n"
  n= 37
  
  GDGT.IND <- data.frame(matrix(nrow = nrow(GDGTs),ncol = n))
  
  # Set rownames, take those from the input file
  row.names(GDGT.IND) <- rownames(GDGTs)
  
  # Set column names
  colnames(GDGT.IND)  <- c("CBT",
                           "CBT'",
                           "CBT'.5Me",
                           "MBT",
                           "MBT'",
                           "MBT.5Me",
                           "MBT.6Me",
                           "IR",
                           "INDEX1",
                           "pH.DJ",
                           "pH.RB",
                           "ln(Cond)",
                           "Conduct",
                           "MAT.DJ",
                           "GT.DA",
                           "UKT.HA",
                           "MAAT.SUN",
                           "MAAT1.RU",
                           "MAAT2.RU",
                           "MAAT1.LO",
                           "MAAT2.LO",
                           "MAAT3.LO",
                           "SFS.RU",
                           "MWT.ZH",
                           "MAF.METH",
                           "MAF.FULL",
                           "MAAT.trop",
                           "MLR.trop",
                           "MAF.highlat",
                           "MLR.highlat",
                           "IIIa.IIIaIIIa",
                           "IIIa.IIa",
                           "HP5Me",
                           "RINGtetra",
                           "RINGpenta5",
                           "RINGpenta6",
                           "DC",
                           "IBT",
                           "CI",
                           "BIT")
  
  
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
  GDGT.IND$CBT.          <-   (-log(rowSums(GDGTs[,c("Ic","IIa.6Me","IIb.6Me","IIc.6Me","IIIa.6Me","IIIb.6Me","IIIc.6Me")])/
                                      rowSums(GDGTs[,c("Ia","IIa.5Me","IIIa.5Me")]),
                                    base = 10))
  
  ### 3
  #calculate CBT'. 5Me; Russell et al (2018)
  GDGT.IND$CBT..5Me       <-   (-log(rowSums(GDGTs[,c("Ib","IIb.5Me")])/
                                      rowSums(GDGTs[,c("Ia","IIa.5Me")]),
                                    base = 10))
  
  ### 4
  #calculate MBT; Weijers et al. (2007)
  GDGT.IND$MBT           <-   (rowSums(GDGTs[,c("Ia","Ib","Ic")])/
                                 rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me",
                                                  "IIc.5Me","IIc.6Me","IIIa.5Me","IIIa.6Me","IIIb.5Me","IIIb.6Me",
                                                  "IIIc.5Me","IIIc.6Me")]))
  
  ### 5
  #calculate MBT'; Peterse et al. (2012)
  GDGT.IND$MBT.          <-   (rowSums(GDGTs[,c("Ia","Ib","Ic")])/
                                 rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me","IIc.5Me",
                                                  "IIc.6Me","IIIa.5Me","IIIa.6Me")]))
  
  ### 6
  #calculate MBT'.5Me; Dang et al (2018)
  GDGT.IND$MBT.5Me       <-   (rowSums(GDGTs[,c("Ia","Ib","Ic")])/
                                 rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIb.5Me","IIc.5Me","IIIa.5Me")]))
  
  ### 7
  #calculate MBT'.6Me; Dang et al (2018)
  GDGT.IND$MBT.6Me       <-   (rowSums(GDGTs[,c("Ia","Ib","Ic")])/
                                 rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.6Me","IIb.6Me","IIc.6Me","IIIa.6Me")]))
  
  ### 8
  #calculate IR; de Jonge et al (2014)
  GDGT.IND$IR            <-   (rowSums(GDGTs[,c("IIa.6Me","IIb.6Me","IIc.6Me","IIIa.6Me","IIIb.6Me","IIIc.6Me")])/
                                 rowSums(GDGTs[,c("IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me","IIc.5Me","IIc.6Me",
                                                  "IIIa.5Me","IIIa.6Me","IIIb.5Me","IIIb.6Me","IIIc.5Me","IIIc.6Me")]))
  
  ### 9
  #calculate INDEX1; deJonge et al (2014)
  GDGT.IND$INDEX1        <-   (log(rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.6Me","IIIa.6Me")])/
                                     rowSums(GDGTs[,c("Ic","IIa.5Me","IIc.5Me","IIIa.5Me","IIIa.6Me")]),
                                   base = 10))
  
  ### 10
  #calculate pH; deJonge et al (2014)
  GDGT.IND$pH.DJ   <-   7.84 - 1.73*GDGT.IND$CBT..5Me
 
  ### 11
  #calculate pH; Raberg et al (2021)
  GDGT.IND$pH.RB    <-   8.93 - (3.84*(as.numeric(brGDGT.CYCL.FA[,"Ia"])^2)) + (2.63*(as.numeric(brGDGT.CYCL.FA[,"IIa.6Me"])))
  
  ### 12
  #calculate ln(Cond); Raberg et al (2021)
  GDGT.IND$ln.Cond.     <-   6.62 + 8.87*as.numeric(brGDGT.CYCL.FA[,"Ib"]) + 5.12*(as.numeric(brGDGT.CYCL.FA[,"IIa.6Me"])^2) +
                             10.64*(as.numeric(brGDGT.CYCL.FA[,"IIa.5Me"])^2) - 8.59*as.numeric(brGDGT.CYCL.FA[,"IIa.5Me"]) -
                             4.32*(as.numeric(brGDGT.CYCL.FA[,"IIIa.6Me"])^2) - 5.31*(as.numeric(brGDGT.CYCL.FA[,"IIIa.5Me"])^2) -
                             142.67*(as.numeric(brGDGT.CYCL.FA[,"IIIb.5Me"])^2)
  
  ### 13
  #calculate Conduct; Raberg et al. (2021)
  GDGT.IND$Conduct      <-   exp(GDGT.IND$ln.Cond.)
  
  
  ### 14
  #calculate MAT; deJonge et al (2014)
  GDGT.IND$MAT.DJ       <-   -8.57 + 31.45*GDGT.IND$MBT.5Me
  
  ### 15
  #calculate Growth Temperature; Dang et al (2018)
  GDGT.IND$GT.DA        <-   21.39*GDGT.IND$MBT.6Me+2.27
  
  ### 16
  #calculate UK37-temp, Harning et al (2020)
  GDGT.IND$UKT.HA       <-   (-0.154*GDGTs$IIIa.5Me)+(0.3538*GDGTs$Ia)+(1.0016*GDGTs$IIIa.6Me)-0.7537
  
  ### 17
  #calculate MAAT; SUN et al
  GDGT.IND$MAAT.SUN     <-   6.803 - (7.062*GDGT.IND$CBT)+(37.09*GDGT.IND$MBT)
  
  ### 18
  #calculate MAAT; Russell et al (2018)
  GDGT.IND$MAAT1.RU     <-   (-1.21)+(32.42*GDGT.IND$MBT.5Me)
  
  ### 19
  #calculate MAAT; Russell et al (2018)
  GDGT.IND$MAAT2.RU     <-   12.22+(18.79*GDGT.IND$INDEX1)
  
  ### 20
  #calculate MAAT; Loomis et al. (2012)
  GDGT.IND$MAAT1.LO    <-   23.81 -(31.02*GDGTs[,c("IIIa.5Me")])-(41.91*GDGTs[,c("IIIb.5Me")])
  -(51.59*GDGTs[,c("IIb.6Me")])-(24.7*GDGTs[,c("IIa.5Me")])+(68.8*GDGTs[,c("Ib")])
  
  ### 21
  #calculate MAAT; Loomis et al. (2012)
  GDGT.IND$MAAT2.LO    <-   23.81 -(31.02*GDGTs[,c("IIIa.5Me")])-(41.91*GDGTs[,c("IIIb.5Me")])
  -(51.59*GDGTs[,c("IIb.6Me")])-(24.7*GDGTs[,c("IIa.5Me")])+(68.8*GDGTs[,c("Ib")])
  
  ### 22
  #calculate MAAT; Loomis et al. (2012)
  GDGT.IND$MAAT3.LO    <-   23.81 -(31.02*GDGTs[,c("IIIa.5Me")])-(41.91*GDGTs[,c("IIIb.5Me")])
  -(51.59*GDGTs[,c("IIb.6Me")])-(24.7*GDGTs[,c("IIa.5Me")])+(68.8*GDGTs[,c("Ib")])
  
  ### 23
  #calculate MAAT SFS; Russell et al (2018)
  GDGT.IND$SFS.RU     <-   23.81 -(31.02*GDGTs[,c("IIIa.5Me")])-(41.91*GDGTs[,c("IIIb.5Me")])
                            -(51.59*GDGTs[,c("IIb.6Me")])-(24.7*GDGTs[,c("IIa.5Me")])+(68.8*GDGTs[,c("Ib")])
  
  ### 24
  #calculate MWT; Zhao et al. (2020)
  GDGT.IND$MWT.ZH       <-    -1.82 + 56.06*GDGT.IND$MBT.5Me
  
  ### 25
  #calculate MAF.METH; Raberg et al. (2021)
  brGDGT.ME.FA        <- as.matrix(brGDGT.METH.5Me.FA)# for col assignment
  
  for(i in 1:nrow(brGDGT.ME.FA)){
  
  GDGT.IND$MAF.METH[i]       <-  (92.9 + (63.84*(as.numeric(brGDGT.ME.FA[i,c("Ib")])^2))-(130.51*(as.numeric(brGDGT.ME.FA[i,c("Ib")])))
                                  -(28.77*(as.numeric(brGDGT.ME.FA[i,c("IIa.5Me")])^2))-(72.28*(as.numeric(brGDGT.ME.FA[i,c("IIb.5Me")])^2))
                                  -(5.88*(as.numeric(brGDGT.ME.FA[i,c("IIc.5Me")])^2))+(20.89*(as.numeric(brGDGT.ME.FA[i,c("IIIa.5Me")])^2))
                                  -(40.54*as.numeric(brGDGT.ME.FA[i,c("IIIa.5Me")]))-(80.47*as.numeric(brGDGT.ME.FA[i,c("IIIb.5Me")])))
  }

  ### 26
  #calculate MAF.FULL; Raberg et al. (2021)
  brGDGT.F  <- as.matrix(brGDGT.FA) #for col assignment
  
  for(r in 1:nrow(brGDGT.F)){
  
  GDGT.IND$MAF.FULL[r]       <-    ((-8.06)+(37.52*as.numeric(brGDGT.F[r,c("Ia")]))-(266.83*(as.numeric(brGDGT.F[r,c("Ib")])^2))
                                    +(133.42*as.numeric(brGDGT.F[r,c("Ib")]))+(100.85*(as.numeric(brGDGT.F[r,c("IIa.6Me")])^2))
                                    +(58.15*(as.numeric(brGDGT.F[r,c("IIIa.6Me")])^2))+(12.79*as.numeric(brGDGT.F[r,c("IIIa.5Me")])))
  }
  
  
  ### 27
  #calculate MAAT.trop, zhao et al. (2023)
  GDGT.IND$MAAT.trop      <-  (-1.78)+31.01*GDGT.IND$MBT.5Me
  
  ### 28
  #calculate MLR-lowlat, zhao et al. (2023)
  GDGT.IND$MLR.trop       <-  (-4.11)+31.63*fIa.Zh+64.5*fIb.Zh+32.28*fIIa.Zh
  
  ### 29
  #calculate MLR-highlat
  GDGT.IND$MAF.highlat      <-  3.36+17.25*GDGT.IND$MBT.5Me
  
  ### 30
  #calculate MLR-highlat
  GDGT.IND$MLR.highlat      <-  1.44+15.88*fIa.Zh+66.92*fIb.Zh+8.33*fIIa.Zh+7.02*fIIIa.Zh
  
  ### 31
  #calculate IIIa/(IIIa+IIIa')
  GDGT.IND$IIIa.IIIaIIIa  <-   GDGTs$IIIa.5Me/ rowSums(GDGTs[,c("IIIa.5Me","IIIa.6Me")])
  
  ### 32
  #calculate sum (IIIa)/ sum (IIa)
  GDGT.IND$IIIa.IIa  <-   rowSums(GDGTs[,c("IIIa.5Me","IIIa.6Me")]) / rowSums(GDGTs[,c("IIa.5Me","IIa.6Me")])
  
  ### 33
  #calculate HP 5Me
  GDGT.IND$HP5Me        <-   GDGTs$IIIa.5Me/rowSums(GDGTs[,c("IIIa.5Me","IIa.5Me")])
  
  ### 34
  #calculate RING tetra
  GDGT.IND$RINGtetra    <-   (GDGTs$Ib + 2*GDGTs$Ic)/rowSums(GDGTs[,c("Ia","Ib","Ic")])
  
  ### 35
  #calculate RING penta 5Me
  GDGT.IND$RINGpenta5    <-   (GDGTs$IIb.5Me + 2*GDGTs$IIc.5Me)/rowSums(GDGTs[,c("IIa.5Me","IIb.5Me","IIc.5Me")])
  
  ### 36
  #calculate RING penta 6Me
  GDGT.IND$RINGpenta6    <-   (GDGTs$IIb.6Me + 2*GDGTs$IIc.6Me)/rowSums(GDGTs[,c("IIa.6Me","IIb.6Me","IIc.6Me")])
  
  ### 37
  #calculate DC
  GDGT.IND$DC          <-   (GDGTs$Ib + 2*GDGTs$Ic + GDGTs$IIb.5Me + GDGTs$IIb.6Me)/
                            rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me")])
  
  ### 38
  #calculate IBT
  GDGT.IND$IBT      <-   (-log(rowSums(GDGTs[,c("IIa.6Me","IIIa.6Me")])/
                                      rowSums(GDGTs[,c("IIa.5Me","IIIa.5Me")]),
                                    base = 10))
  
  ### 39
  #calculate IBT
  GDGT.IND$CI      <-   GDGTs$Ia/rowSums(GDGTs[,c("Ia","IIa.5Me","IIIa.5Me")])
 
  ### 40
  #calculate BIT
  GDGT.IND$BIT      <-  rowSums(GDGTs[,c("Ia","IIa.5Me","IIIa.5Me")])/rowSums(GDGTs[,c("Ia","IIa.5Me","IIIa.5Me","GDGT.4")]) 
  
  return(GDGT.IND)
}


#**************************************************************************************************************************#
#******************************************************* END **************************************************************#
#**************************************************************************************************************************#