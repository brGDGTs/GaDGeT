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
#          17.  MAAT.SUN:       Sun et al............ 
#          18.  MAAT1.RU:       Russell et al. (2018)
#          19.  MAAT2.RU:       Russell et al. (2018)
#          20.  SFS.RU:         Russell et al. (2018)
#          21.  MWT.ZH:         Zhao et al. (2020)
#          22.  MAF.METH:       Raberg et al. (2021)
#          23.  MAF.FULL:       Raberg et al. (2021)
#          24.  MAAT.trop:      Zhao et al. (2023)
#          25.  MLR.trop:       Zhao et al. (2023)
#          26.  MAF.highlat:    Zhao et al. (2023)
#          27.  MLR.highlat:    Zhao et al. (2023)
#          28.  IIIa.IIIaIIIa:  Raberg et al. (2021)
#          29.  IIIa.IIa:       Raberg et al. (2021)

#          30.  HP5Me:          Yao et al. (2020)
#          31.  RINGtetra:      Raberg et al. (2021) 
#          32.  RINGpenta 5Me:  Raberg et al. (2021)
#          33.  RINGpenta 6Me:  Raberg et al. (2021)
#          34.  DC:             Raberg et al. (2021)
#          35.  IBT:            Ding et al. (2015)
#          36.  CI:             Raberg et al. (2021)
#          37.  BIT:            Hopmans et al. (2004)


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
  
  GDGT.IND <- data.frame(GDGT.IND)
  
  ###-------------------------------------------- INDEX CALC --------------------------------------------------------------###
  
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
  #calculate MAAT SFS; Russell et al (2018)
  GDGT.IND$SFS.RU     <-   23.81 -(31.02*GDGTs[,c("IIIa.5Me")])-(41.91*GDGTs[,c("IIIb.5Me")])
                            -(51.59*GDGTs[,c("IIb.6Me")])-(24.7*GDGTs[,c("IIa.5Me")])+(68.8*GDGTs[,c("Ib")])
  
  ### 21
  #calculate MWT; Zhao et al. (2020)
  GDGT.IND$MWT.ZH       <-    -1.82 + 56.06*GDGT.IND$MBT.5Me
  
  ### 22
  #calculate MAF.METH; Raberg et al. (2021)
  brGDGT.ME.FA        <- as.matrix(brGDGT.METH.5Me.FA)# for col assignment
  
  for(i in 1:nrow(brGDGT.ME.FA)){
  
  GDGT.IND$MAF.METH[i]       <-  (92.9 + (63.84*(as.numeric(brGDGT.ME.FA[i,c("Ib")])^2))-(130.51*(as.numeric(brGDGT.ME.FA[i,c("Ib")])))
                                  -(28.77*(as.numeric(brGDGT.ME.FA[i,c("IIa.5Me")])^2))-(72.28*(as.numeric(brGDGT.ME.FA[i,c("IIb.5Me")])^2))
                                  -(5.88*(as.numeric(brGDGT.ME.FA[i,c("IIc.5Me")])^2))+(20.89*(as.numeric(brGDGT.ME.FA[i,c("IIIa.5Me")])^2))
                                  -(40.54*as.numeric(brGDGT.ME.FA[i,c("IIIa.5Me")]))-(80.47*as.numeric(brGDGT.ME.FA[i,c("IIIb.5Me")])))
  }

  ### 23
  #calculate MAF.FULL; Raberg et al. (2021)
  brGDGT.F  <- as.matrix(brGDGT.FA) #for col assignment
  
  for(r in 1:nrow(brGDGT.F)){
  
  GDGT.IND$MAF.FULL[r]       <-    ((-8.06)+(37.52*as.numeric(brGDGT.F[r,c("Ia")]))-(266.83*(as.numeric(brGDGT.F[r,c("Ib")])^2))
                                    +(133.42*as.numeric(brGDGT.F[r,c("Ib")]))+(100.85*(as.numeric(brGDGT.F[r,c("IIa.6Me")])^2))
                                    +(58.15*(as.numeric(brGDGT.F[r,c("IIIa.6Me")])^2))+(12.79*as.numeric(brGDGT.F[r,c("IIIa.5Me")])))
  }
  
  
  #prep for 25 and 27
  fIa        <- GDGTs[,c("Ia")] / rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me","IIIa.5Me","IIIa.6Me")])
  fIb        <- GDGTs[,c("Ib")] / rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me","IIIa.5Me","IIIa.6Me")])
  fIIa        <- GDGTs[,c("IIa.6Me")] / rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me","IIIa.5Me","IIIa.6Me")])
  fIIIa        <- GDGTs[,c("IIIa.5Me")] / rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me","IIIa.5Me","IIIa.6Me")])
  
  
  ### 24
  #calculate MAAT.trop, zhao et al. (2023)
  GDGT.IND$MAAT.trop      <-  (-1.78)+31.01*GDGT.IND$MBT.5Me
  
  ### 25
  #calculate MLR-lowlat, zhao et al. (2023)
  GDGT.IND$MLR.trop       <-  (-4.11)+31.63*fIa+64.5*fIb+32.28*fIIa
  
  ### 26
  #calculate MLR-highlat
  GDGT.IND$MAF.highlat      <-  3.36+17.25*GDGT.IND$MBT.5Me
  
  ### 27
  #calculate MLR-highlat
  GDGT.IND$MLR.highlat      <-  1.44+15.88*fIa+66.92*fIb+8.33*fIIa+7.02*fIIIa
  
  ### 28
  #calculate IIIa/(IIIa+IIIa')
  GDGT.IND$IIIa.IIIaIIIa  <-   GDGTs$IIIa.5Me/ rowSums(GDGTs[,c("IIIa.5Me","IIIa.6Me")])
  
  ### 29
  #calculate sum (IIIa)/ sum (IIa)
  GDGT.IND$IIIa.IIa  <-   rowSums(GDGTs[,c("IIIa.5Me","IIIa.6Me")]) / rowSums(GDGTs[,c("IIa.5Me","IIa.6Me")])
  
  ### 30
  #calculate HP 5Me
  GDGT.IND$HP5Me        <-   GDGTs$IIIa.5Me/rowSums(GDGTs[,c("IIIa.5Me","IIa.5Me")])
  
  ### 31
  #calculate RING tetra
  GDGT.IND$RINGtetra    <-   (GDGTs$Ib + 2*GDGTs$Ic)/rowSums(GDGTs[,c("Ia","Ib","Ic")])
  
  ### 32
  #calculate RING penta 5Me
  GDGT.IND$RINGpenta5    <-   (GDGTs$IIb.5Me + 2*GDGTs$IIc.5Me)/rowSums(GDGTs[,c("IIa.5Me","IIb.5Me","IIc.5Me")])
  
  ### 33
  #calculate RING penta 6Me
  GDGT.IND$RINGpenta6    <-   (GDGTs$IIb.6Me + 2*GDGTs$IIc.6Me)/rowSums(GDGTs[,c("IIa.6Me","IIb.6Me","IIc.6Me")])
  
  ### 34
  #calculate DC
  GDGT.IND$DC          <-   (GDGTs$Ib + 2*GDGTs$Ic + GDGTs$IIb.5Me + GDGTs$IIb.6Me)/
                            rowSums(GDGTs[,c("Ia","Ib","Ic","IIa.5Me","IIa.6Me","IIb.5Me","IIb.6Me")])
  
  ### 35
  #calculate IBT
  GDGT.IND$IBT      <-   (-log(rowSums(GDGTs[,c("IIa.6Me","IIIa.6Me")])/
                                      rowSums(GDGTs[,c("IIa.5Me","IIIa.5Me")]),
                                    base = 10))
  
  ### 36
  #calculate IBT
  GDGT.IND$CI      <-   GDGTs$Ia/rowSums(GDGTs[,c("Ia","IIa.5Me","IIIa.5Me")])
 
  ### 37
  #calculate BIT
  GDGT.IND$BIT      <-  rowSums(GDGTs[,c("Ia","IIa.5Me","IIIa.5Me")])/rowSums(GDGTs[,c("Ia","IIa.5Me","IIIa.5Me","GDGT.4")]) 
  
  return(GDGT.IND)
}


#**************************************************************************************************************************#
#******************************************************* END **************************************************************#
#**************************************************************************************************************************#