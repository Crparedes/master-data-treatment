# Liquid-Solid extractions using PIMS
source("18-AugustDecember/18-10-05-Li-MemX.R")
# LIX-54-100, D2EHP, TEHP, Cyanex 923, TBP
MW <- c(246, 322.43, 434.6, 348, 266.318)
#Moles used in first stage
molused1 <- 15e-3/MW
mean(molused1); sd(molused1)
#lithium moles present:
40e-3/6.941*0.025
#Lithium moles extracted:
molextracted1 <- c(0.18, 0.33, 0.03, 0.05, 0.06)*1e-3/6.941*0.030
#Lithium extraction efficiency:
ConcFaseRec_1*masaFaseRec/(44*1000*0.025)*100
#Corrected efficiency
molextracted1/molused1[c(3, 4, 5, 3, 4)]*100

40+33+16
40/89;33/89;16/89
#Moles used in seccond stage
