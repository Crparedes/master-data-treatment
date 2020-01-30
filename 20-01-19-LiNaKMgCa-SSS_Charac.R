library(transmem)
PDF <- FALSE
if (PDF) pdf("SSS-20-01-19.pdf", height = 7/1.3, width = 9/1.3)

#-----STOCK SOLUTIONS--Prepared January 18th, 2020---------------------------
StockLi.622  <- 100.4 * 0.18787 * 0.990 / 0.0300314
StockNa.6627 <- 508.4 * 0.39337 * 0.995 / 0.0300249
StockK.1768  <- 101.8 * 0.52445 * 0.995 / 0.0300397
StockMg.979  <- 304.7 * 0.09479 * 1.014 / 0.0300828
StockCa.1349 <- 102.3 * 0.40043 * 0.990 / 0.0300715

StockLi.500u <- StockLi.622 * 0.6197 / 6.1725 * 0.4114 / 50.0378 * 1000
StockNa.20   <- StockNa.6627 * 0.1546 / 50.3288
StockK.10    <- StockK.1768 * 0.3235 / 50.0196
StockMg.2    <- StockMg.979 * 0.1182 / 51.2115
StockCa.10   <- StockCa.1349 * 0.3882 / 51.5038
#-----CALIBRATION CURVES AND MODELS------------------------------------------
CalCurves <- list(
  Lithium   = data.frame(Conc = c(0, 0.0572, 0.2463, 1.2093, 2.4374, 3.1012) * StockLi.500u / 
                           c(6, 6.1763, 6.1816, 6.1501, 6.2182, 6.1090),
                         Signal = c(0, 0.010, 0.044, 0.216, 0.427, 0.550)),
  Sodium    = data.frame(Conc = c(0, 0.6103, 1.1981, 1.7972, 2.3932, 3.0180) * StockNa.20 /
                           c(6, 6.0091, 6.0703, 6.0552, 6.0101, 6.0426),
                         Signal  = c(0, 0.454, 0.693, 0.855, 0.972, 1.078))[1:5, ],
  Potassium = data.frame(Conc = c(0, 0.1161, 0.3845, 0.7267, 0.9607, 1.2154) * StockK.10 /
                           c(6, 6.0537, 6.0643, 6.0318, 6.0002, 6.0493),
                         Signal = c(0, 0.109, 0.333, 0.593, 0.752, 0.888)),
  Magnesium = data.frame(Conc = c(0, 0.6181, 1.2064, 1.7995, 2.4384, 3.0945) * StockMg.2 /
                           c(6, 6.0880, 6.1408, 6.0009, 6.1014, 6.0274),
                         Signal = c(0, 0.091, 0.171, 0.250, 0.325, 0.398)),
  Calcium   = data.frame(Conc = c(0, 0.6213, 1.2149, 1.8130, 2.4250, 3.1000) * StockCa.10 /
                           c(6, 6.0524, 6.0340, 6.0274, 6.0292, 6.1189),
                         Signal = c(0, 0.060, 0.119, 0.176, 0.237, 0.292))
)
## for a cleaner workspace
#rm(list = ls()[grep("Stock", ls())])
CalModels <- list()
order = c(1, 2, 2, 2, 2)
for (i in 1:5) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = order[i], plot = TRUE)
names(CalModels) <- names(CalCurves)
summary(CalModels$Lithium)

#-----SEAWATER DILUTIONS AND MEASUREMENTS--------------------------------------
SWN.dil.100  <- 12.0202 / 0.1284
SWN.dil.400  <- SWN.dil.100 * 12.0035 / 3.0535
SWN.dil.2000 <- SWN.dil.400 * 12.0125 / 2.4116

SWS.dil.100  <- 11.9230 / 0.1186
SWS.dil.400  <- SWS.dil.100 * 12.0113 / 3.0016
SWS.dil.2000 <- SWS.dil.400 * 12.0407 / 2.4136

# Lithium in SWN, SWN, SWN, SWS, SWS, SWS
LithiumESCC <- signal2conc(signal = c(0.444, 0.440, 0.420, 0.475, 0.466, 0.447), model = CalModels$Lithium)

FinalMass <- c(1.6268, 1.6731, 1.7587, 1.6503, 1.6949, 1.7560)
SpikeMass <- c(0.0479, 0.0977, 0.1575, 0.0518, 0.0987, 0.1583)
InitiMass <- FinalMass - SpikeMass
SPSA.Em01 <- c(rep(0.440, 3), rep(0.466, 3))
SPSA.Em1 <- c(0.453, 0.461, 0.472, 0.480, 0.495, 0.497)
SPSA.Em02 <- c(rep(0.420, 3), rep(0.447, 3))
SPSA.Em2 <- c(0.442, 0.446, 0.464, 0.463, 0.481, 0.503)

LithiumSPSA1 <- (SPSA.Em01 * StockLi.500u * (SpikeMass/FinalMass)) / (SPSA.Em1 - SPSA.Em01 * (InitiMass / FinalMass))
LithiumSPSA2 <- (SPSA.Em02 * StockLi.500u * (SpikeMass/FinalMass)) / (SPSA.Em2 - SPSA.Em02 * (InitiMass / FinalMass))
# Data in SPSA1 is not trustable due to instrumental noise
SodiumESCC    <- signal2conc(signal = c(0.834, 0.831), model = CalModels$Sodium, 
                             dilution = c(SWN.dil.2000, SWS.dil.2000))
PotassiumESCC <- signal2conc(signal = c(0.456, 0.092, 0.454, 0.096), model = CalModels$Potassium, 
                             dilution = c(SWN.dil.400, SWN.dil.2000, SWS.dil.400, SWS.dil.2000))
MagnesiumESCC <- signal2conc(signal = c(0.240, 0.239), model = CalModels$Magnesium, 
                             dilution = c(SWN.dil.2000, SWS.dil.2000))
CalciumESCC   <- signal2conc(signal = c(0.225, 0.059, 0.225, 0.057), model = CalModels$Calcium,
                             dilution = c(SWN.dil.100, SWN.dil.400, SWS.dil.100, SWS.dil.400))

cat(paste0('Lithium concentration in SWN by ESCC: ', round(mean(LithiumESCC[1:3]), 0), 
           ' +/- ', round(sd(LithiumESCC[1:3]), 0), ' ug/kg \n',
           'Lithium concentration in SWN by SPSA: ', round(mean(LithiumSPSA2[1:3]), 0), 
           ' +/- ', round(sd(LithiumSPSA2[1:3]), 0), ' ug/kg \n', 
           'Lithium concentration in SWS by ESCC: ', round(mean(LithiumESCC[4:6]), 0), 
           ' +/- ', round(sd(LithiumESCC[4:6]), 0), ' ug/kg \n',
           'Lithium concentration in SWS by SPSA: ', round(mean(LithiumSPSA2[4:6]), 0), 
           ' +/- ', round(sd(LithiumSPSA2[4:6]), 0), ' ug/kg \n\n',
           'Sodium concentration in SWN by ESCC: ', round(SodiumESCC[1], 0), ' mg/kg \n', 
           'Sodium concentration in SWS by ESCC: ', round(SodiumESCC[2], 0), ' mg/kg \n\n', 
           'Potassium concentration in SWN by ESCC: ', round(mean(PotassiumESCC[1:2]), 0), 
           ' +/- ', round(sd(PotassiumESCC[1:2]), 0), ' mg/kg \n', 
           'Potassium concentration in SWS by ESCC: ', round(mean(PotassiumESCC[3:4]), 0), 
           ' +/- ', round(sd(PotassiumESCC[3:4]), 0), ' mg/kg \n\n',
           'Magnesium concentration in SWN by ESCC: ', round(MagnesiumESCC[1], 0), ' mg/kg \n', 
           'Magnesium concentration in SWS by ESCC: ', round(MagnesiumESCC[2], 0), ' mg/kg \n\n', 
           'Calcium concentration in SWN by ESCC: ', round(mean(CalciumESCC[1:2]), 0), 
           ' +/- ', round(sd(CalciumESCC[1:2]), 0), ' mg/kg \n', 
           'Calcium concentration in SWS by ESCC: ', round(mean(CalciumESCC[3:4]), 0), 
           ' +/- ', round(sd(CalciumESCC[3:4]), 0), ' mg/kg \n'))


if (PDF) dev.off()
6*40*.05
