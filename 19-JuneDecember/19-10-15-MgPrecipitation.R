library(ggplot2)
library(ggformula)
library(transmem)
PDF <- FALSE
if (PDF) pdf("Perfiles11-10-19p2.pdf", height = 7/1.8, width = 9/1.8)

#-----STOCK SOLUTIONS--------------------------------------------------------
StockMg.10000_1 <- 1.7698 * 0.99 / 50.0892 * 0.28827 * 1000000
StockMg.200_3   <- 0.6036 * StockMg.10000_1 / 30.4746
StockMg.40_3    <- 5.1314 * StockMg.200_3 / 20.5494
StockMg.4_3     <- 2.9958 * StockMg.40_3 / 30.0684
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CalCurves <- list(
  Magnessium.1 = data.frame(Conc = c(c(0.0000, 0.1692, 0.3373, 0.9324, 1.8073, 2.7109) * StockMg.4_3 /
                              c(6.0000, 6.0576, 6.1379, 6.1271, 6.0632, 6.1367),
                              c(0.2776) * StockMg.40_3 / c(6.2208)),
                            Signal = c(0.000, 0.066, 0.126, 0.329, 0.590, 0.788, 0.764)),
  Magnessium.2 = data.frame(Conc = c(c(0.0000, 0.1692, 0.3373, 0.9324, 1.8073, 2.7109) * StockMg.4_3 /
                              c(6.0000, 6.0576, 6.1379, 6.1271, 6.0632, 6.1367),
                              c(0.2776) * StockMg.40_3 / c(6.2208)),
                            Signal = c(0.000, 0.065, 0.128, 0.343, 0.597, 0.800, 0.796))
)
## for a cleaner workspace
#rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
Order <- c(2, 2)
CalModels <- list()
for (i in 1:2) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = Order[i])
names(CalModels) <- names(CalCurves)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
dilutions <- c(5.0256/0.2005*5.0291/0.2240, 5.0101/0.2413*5.6470/0.2721, 50840/2066*50868/2037, 67163/112,
               70723/144, 66540/176, 62862/284, 60275/614, 61035/632, 62247/649, 61645/643, 60700/657)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- c(0.503, 0.627, 0.477, 0.484, 0.456, 0.721, 0.606, 0.509, 0.288, 0.144, 0.230, 0)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- signal2conc(signal = AliAbs, model = CalModels$Magnessium.2, dilution = dilutions)

Phos <- (c(0.0942, 0.1388, 0.1854, 0.2765, 0.3475, 0.5763, 0.8686, 1.2557) / 132.0562) / 
  (c(41.3717, 41.2758, 41.2779, 42.0300, 26.1036, 27.1305, 27.1249, 24.9340) / 1000)
132.0562 * 0.05
c(.05, .1) * 0.02 * 132.0562
(Mg <- 700 / 24.305 / 1000)
Mg * 2 / 3
