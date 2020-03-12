library(transmem)
A <- 2.9576*(0.08502959)/(20.3837+2.9576)
B <- 0.3257*(1.627547)/(20.3917+0.3257)
C <- 1.2368*(1.627547)/(19.0493+1.2368)
D <- 2.4622*(1.627547)/(18.0569+2.4622)
#-----STOCK SOLUTIONS--------------------------------------------------------
StockMg.1000_4 <- 1000
StockMg.2p5_4  <- StockMg.1000_4 * 0.1300 / 50.0295
StockCa.1000_4 <- 0.1242 * 0.40043 / 50.2750 * 1000000
StockCa.10_4   <- 0.1220 * StockCa.1000_4 / 12.2309
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CalCurves <- list(
  Magnessium.1 = data.frame(Conc = c(0.0000, 0.1332, 0.9991, 1.9961, 2.9974, 4.0234) * StockMg.2p5_4 /
                                       c(6.0000, 6.1608, 6.0342, 6.1474, 6.2812, 6.1459),
                            Signal = c(0.000, 0.043, 0.308, 0.548, 0.737, 0.896)),
  Calcium.1 = data.frame(Conc = c(0.0000, 0.0744, 0.2916, 0.5724, 1.8285, 2.9865, 4.2062) * StockCa.10_4 /
                                       c(6.0000, 6.3447, 6.3288, 6.2018, 6.4452, 6.1708, 6.1998),
                            Signal = c(0.000, 0.008, 0.033, 0.068, 0.202, 0.345, 0.484)),
  Magnessium.2 = data.frame(Conc = c(0.0000, 0.1332, 0.9991, 1.9961, 2.9974, 4.0234) * StockMg.2p5_4 /
                              c(6.0000, 6.1608, 6.0342, 6.1474, 6.2812, 6.1459),
                            Signal = c(0.000, 0.043, 0.308, 0.548, 0.737, 0.896)),
  Calcium.2 = data.frame(Conc = c(0.0000, 0.0744, 0.2916, 0.5724, 1.8285, 2.9865, 4.2062) * StockMg.2p5_4 /
                           c(6.0000, 6.3447, 6.3288, 6.2018, 6.4452, 6.1708, 6.1998),
                         Signal = c(0.000, 0.008, 0.033, 0.068, 0.202, 0.345, 0.484))
)
#-----MODELOS DE LAS CURVAS--------------------------------------------------
Order <- c(2, 1)
CalModels <- list()
for (i in 1:2) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = Order[i])
names(CalModels) <- names(CalCurves)
#-----MODELOS DE LAS CURVAS--------------------------------------------------
NaOHConc <- c(0.3257, 1.2368, 2.4622) * mean(naoh[3:4]) / c(0.3257 + 20.3912, 1.2368 + 19.0493, 2.4622 + 18.0569)
c(0.1, 0.125, 0.15) * 20 / mean(naoh[3:4])
0.15 * 100 / mean(naoh[3:4])
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
dilutionsMg <- c(60330/669*19813/2080, 20666/10073, 20247/10143)#SSS, C, D
dilutionsCa <- c(60330/669, 62139/606, 61140/614, 19946/949*19041/1276, 19569/976*19703/1326)#SSS, A, B, C, D
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbsMg <- c(0.653, 0.280, 0)
AliAbsCa <- c(0.276, 0.212, 0.244, 0.071, 0.055)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConcMg <- signal2conc(signal = AliAbsMg, model = CalModels$Magnessium.1, dilution = dilutionsMg)
AliConcCa <- signal2conc(signal = AliAbsCa, model = CalModels$Calcium.1, dilution = dilutionsCa)
