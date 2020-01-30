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
  Magnessium.1 = data.frame(Conc = c(0.0000, 0.1332, 0.9991) * StockMg.2p5_4 /
                                       c(6.0000, 6.1608, 6.0342),
                            Signal = c(0.000, 0.043, 0.289)),
  Calcium.1 = data.frame(Conc = c(0.0000, 0.0744, 0.2916, 0.5724, 1.8285, 2.9865, 4.2062) * StockCa.10_4 /
                                       c(6.0000, 6.3447, 6.3288, 6.2018, 6.4452, 6.1708, 6.1998),
                            Signal = c(0.000, 0.003, 0.030, 0.067, 0.205, 0.350, 0.485))
)
#-----MODELOS DE LAS CURVAS--------------------------------------------------
Order <- c(2, 1)
CalModels <- list()
for (i in 1:2) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = Order[i])
names(CalModels) <- names(CalCurves)
#-----MODELOS DE LAS CURVAS--------------------------------------------------
PO4_S <- 6.608 / 132.0562 / 50.002 * 1e3
NaOH_s <-mean(c(232.7, 224.7) / 204.22 / c(0.7210, 0.6570))
NaOHConc <- c(0.6510, 0.7794, 0.8647, 1.0015) * mean(naoh[3:4]) / c(10.1257, 10.0009, 10.0011, 10.0680)
HPO4Conc <- c(0.0644, 0.1054, 0.2464, 0.5403) * (6.608 / 132.0562 / 50.002 * 1e3) / c(10.0426, 10.0381, 10.0363, 10.0652)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
dilutionsMg <- c(20027/5200, 20263/5107, 19994/5129, 19951/4939)
dilutionsCa <- rep(1, 4)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbsMg <- c(0.096, 0.106, 0.039, 0.055)
AliAbsCa <- c(0.026, 0.019, 0.014, 0.056)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConcMg <- signal2conc(signal = AliAbsMg, model = CalModels$Magnessium.1, dilution = dilutionsMg)
AliConcCa <- signal2conc(signal = AliAbsCa, model = CalModels$Calcium.1, dilution = dilutionsCa)

# Desiciones
# NaOH: 0.15 M -> 1028 * 0.15 / NaOH6N
# HPO4: 0.005 M -> 0.005 * 1005 / PO4_S
