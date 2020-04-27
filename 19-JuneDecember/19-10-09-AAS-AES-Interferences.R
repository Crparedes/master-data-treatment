library('transmem')
library('ggplot2')
StockNa.5000_1  <- 0.6399 * 0.996 / 50.0160 * 0.393372 * 1000000
StockNa.200_3   <- 1.2006 *  StockNa.5000_1 / 30.2104
StockNa.20_3    <- 3.0048 *  StockNa.200_3 / 30.2730
StockNa.2_3     <- 3.0019 *  StockNa.20_3 / 29.5762
StockK.10000_1  <- 0.9592 * 0.996 / 50.0812 * 0.52445 * 1000000
StockK.200_3    <- 0.5958 * StockK.10000_1 / 30.2276
StockK.20_3     <- 3.1167 * StockK.200_3 / 30.1430
StockK.2_3      <- 3.0613 * StockK.20_3 / 30.2162
StockMg.10000_1 <- 1.7698 * 0.99 / 50.0892 * 0.28827 * 1000000
StockMg.200_3   <- 0.6036 * StockMg.10000_1 / 30.4746
StockMg.40_3    <- 5.1314 * StockMg.200_3 / 20.5494
StockMg.4_3     <- 2.9958 * StockMg.40_3 / 30.0684
  
CalCurves <- list(
  # For sodium the order is AAS-LaCl5, AAS-KCl, AAS-LiCl, AAS-CsCl3, AES-LaCl5, AES-KCl, AES-LiCl, AES-CsCl3
  # Cesium is the only one that really sucks
  Sodium.1 = data.frame(Conc = c(0.0000, 0.0554, 0.1119, 0.3022, 0.6121, 0.8995) *
                          StockNa.2_3 / c(2.0000, 2.0248, 2.0477, 2.0366, 2.0531, 2.0452),
                        Signal  = c(0, 0.008, 0.015, 0.042, 0.084, 0.127)),
  Sodium.2 = data.frame(Conc = c(0.0000, 0.0483, 0.1233, 0.2988, 0.6114, 0.9029) *
                          StockNa.2_3 / c(2.0000, 2.0141, 2.0331, 2.0488, 2.0692, 2.0138),
                        Signal  = c(0, 0.010, 0.019, 0.045, 0.096, 0.141)),
  Sodium.3 = data.frame(Conc = c(0.0000, 0.0495, 0.1009, 0.3094, 0.6029, 0.9212) *
                          StockNa.2_3 / c(2.0000, 2.0489, 2.0266, 2.0925, 2.0645, 2.1462),
                        Signal  = c(0, 0.011, 0.017, 0.046, 0.088, 0.126)),
  Sodium.4 = data.frame(Conc = c(0.0000, 0.1125, 0.2988, 0.6058, 0.9027) *
                          StockNa.2_3 / c(2.0000, 2.0011, 2.0507, 2.1100, 2.2047),
                        Signal  = c(0, 0.004, 0.043, 0.064, 0.098)),
  Sodium.5 = data.frame(Conc = c(0.0000, 0.0554, 0.1119, 0.3022, 0.6121, 0.8995) *
                          StockNa.2_3 / c(2.0000, 2.0248, 2.0477, 2.0366, 2.0531, 2.0452),
                        Signal  = c(0, 0.097, 0.143, 0.402, 0.766, 1.119)),
  Sodium.6 = data.frame(Conc = c(0.0000, 0.0483, 0.1233, 0.2988) *
                          StockNa.2_3 / c(2.0000, 2.0141, 2.0331, 2.0488),
                        Signal  = c(0.112, 0.180, 0.296, 0.511)),
  Sodium.7 = data.frame(Conc = c(0.0000, 0.0495, 0.1009, 0.3094, 0.6029) *
                          StockNa.2_3 / c(2.0000, 2.0489, 2.0266, 2.0925, 2.0645),
                        Signal  = c(0.230, 0.288, 0.360, 0.630, 0.926)),
  Sodium.8 = data.frame(Conc = c(0.0000, 0.1125, 0.2988, 0.6058) *
                          StockNa.2_3 / c(2.0000, 2.0011, 2.0507, 2.1100),
                        Signal  = c(0.020, 0.070, 0.274, 0.371)),
  # For Potassium the order is AAS-CsCl3, AAS-Alone, AAS-LaCl5, AAS-LiCl, AES-CsCl3, AES-Alone, AES-LaCl5, AES-LiCl
  # AES-LiCL -> Buena dependencia, orden 2  AAS -> Regular
  # AES-La -> malo                          AAS -> muy malo
  # AES Alone -> bueno                      AAS -> bueno
  # AES-Cs -> Pésimo                        AAS -> Pésimo
  Potassium.1 = data.frame(Conc = c(0.0000, 1.0503, 2.1497, 3.0142, 3.5976) * StockK.2_3 /
                             c(6.0000, 6.1914, 6.2460, 6.1937, 6.0239),
                           Signal = c(0.000, 0.035, 0.101, 0.210, 0.359)),
  Potassium.2 = data.frame(Conc = c(0.0000, 0.1063, 0.3115, 0.7090, 1.2164) * StockK.2_3 /
                             c(2.0000,2.0203, 2.1492, 2.0015, 2.0978),
                           Signal = c(0.000, 0.023, 0.055, 0.139, 0.220)),
  Potassium.3 = data.frame(Conc = c(0.0000, 0.0598, 0.1042, 0.3128, 0.7068, 1.2105) * StockK.2_3 /
                             c(2.0000, 2.1117, 2.0702, 2.1592, 2.0599, 2.1290),
                           Signal = c(0.000, 0.004, 0.015, 0.086, 0.107, 0.201)),
  Potassium.4 = data.frame(Conc = c(0.0000, 0.1111, 0.2935, 0.7116, 1.2256) * StockK.2_3 /
                             c(2.0000, 2.0061, 2.0223, 2.1984, 2.0371),
                           Signal = c(0.000, 0.018, 0.052, 0.142, 0.233)),
  Potassium.5 = data.frame(Conc = c(0.0000, 0.3740, 1.0503, 2.1497, 3.0142, 3.5976) * StockK.2_3 /
                             c(6.0000, 6.1350, 6.1914, 6.2460, 6.1937, 6.0239),
                           Signal = c(0.104, 0.202, 0.213, 0.416, 0.722, 1.1095)),
  Potassium.6 = data.frame(Conc = c(0.0000, 0.0565, 0.1063, 0.3115, 0.7090, 1.2164) * StockK.2_3 /
                             c(2.0000, 2.0179, 2.0203, 2.1492, 2.0015, 2.0978),
                           Signal = c(0.000, 0.078, 0.134, 0.288, 0.684, 1.058)),
  Potassium.7 = data.frame(Conc = c(0.0000, 0.0598, 0.1042, 0.3128, 0.7068, 1.2105) * StockK.2_3 /
                             c(2.0000, 2.1117, 2.0702, 2.1592, 2.0599, 2.1290),
                           Signal = c(0.000, 0.059, 0.087, 0.200, 0.661, 0.961)),
  Potassium.8 = data.frame(Conc = c(0.0000, 0.1111, 0.2935, 0.7116, 1.2256) * StockK.2_3 /
                             c(2.0000, 2.0061, 2.0223, 2.1984, 2.0371),
                           Signal = c(0.000, 0.129, 0.347, 0.731, 1.162)),
  # For Magnessium the order is AAS-Alone, AAS-LaCl5, AAS-KCl, AES-KCl+LaCl5
  
  Magnessium.1 = data.frame(Conc = c(0.0000, 0.1062, 0.3056, 0.6106, 0.9048) * StockMg.4_3 /
                             c(2.0000, 2.0648, 2.1102, 2.0322, 2.0463),
                           Signal = c(0.000, 0.121, 0.330, 0.630, 0.842)),
  Magnessium.2 = data.frame(Conc = c(0.0000, 0.1069, 0.3314, 0.5968, 0.9099) * StockMg.4_3 /
                              c(2.0000, 2.0058, 2.1982, 2.0323, 2.1550),
                            Signal = c(0.000, 0.130, 0.351, 0.619, 0.816)),
  Magnessium.3 = data.frame(Conc = c(0.0000, 0.1129, 0.2975, 0.6203, 0.9058) * StockMg.4_3 /
                              c(2.0000, 2.0294, 2.0774, 2.1202, 2.0181),
                            Signal = c(0.000, 0.135, 0.330, 0.615, 0.848)),
  Magnessium.4 = data.frame(Conc = c(0.0000, 0.1188, 0.3301, 0.6327, 0.9234) * StockMg.4_3 /
                              c(2.0000, 2.0166, 2.0758, 2.1708, 2.1375),
                            Signal = c(0.000, 0.136, 0.349, 0.596, 0.802))
)

CalModels <- list()
#pdf("SodiumLowCurves.pdf")
for (i in 1:4) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]])
for (i in 5:8) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = 2)

calibCurve(curve = CalCurves[[8]], order = 2)

Natrium <- function(i) {
  Nat <- ggplot(data = CalCurves[[i]], aes(x = Conc, y = Signal)) +
    theme_bw() + geom_point(size = 3, shape = 16)  + 
    labs(y = 'Señal (cuentas)', x = expression(paste('Concentración mg k', g^{-1}))) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black")) + 
    #scale_y_continuous(limits = c(0, -5)) + 
    scale_x_continuous(limits = c(-1, 7)) +  
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1.2)) +
    geom_smooth(method = 'lm', formula = y ~ poly(x, 2), fullrange = TRUE, color = 'black', size = 0.4)
  print(Nat)
} 
Natrium(5)
Natrium(6)
Natrium(7)
Natrium(8)



plot(CalModels[[7]]$residuals)
for (i in 9:12) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]])
for (i in 13:16) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = 2)
for (i in 17:20) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = 2)
dev.off()






















#Old curves another ranges...
StockNa.200_2 <- StockNa.5000_1  * 0.4820 / 12.0120
StockK.200_2 <- StockK.10000_1  * 0.2382 / 11.9997
StockMg.400_2 <- StockMg.10000_1  * 0.4847 / 12.0128
StockNa.20_2 <- StockNa.200_2  * 1.2264 / 12.1149
StockK.20_2 <- StockK.200_2  * 1.0029 / 10.0097
StockMg.40_2 <- StockMg.400_2  * 1.2079 / 12.0953
StockNa.2_2 <- StockNa.20_2  * 0.9967 / 9.9949
StockK.2_2 <- StockK.200_2  * 0.1003 / 10.0355
StockMg.4_2 <- StockMg.40_2  * 1.1864 / 11.8529
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CalCurves <- list(
  Sodium.1 = data.frame(Conc = c(0.0000, 0.2670, 0.7204, 1.4464, 1.9935, 2.4448) *
                          StockNa.2_2 / c(6.0000, 6.1236, 6.3596, 6.1113, 6.1979, 6.0654),
                        Signal  = c(0, 0.015, 0.023, 0.072, 0.129, 0.210)),
  Sodium.2 = data.frame(Conc = c(0.0000, 0.2716, 0.7276, 1.4523, 1.9869, 2.7014) *
                          StockNa.20_2 / c(6.0000, 6.2121, 6.1135, 6.1808, 6.3065, 6.0991),
                        Signal  = c(0, 0.199, 0.489, 0.807, 0.949, 1.115)),
  Sodium.3 = data.frame(Conc = c(0.0000, 0.1549, 0.3028, 0.9062, 1.7930, 2.6960) *
                          StockNa.200_2 / c(6.0000, 6.1139, 6.1620, 6.0301, 6.2480, 6.0015),
                        Signal  = c(0, 0.016, 0.048, 0.096, 0.130, 0.161)),
  Sodium.4 = data.frame(Conc = c(0.0000, 0.2670, 0.7204, 1.4464, 1.9935, 2.4448) *
                          StockNa.2_2 / c(6.0000, 6.1236, 6.3596, 6.1113, 6.1979, 6.0654),
                        Signal  = c(0, 0.042, 0.065, 0.185, 0.300, 0.465)),
  Sodium.5 = data.frame(Conc = c(0.0000, 0.2716, 0.7276, 1.4523, 1.9869, 2.7014) *
                          StockNa.20_2 / c(6.0000, 6.2121, 6.1135, 6.1808, 6.3065, 6.0991),
                        Signal  = c(0, 0.201, 0.427, 0.644, 0.740, 0.883)),
  Sodium.6 = data.frame(Conc = c(0.0000, 0.1549, 0.3028, 0.9062, 1.7930, 2.6960) *
                          StockNa.200_2 / c(6.0000, 6.1139, 6.1620, 6.0301, 6.2480, 6.0015),
                        Signal  = c(0, 0.385, 0.612, 0.869, 1.014, 1.117)),
  Potassium.1 = data.frame(Conc = c(0.0000, 1.0503, 2.1497, 3.0142, 3.5976) * StockK.2_2 /
                             c(6.0000, 6.1914, 6.2460, 6.1937, 6.0239),
                           Signal = c(0.000, 0.054, 0.116, 0.216, 0.362)),
  Potassium.2 = data.frame(Conc = c(0.0000, 0.3676, 1.0262, 2.0223, 3.0185, 3.5562) * StockK.20_2 /
                             c(6.0000, 6.0034, 6.1308, 6.0078, 6.0480, 6.2173),
                           Signal = c(0.000, 0.167, 0.448, 0.720, 1.731, 2.414)),
  Potassium.3 = data.frame(Conc = c(0.0000, 0.1600, 0.3074, 0.9089, 2.0894, 3.6151) * StockK.200_2 /
                             c(6.0000, 6.1403, 6.0614, 6.0049, 6.0857, 6.0186),
                           Signal = c(0.000, 0.014, 0.025, 0.053, 0.075, 0.087)),
  Potassium.5 = data.frame(Conc = c(0.0000, 0.3676, 1.0262, 2.0223, 3.0185, 3.5562) * StockK.20_2 /
                             c(6.0000, 6.0034, 6.1308, 6.0078, 6.0480, 6.2173),
                           Signal = c(0.000, 0.241, 0.503, 0.759, 0.939, 1.006)),
  Potassium.6 = data.frame(Conc = c(0.0000, 0.1600, 0.3074, 0.9089, 2.0894, 3.6151) * StockK.200_2 /
                             c(6.0000, 6.1403, 6.0614, 6.0049, 6.0857, 6.0186),
                           Signal = c(0.000, 0.388, 0.649, 0.910, 1.074, 1.173)),
  Magnessium.1 = data.frame(Conc = c(0.0000, 0.2737, 0.7372, 1.4480, 2.0974, 2.7042) * StockMg.4_2 /
                              c(6.0000, 6.1481, 6.1110, 6.1526, 6.1464, 6.1803),
                            Signal = c(0.000, 0.047, 0.084, 0.285, 0.511, 0.714)),
  Magnessium.2 = data.frame(Conc = c(0.0000, 0.2724, 0.7445, 1.4443, 2.1060, 2.7087) * StockMg.40_2 /
                              c(6.0000, 6.0633, 6.1393, 6.2075, 6.1327, 6.0602),
                            Signal = c(0.000, 0.013, 0.038, 0.063, 0.080, 0.099)),
  Magnessium.3 = data.frame(Conc = c(0.0000, 0.1557, 0.2975, 0.9373, 1.8112) * StockMg.400_2 /
                              c(6.0000, 6.0923, 6.0968, 6.0901, 6.2097),
                            Signal = c(0.000, 0.095, 0.150, 0.169, 0.176)),
  Magnessium.5 = data.frame(Conc = c(0.0000, 0.2724, 0.7445, 1.4443, 2.1060) * StockMg.40_2 /
                              c(6.0000, 6.0633, 6.1393, 6.2075, 6.1327),
                            Signal = c(0.000, 0.023, 0.053, 0.065, 0.079)),
  Magnessium.6 = data.frame(Conc = c(0.0000, 0.1557, 0.2975, 0.9373, 1.8112, 2.7180) * StockMg.400_2 /
                              c(6.0000, 6.0923, 6.0968, 6.0901, 6.2097, 6.2260),
                            Signal = c(0.000, 0.058, 0.092, 0.136, 0.158, 0.185))
)
calibCurve(CalCurves[[1]])
