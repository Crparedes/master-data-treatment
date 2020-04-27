library(ggplot2)
library(transmem)
PDF <- FALSE
#-----STOCK SOLUTIONS--------------------------------------------------------
StockLi.200 <- 129.5 * 0.187872 * 0.99 / 0.1200962
StockNa.11000 <- 1.1693 * 0.996 /41.5065 * 0.393372 * 1000000

StockLi.5_4   <- StockLi.200 * 1.2447 / 50.0609
StockNa.600_1 <- StockNa.11000  * 1.6505 / 30.1080
StockNa.10_1  <- StockNa.600_1  * 0.6065 / 30.0068
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CalCurves <- list(
  Lithium.P = data.frame(Conc = c(0.0000, 0.1220, 0.5995, 2.4008, 0.0623, 0.3031, 1.2507, 2.7362, 0.0000, 0.1253,
                                  0.6163, 2.4954, 0.0625, 0.3036, 1.2362, 2.7323, 0.0000, 0.1260, 0.6249, 2.4205,
                                  0.0596, 0.3088, 1.2048, 2.7345) *
                                StockLi.5_4 / c(6.0000, 6.0991, 6.1728, 6.0965, 6.0264, 6.0103, 6.0094, 6.0192,
                                                6.0388, 6.1271, 6.1162, 6.0195, 6.1010, 6.0030, 6.1259, 6.0507,
                                                6.1173, 6.0616, 6.1872, 6.0193, 6.0224, 6.1524, 6.1101, 6.0337),
                         Signal = c(0.000, 0.013, 0.062, 0.251, 0.007, 0.034, 0.141, 0.303, 0.000, 0.015, 0.069,
                                    0.279, 0.008, 0.036, 0.140, 0.307, 0.000, 0.015, 0.071, 0.276, 0.007, 0.036,
                                    0.139, 0.309),
                         Conc.S = c(0.0000, 0.0000, 0.0000, 0.0000, 0.2621, 0.2507, 0.2686, 0.2700, 0.5049, 0.5253,
                                    0.5298, 0.5256, 0.8370, 0.7558, 0.8183, 0.8020, 1.0036, 1.0174, 1.0144, 1.0304,
                                    2.0503, 2.0534, 2.0552, 2.0457) *
                                  StockNa.600_1 / c(6.0000, 6.0991, 6.1728, 6.0965, 6.0264, 6.0103, 6.0094, 6.0192,
                                                    6.0388, 6.1271, 6.1162, 6.0195, 6.1010, 6.0030, 6.1259, 6.0507,
                                                    6.1173, 6.0616, 6.1872, 6.0193, 6.0224, 6.1524, 6.1101, 6.0337)),
  Sodium.1 = data.frame(Conc = c(0.0000, 0.0606, 0.1258, 0.3025, 0.6048, 1.5792, 3.0508, 4.2181) *
                               StockNa.10_1 / c(6.0000, 6.0803, 6.1447, 6.0491, 6.0041, 6.0502, 6.0284, 6.0531),
                        Signal  = c(0.000, 0.025, 0.054, 0.124, 0.246, 0.587, 0.975, 1.187))
)

CalCurves <- list(
  Lithium.P = data.frame(Conc = c(0.0000, 0.1220, 0.5995, 2.4008, 0.0623, 0.3031, 1.2507, 2.7362, 0.0000, 0.1253,
                                  0.6163, 2.4954, 0.0625, 0.3036, 1.2362, 2.7323, 0.0000, 0.1260, 0.6249, 2.4205,
                                  0.0596, 0.3088, 1.2048, 2.7345) *
                           StockLi.5_4 / c(6.0000, 6.0991, 6.1728, 6.0965, 6.0264, 6.0103, 6.0094, 6.0192,
                                           6.0388, 6.1271, 6.1162, 6.0195, 6.1010, 6.0030, 6.1259, 6.0507,
                                           6.1173, 6.0616, 6.1872, 6.0193, 6.0224, 6.1524, 6.1101, 6.0337),
                         Signal = c(0.000, 0.013, 0.060, 0.245, 0.007, 0.034, 0.141, 0.303, 0.000, 0.015, 0.069,
                                    0.279, 0.008, 0.036, 0.140, 0.307, 0.000, 0.015, 0.071, 0.276, 0.007, 0.036,
                                    0.139, 0.309),
                         Conc.S = c(0.0000, 0.0000, 0.0000, 0.0000, 0.2621, 0.2507, 0.2686, 0.2700, 0.5049, 0.5253,
                                    0.5298, 0.5256, 0.8370, 0.7558, 0.8183, 0.8020, 1.0036, 1.0174, 1.0144, 1.0304,
                                    2.0503, 2.0534, 2.0552, 2.0457) *
                           StockNa.600_1 / c(6.0000, 6.0991, 6.1728, 6.0965, 6.0264, 6.0103, 6.0094, 6.0192,
                                             6.0388, 6.1271, 6.1162, 6.0195, 6.1010, 6.0030, 6.1259, 6.0507,
                                             6.1173, 6.0616, 6.1872, 6.0193, 6.0224, 6.1524, 6.1101, 6.0337)),
  Sodium.1 = data.frame(Conc = c(0.0000, 0.0606, 0.1258, 0.3025, 0.6048, 1.5792, 3.0508, 4.2181) *
                          StockNa.10_1 / c(6.0000, 6.0803, 6.1447, 6.0491, 6.0041, 6.0502, 6.0284, 6.0531),
                        Signal  = c(0.000, 0.025, 0.054, 0.124, 0.246, 0.587, 0.975, 1.187))
)
## for a cleaner workspace
#rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
CalModels <- list(
  Lithium.P = calibPlane(plane = CalCurves$Lithium.P[1:20, ]),
  Sodium.1 = calibCurve(curve = CalCurves$Sodium.1, order = 2)
)
anova(CalModels$Lithium.P$model)
summary(CalModels$Lithium.P$model)
#-----MUESTRAS CIEGAS--------------------------------------------------------
BlindeP <- data.frame(LiRe = c(0.4062, 0.0921, 0.2189, 0.3157, 1.4315, 1.5697, 0.9967, 0.9701, 2.0010) * StockLi.5_4 /
                        c(6.4381, 6.0603, 6.0808, 6.0657, 6.0402, 6.1427, 6.2846, 6.2175, 6.0563),
                      LiSg = c(0.043, 0.011, 0.024, 0.036, 0.162, 0.170, 0.103, 0.109, 0.250),
                      NaRe = c(0.1385, 0.2696, 0.1867, 0.3856, 0.5885, 0.5897, 0.1488, 0.6064, 0.7205) * StockNa.600_1 /
                        c(6.4381, 6.0603, 6.0808, 6.0657, 6.0402, 6.1427, 6.2846, 6.2175, 6.0563),
                      NaDl = c(1.5125/0.1739, 1.4951/0.1358, 1.5241/0.1679, 1.5112/0.1543, 1.5171/0.1540,
                               1.5175/0.1541, 1.5170/0.1556, 1.5125/0.1555, 1.5205/0.1561),
                      NaSg = c(0.310, 0.489, 0.416, 0.723, 1.000, 0.988, 0.322, 1.007, 1.140))
BlindeP$NaIn <- signal2conc(signal = BlindeP$NaSg, model = CalModels$Sodium.1, dilution = BlindeP$NaDl)
plot(x = BlindeP$NaRe, y = BlindeP$NaIn)
abline(a = 0, b = 1, col = 2, lty = 3)
abline(lm(BlindeP$NaIn ~ BlindeP$NaRe))
BlindeP$LiIn <- signal2conc(signal = BlindeP$LiSg, model = CalModels$Lithium.P, planar = TRUE,
                            Conc.S = BlindeP$NaIn)
BlindeP$LilI <- signal2conc(signal = BlindeP$LiSg, model = calibCurve(CalCurves$Lithium.P[21:24, ]))
plot(x = BlindeP$LiRe, y = BlindeP$LiIn)
abline(a = 0, b = 1, col = 2, lty = 3)
plot(x = BlindeP$LiRe, y = BlindeP$LilI)
abline(a = 0, b = 1, col = 2, lty = 3)

abline(lm(BlindeP$LiIn ~ BlindeP$LiRe))
summary(lm(BlindeP$LiIn ~ BlindeP$LiRe))
## NO HAY EFECTO MATRIZ POR EL ÁCIDO CLORHÍDRICO O EL HIDRÓXIDO DE AMONIO:
t.test(x = BlindeP$LiIn, y = BlindeP$LiRe, paired = TRUE)

#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- list (
  T.12.1 = c(0, 1, 2.5, 3.25, 4, 5, 6, 7, 8, 22, 23),
  T.12.2 = c(0, 1, 2, 3, 4, 6, 6.75, 7.5, 8.5, 22, 23.25),
  T.12.3 = c(0, 1, 2, 2.5, 3.75, 5, 6, 7, 10, 21.75, 22.75),
  T.12.4 = c(0, 1, 2, 2.75, 3.75, 5.25, 6, 7, 8, 25, 26),
  T.12.5 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 22.25, 23),
  T.12.6 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 22.5, 23),
  T.12.7 = c(0, 0.5, 1.75, 3.33, 4, 5, 6, 7, 9, 23, 23.83),
  T.12.8 = c(0, 1, 2, 3, 4, 5, 6, 7, 10.25, 25.25, 26)
)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
dilutions <- list(
  Feed.12.1  = c(1.5085/0.1233, 1.4740/0.1233, 1.5166/0.1242, 1.5094/0.1245, 1.5022/0.1238, 1.5177/0.1239),
  Strip.12.1 = c(1.5293/0.2227, 1.5063/0.2153, 1.5071/0.2150, 1.5154/0.2145, 1.5184/0.2143, 1.5162/0.2132),
  Feed.12.2  = c(1.5085/0.1243, 1.5046/0.1253, 1.5251/0.1236, 1.5040/0.1175, 1.5133/0.1248, 1.5113/0.1251),
  Strip.12.2 = c(1.5171/0.2183, 1.5311/0.2228, 1.5126/0.2231, 1.5206/0.2222, 1.5300/0.2257, 1.5240/0.2227),
  Feed.12.3  = c(1.5071/0.1238, 1.5067/0.1237, 1.5102/0.1246, 1.4546/0.1251, 1.5149/0.1249, 1.5123/0.1260),
  Strip.12.3 = c(1.5003/0.2181, 1.5283/0.2210, 1.5320/0.2198, 1.5288/0.2209, 1.5194/0.2207, 1.5288/0.2201),
  Feed.12.4  = c(1.5050/0.1266, 1.5094/0.1263, 1.5085/0.1269, 1.5031/0.1256, 1.5157/0.1274, 1.5051/0.1272),
  Strip.12.4 = c(1.4782/0.1985, 1.5351/0.2220, 1.5274/0.2217, 1.5163/0.2228, 1.5285/0.2240, 1.5262/0.2226),
  Feed.12.5  = c(1.5071/0.1255, 1.5062/0.1263, 1.5052/0.1249, 1.5184/0.1256, 1.5147/0.1263, 1.5165/0.1255),
  Strip.12.5 = c(1.5495/0.1824, 1.5198/0.1804, 1.5328/0.1822, 1.5323/0.1820, 1.5416/0.1821, 1.5280/0.1824),
  Feed.12.6  = c(1.7905/0.1257, 1.5205/0.1264, 1.5092/0.1266, 1.5070/0.1261, 1.5091/0.1265, 1.4969/0.1260),
  Strip.12.6 = c(1.5348/0.1814, 1.5480/0.1817, 1.5252/0.1815, 1.5322/0.1816, 1.5312/0.1812, 1.5760/0.1806),
  Feed.12.7  = c(1.5113/0.1262, 1.5110/0.1252, 1.5109/0.1269, 1.5117/0.1257, 1.5146/0.1263, 1.5538/0.1266),
  Strip.12.7 = c(1.5332/0.1817, 1.5282/0.1814, 1.4768/0.1821, 1.5070/0.1819, 1.5300/0.1815, 1.5413/0.1824),
  Feed.12.8  = c(1.5107/0.1244, 1.5137/0.1261, 1.5111/0.1172, 1.4990/0.1274, 1.5750/0.1224, 1.5637/0.1205),
  Strip.12.8 = c(1.5274/0.1814, 1.5341/0.1823, 1.5277/0.1799, 1.5299/0.1827, 1.5250/0.1259, 1.5713/0.1235)
)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(
  Feed.12.1.Li   = c(0.265, 0.139, 0.065, 0.049, 0.035, 0.022, 0.015, 0.011, 0.008, 0.001, 0.001),
  Strip.12.1.Li  = c(0.002, 0.127, 0.196, 0.209, 0.222, 0.231, 0.239, 0.245, 0.246, 0.255, 0.256),
  Feed.12.1.Na   = c(0.900, 0.915, 0.903, 0.900, 0.897, 0.875),
  Strip.12.1.Na  = c(0.016, 0.031, 0.043, 0.055, 0.070, 0.128),
  Feed.12.2.Li   = c(0.274, 0.241, 0.218, 0.200, 0.189, 0.164, 0.158, 0.152, 0.143, 0.095, 0.091),
  Strip.12.2.Li  = c(0.000, 0.027, 0.049, 0.066, 0.080, 0.100, 0.108, 0.113, 0.120, 0.167, 0.169),
  Feed.12.2.Na   = c(1.345, 1.345, 1.341, 1.307, 1.350, 1.354) *0.8,
  Strip.12.2.Na  = c(0.018, 0.022, 0.026, 0.028, 0.045, 0.111),
  Feed.12.3.Li   = c(0.263, 0.181, 0.149, 0.138, 0.117, 0.104, 0.096, 0.090, 0.080, 0.077, 0.076),
  Strip.12.3.Li  = c(0.000, 0.073, 0.107, 0.118, 0.141, 0.155, 0.164, 0.171, 0.181, 0.189, 0.190),
  Feed.12.3.Na   = c(0.897, 0.871, 0.873, 0.897, 0.876, 0.847),
  Strip.12.3.Na  = c(0.020, 0.033, 0.052, 0.069, 0.094, 0.112),
  Feed.12.4.Li   = c(0.267, 0.207, 0.168, 0.138, 0.116, 0.090, 0.080, 0.069, 0.060, 0.014, 0.014),
  Strip.12.4.Li  = c(0.000, 0.052, 0.092, 0.119, 0.142, 0.168, 0.178, 0.188, 0.197, 0.240, 0.240),
  Feed.12.4.Na   = c(0.914, 0.923, 0.918, 0.917, 0.920, 0.911),
  Strip.12.4.Na  = c(0.010, 0.014, 0.021, 0.026, 0.032, 0.069),
  Feed.12.5.Li   = c(0.270, 0.167, 0.111, 0.079, 0.057, 0.042, 0.032, 0.025, 0.020, 0.003, 0.003),
  Strip.12.5.Li  = c(0.001, 0.095, 0.149, 0.181, 0.199, 0.213, 0.222, 0.231, 0.233, 0.243, 0.249),
  Feed.12.5.Na   = c(0.913, 0.930, 0.926, 0.917, 0.932, 0.924),
  Strip.12.5.Na  = c(0.016, 0.013, 0.022, 0.022, 0.023, 0.036),
  Feed.12.6.Li   = c(0.270, 0.207, 0.161, 0.133, 0.113, 0.096, 0.082, 0.072, 0.066, 0.020, 0.019),
  Strip.12.6.Li  = c(0.000, 0.046, 0.092, 0.122, 0.145, 0.160, 0.176, 0.184, 0.190, 0.237, 0.239),
  Feed.12.6.Na   = c(0.803, 0.930, 0.932, 0.930, 0.934, 0.949),
  Strip.12.6.Na  = c(0.012, 0.009, 0.013, 0.014, 0.020, 0.026),
  Feed.12.7.Li   = c(0.270, 0.198, 0.107, 0.054, 0.044, 0.031, 0.023, 0.018, 0.011, 0.002, 0.002),
  Strip.12.7.Li  = c(0.001, 0.067, 0.155, 0.205, 0.214, 0.226, 0.234, 0.238, 0.246, 0.256, 0.256),
  Feed.12.7.Na   = c(0.917, 0.914, 0.917, 0.911, 0.906, 0.916),
  Strip.12.7.Na  = c(0.023, 0.023, 0.037, 0.052, 0.065, 0.109),
  Feed.12.8.Li   = c(0.269, 0.169, 0.113, 0.077, 0.057, 0.042, 0.033, 0.025, 0.013, 0.002, 0.002),
  Strip.12.8.Li  = c(0.001, 0.095, 0.151, 0.183, 0.201, 0.216, 0.223, 0.229, 0.243, 0.254, 0.254),
  Feed.12.8.Na   = c(0.932, 0.965, 0.878, 0.955, 0.900, 0.876),
  Strip.12.8.Na  = c(0.013, 0.019, 0.023, 0.025, 0.023, 0.039)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
expLi <- c(0.8520, 0.8502, 0.8573, 0.8492, 0.8540, 0.8566, 0.8580, 0.8571) * StockLi.200 /
  c(85.1935, 85.3400, 85.6001, 85.0758, 85.2523, 85.1237, 85.1622, 85.1411)
expNa <- c(0.5364, 1.0065, 0.5243, 0.5180, 0.5254, 0.5235, 0.5153, 0.5226) * StockNa.11000 /
  c(85.1935, 85.3400, 85.6001, 85.0758, 85.2523, 85.1237, 85.1622, 85.1411)
detLi <- detNa <- vector()

AliConc <- list()
for (i in 1:8) {
  eval(parse(text = paste0("AliConc$Feed.12.", i, ".Na <- signal2conc(signal = AliAbs$Feed.12.", i, ".Na,
                            model = CalModels$Sodium.1, dilution = dilutions$Feed.12.", i, ")")))
  eval(parse(text = paste0("AliConc$Strip.12.", i, ".Na <- signal2conc(signal = AliAbs$Strip.12.", i, ".Na,
                            model = CalModels$Sodium.1, dilution = dilutions$Strip.12.", i, ")")))
  eval(parse(text = paste0("AliConc$Feed.12.", i, ".Li <- signal2conc(signal = AliAbs$Feed.12.", i, ".Li,
                            model = CalModels$Lithium.P, planar = TRUE,
                            Conc.S = fixSecondary(conc = AliConc$Feed.12.", i, ".Na,
                                                  time = AliTimes$T.12.", i, "[c(1, 3, 5, 7, 9, 11)],
                                                  compTime = AliTimes$T.12.", i, ", order = 2))")))
  eval(parse(text = paste0("AliConc$Strip.12.", i, ".Li <- signal2conc(signal = AliAbs$Strip.12.", i, ".Li,
                            model = CalModels$Lithium.P, planar = TRUE,
                            Conc.S = fixSecondary(conc = AliConc$Strip.12.", i, ".Na,
                                                  time = AliTimes$T.12.", i, "[c(1, 3, 5, 7, 9, 11)],
                                                  compTime = AliTimes$T.12.", i, ", order = 2))")))
  eval(parse(text = paste0("detLi <- c(detLi, AliConc$Feed.12.", i, ".Li[1])")))
  eval(parse(text = paste0("detNa <- c(detNa, AliConc$Feed.12.", i, ".Na[1])")))
}
t.test(x = expLi, y = detLi)
t.test(x = expNa, y = detNa)
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- list()
for (i in 1:8) {
  sub.Na <- c(1, 3, 5, 7, 9, 11)
  eval(parse(text = paste0("TransFrac$M.12.", i, ".Li = conc2frac(feed = AliConc$Feed.12.", i, ".Li,
                            strip = AliConc$Strip.12.", i, ".Li, time = AliTimes$T.12.", i, ")")))
  eval(parse(text = paste0("TransFrac$M.12.", i, ".Na = conc2frac(feed = AliConc$Feed.12.", i, ".Na,
                            strip = AliConc$Strip.12.", i, ".Na, time = AliTimes$T.12.", i, "[sub.Na])")))
  #eval(parse(text = paste0("TransFrac$M.12.", i, ".Na$Fraction[which(TransFrac$M.12.", i, ".Na$Phase == 'Strip')] <-
  #  1 - TransFrac$M.12.", i, ".Na$Fraction[which(TransFrac$M.12.", i, ".Na$Phase == 'Feed')]")))
}
#rm(sub.Na)
#-----MODELOS DE REGRESIÓN NO LINEAL-----------------------------------------
TransNLS <- list()
SS_par <- vector()
for (i in 1:8) {
  eval(parse(text = paste0("X <- transTrend(TransFrac$M.12.", i, ".Li, model = 'paredes', eccen = 1)")))
  SS_par <- c(SS_par, sum(resid(X$feed)^2), sum(resid(X$strip)^2))
  eval(parse(text = paste0("TransNLS$M.12.", i, " <- X")))

}

TransNLSXot <- list()
SS_xot <- vector()
for (i in 1:8) {
  eval(parse(text = paste0("X <- transTrend(TransFrac$M.12.", i, ".Li, model = 'rodriguez')")))
  SS_xot <- c(SS_xot, sum(resid(X$feed)^2), sum(resid(X$strip)^2))
    eval(parse(text = paste0("TransNLSXot$M.12.", i, " <- X")))
}
t.test(x = SS_par, y = SS_xot, paired = TRUE)
#-----PERFILES DE TRANSPORTE ------------------------------------------------
if (PDF) pdf("Perfiles19-06-12.pdf", height = 7/1.8, width = 9/1.8)

Parameters <- data.frame()
for (i in 1:8) {
# invisible(readline(prompt="Press [enter] to continue"))

  eval(parse(text = paste0("transPlot(trans = TransFrac$M.12.", i, ".Li, trend = TransNLS$M.12.", i, ",
                             secondary = TransFrac$M.12.", i, ".Na, xlim = c(0, 26.1), ylim = c(-0.05, 1.08),
                             ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = c(0, 4, 8, 12, 16, 20, 24))")))
  eval(parse(text = paste0("Parameters <- rbind(Parameters, TransNLS$M.12.", i, "$Result)")))
}
for (i in 1:8) {
  eval(parse(text = paste0("transPlot(trans = TransFrac$M.12.", i, ".Li, trend = TransNLS$M.12.", i, ",
                             xlim = c(0, 26.1), ylim = c(-0.05, 1.08),
                             ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = c(0, 4, 8, 12, 16, 20, 24))")))
}
colnames(Parameters) <- names(TransNLS$M.12.1$Result)
round(Parameters, 3)

#-----FACTORES DE SEPARACIÓN-------------------------------------------------
sepFactor <- list()
for (i in 1:8) {
  eval(parse(text = paste0("X <- data.frame(time = AliTimes$T.12.", i, ",
                                   factor = (AliConc$Strip.12.", i, ".Li/
                                     fixSecondary(conc = AliConc$Strip.12.", i, ".Na,
                                                  time = AliTimes$T.12.", i, "[c(1, 3, 5, 7, 9, 11)],
                                                  compTime = AliTimes$T.12.", i, ", order = 2)) /
                                            (AliConc$Feed.12.", i, ".Li[1]/AliConc$Feed.12.", i, ".Na[1]))")))
  X <- X[-1, ]
  eval(parse(text = paste0("sepFactor$M.12.", i, " <- X")))
}
ssepFactor <- data.frame()

for (i in 1:8) ssepFactor <- rbind(ssepFactor, rbind(c(0, 1), sepFactor[[i]]))

ssepFactor$Membrana <- as.factor(paste0('Mem.12.', rep(1:8, each = 11)))
ggplot(data = ssepFactor, aes(x = time, y = factor, colour = Membrana)) + geom_point() + theme_bw() +
  ggsci::scale_color_npg() + stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, size = 0.4) +
  xlab(label = "Tiempo (horas)") + ylab(label = "Factor de separación")

ggplot(data = ssepFactor, aes(x = time, y = factor, colour = Membrana)) + geom_point() + theme_bw() +
  ggsci::scale_color_npg() + stat_smooth(method = "loess", se = FALSE, size = 0.4) +
  xlab(label = "Tiempo (horas)") + ylab(label = "Factor de separación")

ssepFactor$PIM <- as.factor(paste0('H.', rep(1:8, each = 11)))
ggplot(data = ssepFactor, aes(x = time, y = factor, shape = PIM)) + geom_point() + theme_bw() +
  stat_smooth(method = "loess", se = FALSE, size = 0.4, color = 'black', span = 0.4) +
  scale_shape_manual(values=1:nlevels(ssepFactor$Membrana)) +
  xlab(label = "Tiempo (horas)") + ylab(label = "Factor de separación")

sF <- vector()
for (i in 1:8) sF <- c(sF, mean(sepFactor[[i]][, 2]))
sF1 <- vector()
for (i in 1:8) sF1 <- c(sF1, max(sepFactor[[i]][, 2]))
sF2 <- vector()
for (i in 1:8) sF2 <- c(sF2, sepFactor[[i]][10, 2])

if (PDF) dev.off()
