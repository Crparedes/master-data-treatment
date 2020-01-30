library(ggplot2)
library(ggformula)
library(transMem)
#-----STOCK SOLUTIONS--------------------------------------------------------
StockLi.1 <- 129.5 * 0.187872 * 0.99 / 0.1200962
StockNa.1 <- 0.0861 * 22990

StockLi.1.5_2   <- StockLi.1 * 1.2484 / 50.2322
StockLi.1.5_3   <- StockLi.1 * 1.2463 / 50.0083
StockNa.1.200_1 <- StockNa.1  * 5.0087 / 50.0536
StockNa.1.20_2  <- StockNa.1.200_1  * 1.1979 / 11.9780
StockNa.1.20_1  <- 0.0861 * 22990 * 0.5136 / 5.1747 * 1.0230 / 10.0315
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CalCurves <- list(
  Lithium.1 = data.frame(Conc = c(0.0000, 0.0615, 0.1223, 0.3027, 0.5948, 1.2023, 2.4034, 2.7270) *
                                StockLi.1.5_3 / c(6.0000, 6.0084, 6.0084, 6.0344, 6.1404, 6.0674, 6.0297, 6.1499),
                         Signal = c(0.000, 0.008, 0.016, 0.039, 0.077, 0.155, 0.306, 0.339),
                         Conc.S = rep(0.000, 8) *
                                  StockNa.1.200_1 / c(6.0000, 6.0084, 6.0084, 6.0344, 6.1404, 6.0674, 6.0297, 6.1499)),
  Lithium.2 = data.frame(Conc = c(0.0000, 0.0624, 0.1180, 0.2995, 0.6141, 1.2080, 2.4169, 2.7044) *
                                StockLi.1.5_3 / c(6.0075, 6.1143, 6.0310, 6.0232, 6.0195, 6.0341, 6.0633, 6.0151),
                         Signal = c(0.000, 0.008, 0.016, 0.041, 0.083, 0.164, 0.313, 0.349),
                         Conc.S = c(0.6026, 0.6020, 0.6056, 0.6257, 0.5895, 0.6113, 0.5844, 0.6145) *
                                StockNa.1.200_1 / c(6.0075, 6.1143, 6.0310, 6.0232, 6.0195, 6.0341, 6.0633, 6.0151)),
  Lithium.3 = data.frame(Conc = c(0.0000, 0.0654, 0.1176, 0.2990, 0.6088, 1.2090, 2.4115, 2.6965) *
                                StockLi.1.5_3 / c(6.0843, 6.0435, 6.0321, 6.0317, 6.0415, 6.0085, 6.0777, 6.1473),
                         Signal = c(0.000, 0.008, 0.016, 0.040, 0.082, 0.164, 0.313, 0.346),
                         Conc.S = c(1.2087, 1.2158, 1.2168, 1.1970, 1.2015, 1.2135, 1.2132, 1.2024) *
                                  StockNa.1.200_1 / c(6.0843, 6.0435, 6.0321, 6.0317, 6.0415, 6.0085, 6.0777, 6.1473)),
  Lithium.4 = data.frame(Conc = c(0.0000, 0.0649, 0.1172, 0.3176, 0.6101, 1.2036, 2.4131, 2.7617) *
                                StockLi.1.5_3 / c(6.0384, 6.0057, 6.0783, 6.0397, 6.0010, 6.0468, 6.0233, 6.0914),
                         Signal = c(0.000, 0.010, 0.017, 0.045, 0.088, 0.168, 0.327, 0.372),
                         Conc.S = c(0.6048, 0.6042, 0.6082, 0.6089, 0.6139, 0.5998, 0.5944, 0.6085) *
                                  StockNa.1 / c(6.0384, 6.0057, 6.0783, 6.0397, 6.0010, 6.0468, 6.0233, 6.0914)),
  Lithium.5 = data.frame(Conc = c(0.0000, 0.0591, 0.1298, 0.3080, 0.6139, 1.2670, 2.4016, 2.7171) *
                                StockLi.1.5_3 / c(6.0586, 6.0102, 6.0702, 6.0508, 6.0309, 6.0551, 6.0384, 6.0498),
                         Signal = c(0.000, 0.008, 0.019, 0.043, 0.088, 0.177, 0.326, 0.367),
                         Conc.S = c(0.7660, 0.7576, 0.7689, 0.7575, 0.7589, 0.7513, 0.7594, 0.7603) *
                                  StockNa.1 / c(6.0586, 6.0102, 6.0702, 6.0508, 6.0309, 6.0551, 6.0384, 6.0498)),
  Sodium.1 = data.frame(Conc = c(0.0000, 0.0614, 0.1405, 0.3050, 0.6079, 0.9008, 1.4124, 1.7866) *
                               StockNa.1.20_2 / c(6.0000, 6.3150, 6.0400, 6.1691, 6.0611, 6.0519, 6.1049, 6.2491),
                        Signal  = c(0.000, 0.037, 0.085, 0.172, 0.339, 0.492, 0.726, 0.876)),
  ## Experimento viejo (Mem.9.7, será tratada como si se tratase de la Mem.10.8):
  #Strip
  Lithium.6 = data.frame(Conc = c(0.0000, 0.0652, 0.1340, 0.3044, 0.6083, 1.2322, 2.3922, 3.0645) *
                                StockLi.1.5_2 / c(6.0000, 6.0075, 6.0268, 6.0103, 6.0341, 6.0633, 6.0103, 6.0626),
                         Signal = c(0.000, 0.007, 0.016, 0.035, 0.071, 0.147, 0.284, 0.349)),
  #Feed
  Lithium.7 = data.frame(Conc = c(0.0000, 0.0592, 0.1205, 0.2446, 0.5938, 1.2562, 2.4378, 3.0479) *
                                StockLi.1.5_2 / c(6.0000, 6.0992, 6.2358, 6.1827, 6.0371, 7.1252, 6.0531, 6.4249),
                         Signal = c(0.000, 0.008, 0.015, 0.040, 0.079, 0.142, 0.313, 0.362)),
  #Sodium
  Sodium.2  = data.frame(Conc = c(0.0000, 0.0548, 0.1433, 0.3205, 0.6178, 0.9372, 1.4929, 2.0799, 3.0523) *
                                StockNa.1.20_1 / c(6.0000, 6.1328, 6.0089, 6.1254, 6.1521, 6.1531, 6.1615, 6.1384, 6.1277),
                         Signal = c(0.000, 0.032, 0.090, 0.196, 0.368, 0.543, 0.848, 1.089, 1.424))
)
## for a cleaner workspace
rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
CalModels <- list(
  Lithium.1 = calibCurve(curve = CalCurves$Lithium.1),
  Lithium.2 = calibCurve(curve = CalCurves$Lithium.2),
  Lithium.3 = calibCurve(curve = CalCurves$Lithium.3),
  Lithium.4 = calibCurve(curve = CalCurves$Lithium.4),
  Lithium.5 = calibCurve(curve = CalCurves$Lithium.5),
  Lithium.6 = calibCurve(curve = CalCurves$Lithium.6, badpoint = 8),
  Lithium.7 = calibCurve(curve = CalCurves$Lithium.7, badpoint = 8),
  Sodium.1 = calibCurve(curve = CalCurves$Sodium.1, order = 2),
  Sodium.2 = calibCurve(curve = CalCurves$Sodium.2, order = 2)
)

#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- list (
  T.10.1 = c(0, 1, 2.5, 3.5, 4.25, 5, 5.75, 6.75, 8, 23.5, 24.5),
  T.10.2 = c(0, 1, 2.5, 3.5, 4.25, 5, 5.75, 6.75, 8, 23.5, 24.5),
  T.10.3 = c(0, 1, 2.5, 3.5, 4.25, 5, 5.75, 6.75, 8, 22.25, 23.25),
  T.10.4 = c(0, 1, 2.5, 3.5, 4.25, 5, 5.75, 6.75, 8, 22.25, 23.25),
  T.10.5 = c(0, 1, 2.5, 3.5, 4.25, 5, 5.75, 6.75, 8, 23.5, 24.5),
  T.10.6 = c(0, 1, 2.5, 3.5, 4.25, 5, 5.75, 6.75, 8, 23.5, 24.5),
  T.10.7 = c(0, 1, 2.5, 4, 4.5, 5, 5.75, 6.75, 8, 23.5, 24.5),
  T.10.8 = c(0, 1, 2.75, 4, 4.5, 5.75, 6.5, 8, 11, 23.33, 24.33)
)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
dilutions <- list(
  Feed.10.1  = c(6.0459/0.0589, 6.2249/0.0599, 6.0791/0.0596, 6.0555/0.0596, 6.0114/0.0593, 6.0464/0.0564),
  Strip.10.1 = c(1.8829/0.1765, 1.5053/0.1516, 1.5054/0.1533, 1.5180/0.1533, 1.5154/0.1513, 1.6379/0.1539),
  Feed.10.2  = c(6.0101/0.0600, 6.1231/0.0614, 6.0568/0.0610, 6.0133/0.0610, 6.0605/0.0589, 6.1831/0.0583),
  Strip.10.2 = c(1.5099/0.1496, 1.5086/0.1536, 1.5089/0.1519, 1.5224/0.1559, 1.5097/0.1536, 1.5051/0.1537),
  Feed.10.3  = c(6.2057/0.0645, 6.0911/0.0562, 6.0675/0.0641, 6.0418/0.0644, 6.0161/0.0583, 6.1814/0.0589),
  Strip.10.3 = c(1.5069/0.1511, 1.6902/0.1533, 1.5093/0.1540, 1.5162/0.1537, 1.5043/0.1536, 1.5180/0.1534),
  Feed.10.4  = c(6.0621/0.0629, 6.0381/0.0635, 6.0680/0.0625, 6.0861/0.0642, 6.0244/0.0633, 6.0144/0.0626),
  Strip.10.4 = c(1.5069/0.1500, 1.5063/0.1530, 1.5053/0.1537, 1.5124/0.1536, 1.5042/0.1522, 1.5070/0.1539),
  Feed.10.5  = c(6.0903/0.0614, 6.0298/0.0617, 6.0221/0.0633, 6.1257/0.0606, 6.0300/0.0586, 6.1100/0.0604),
  Strip.10.5 = c(1.5089/0.1515, 1.5012/0.1544, 1.5040/0.1539, 1.5063/0.1549, 1.5015/0.1553, 1.4994/0.1515),
  Feed.10.6  = c(6.2523/0.0610, 6.0496/0.0604, 6.1019/0.0616, 6.0172/0.0581, 6.1968/0.0604, 6.0326/0.0597),
  Strip.10.6 = c(1.4925/0.1535, 1.5230/0.1508, 1.5342/0.1582, 1.5248/0.1551, 1.5320/0.1545, 1.5160/0.1552),
  Feed.10.7  = c(6.1257/0.0623, 6.0150/0.0610, 6.1595/0.0616, 6.1295/0.0618, 6.0639/0.0616, 6.0005/0.0606),
  Strip.10.7 = c(1.5155/0.1506, 1.5275/0.1525, 1.5208/0.1549, 1.5221/0.1531, 1.5190/0.1526, 1.5184/0.1531),
  Feed.10.8  = c(6.1950/0.0593, 6.0393/0.0592, 6.0999/0.0595, 6.0316/0.0593, 6.0699/0.0591),
  Strip.10.8 = c(2.0209/0.2192, 2.0240/0.2241, 2.0188/0.2185, 2.0225/0.2222, 2.0204/0.2197)
)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(
  Feed.10.1.Li   = c(0.334, 0.247, 0.146, 0.098, 0.071, 0.052, 0.037, 0.025, 0.014, 0.000, 0.000),
  Strip.10.1.Li  = c(0.000, 0.082, 0.180, 0.224, 0.251, 0.270, 0.282, 0.295, 0.304, 0.321, 0.322),
  Feed.10.1.Na   = c(0.435, 0.410, 0.406, 0.402, 0.385, 0.305),
  Strip.10.1.Na  = c(0.000, 0.188, 0.339, 0.340, 0.632, 1.096),
  Feed.10.2.Li   = c(0.322, 0.181, 0.070, 0.024, 0.021, 0.013, 0.007, 0.003, 0.001, 0.001, 0.001),
  Strip.10.2.Li  = c(0.001, 0.140, 0.246, 0.282, 0.297, 0.309, 0.313, 0.317, 0.316, 0.322, 0.333),
  Feed.10.2.Na   = c(0.423, 0.402, 0.396, 0.387, 0.351, 0.307),
  Strip.10.2.Na  = c(0.000, 0.269, 0.400, 0.657, 0.793, 1.149),
  Feed.10.3.Li   = c(0.324, 0.186, 0.072, 0.034, 0.019, 0.011, 0.008, 0.002, 0.000, 0.000, 0.000),
  Strip.10.3.Li  = c(0.000, 0.135, 0.248, 0.283, 0.301, 0.310, 0.315, 0.316, 0.322, 0.327, 0.328),
  Feed.10.3.Na   = c(0.447, 0.388, 0.431, 0.424, 0.365, 0.314),
  Strip.10.3.Na  = c(0.000, 0.677, 0.396, 0.511, 0.672, 1.157),
  Feed.10.4.Li   = c(0.327, 0.261, 0.170, 0.108, 0.070, 0.048, 0.031, 0.017, 0.008, 0.000, 0.002),
  Strip.10.4.Li  = c(0.001, 0.056, 0.148, 0.210, 0.244, 0.266, 0.282, 0.295, 0.305, 0.320, 0.320),
  Feed.10.4.Na   = c(0.444, 0.451, 0.436, 0.443, 0.442, 0.429),
  Strip.10.4.Na  = c(0.000, 0.056, 0.096, 0.126, 0.164, 0.302),
  Feed.10.5.Li   = c(0.335, 0.199, 0.086, 0.046, 0.029, 0.018, 0.012, 0.007, 0.003, 0.000, 0.001),
  Strip.10.5.Li  = c(0.001, 0.125, 0.236, 0.281, 0.298, 0.310, 0.314, 0.326, 0.329, 0.329, 0.336),
  Feed.10.5.Na   = c(0.456, 0.434, 0.428, 0.388, 0.373, 0.314),
  Strip.10.5.Na  = c(0.000, 0.271, 0.649, 0.600, 0.786, 1.322),
  Feed.10.6.Li   = c(0.325, 0.179, 0.060, 0.028, 0.017, 0.011, 0.007, 0.004, 0.002, 0.000, 0.003),
  Strip.10.6.Li  = c(0.001, 0.140, 0.255, 0.288, 0.297, 0.305, 0.311, 0.311, 0.312, 0.322, 0.322),
  Feed.10.6.Na   = c(0.427, 0.428, 0.431, 0.408, 0.414, 0.409),
  Strip.10.6.Na  = c(0.000, 0.088, 0.139, 0.184, 0.229, 0.414),
  Feed.10.7.Li   = c(0.333, 0.152, 0.040, 0.015, 0.010, 0.007, 0.004, 0.002, 0.001, 0.000, 0.002),
  Strip.10.7.Li  = c(0.000, 0.177, 0.283, 0.312, 0.313, 0.321, 0.322, 0.327, 0.329, 0.331, 0.335),
  Feed.10.7.Na   = c(0.444, 0.420, 0.401, 0.391, 0.382, 0.308),
  Strip.10.7.Na  = c(0.000, 0.275, 0.494, 0.621, 0.802, 1.362),
  Feed.10.8.Li   = c(0.306, 0.158, 0.045, 0.018, 0.013, 0.005, 0.004, 0.002, 0.002, 0.001, 0.001),
  Strip.10.8.Li  = c(0.001, 0.143, 0.250, 0.273, 0.280, 0.290, 0.292, 0.289, 0.288, 0.294, 0.296),
  Feed.10.8.Na   = c(0.482, 0.463, 0.437, 0.424, 0.389),
  Strip.10.8.Na  = c(0.006, 0.502, 0.691, 0.952, 1.198)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- list()
for (i in 1:7) {
  eval(parse(text = paste0("AliConc$Feed.10.", i, ".Li = signal2conc(signal = AliAbs$Feed.10.", i, ".Li,
                            model = CalModels$Lithium.5)")))
  eval(parse(text = paste0("AliConc$Strip.10.", i, ".Li = signal2conc(signal = AliAbs$Strip.10.", i, ".Li,
                            model = CalModels$Lithium.1)")))
  eval(parse(text = paste0("AliConc$Feed.10.", i, ".Na = signal2conc(signal = AliAbs$Feed.10.", i, ".Na,
                            model = CalModels$Sodium.1, dilution = dilutions$Feed.10.", i, ")")))
  eval(parse(text = paste0("AliConc$Strip.10.", i, ".Na = signal2conc(signal = AliAbs$Strip.10.", i, ".Na,
                            model = CalModels$Sodium.1, dilution = dilutions$Strip.10.", i, ")")))
}
i = 8
eval(parse(text = paste0("AliConc$Feed.10.", i, ".Li = signal2conc(signal = AliAbs$Feed.10.", i, ".Li,
                            model = CalModels$Lithium.7)")))
eval(parse(text = paste0("AliConc$Strip.10.", i, ".Li = signal2conc(signal = AliAbs$Strip.10.", i, ".Li,
                            model = CalModels$Lithium.6)")))
eval(parse(text = paste0("AliConc$Feed.10.", i, ".Na = signal2conc(signal = AliAbs$Feed.10.", i, ".Na,
                            model = CalModels$Sodium.2, dilution = dilutions$Feed.10.", i, ")")))
eval(parse(text = paste0("AliConc$Strip.10.", i, ".Na = signal2conc(signal = AliAbs$Strip.10.", i, ".Na,
                            model = CalModels$Sodium.2, dilution = dilutions$Strip.10.", i, ")")))
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- list()
for (i in 1:8) {
  sub.Na <- c(1, 3, 5, 7, 9, 11)
  if (i == 8) sub.Na <- c(1, 4, 7, 9, 11)
  eval(parse(text = paste0("TransFrac$M.10.", i, ".Li = conc2frac(feed = AliConc$Feed.10.", i, ".Li,
                            strip = AliConc$Strip.10.", i, ".Li, time = AliTimes$T.10.", i, ")")))
  eval(parse(text = paste0("TransFrac$M.10.", i, ".Na = conc2frac(feed = AliConc$Feed.10.", i, ".Na,
                            strip = AliConc$Strip.10.", i, ".Na, time = AliTimes$T.10.", i, "[sub.Na])")))

  eval(parse(text = paste0("TransFrac$M.10.", i, ".Na$Fraction[which(TransFrac$M.10.", i, ".Na$Phase == 'Strip')] <-
    1 - TransFrac$M.10.", i, ".Na$Fraction[which(TransFrac$M.10.", i, ".Na$Phase == 'Feed')]")))
}
rm(sub.Na)
#-----MODELOS DE REGRESIÓN NO LINEAL-----------------------------------------
TransNLS <- list()
for (i in 1:8) {
  eval(parse(text = paste0("TransNLS$M.10.", i, " <- transTrend(TransFrac$M.10.", i, ".Li)")))
}

TransNLSXot <- list()
for (i in 1:8) {
  eval(parse(text = paste0("TransNLSXot$M.10.", i, " <- transTrend(TransFrac$M.10.", i, ".Li,
                                                                   model = 'rodriguez')")))
}

## Comparación entre ambos métodos
xx <- c(0.008263, 0.007911, 0.003013, 0.005681, 0.002705, 0.003321, 0.01055, 0.005887,
        0.004128, 0.006093, 0.001017, 0.00118, 0.0003279, 0.00103, 0.0009458, 0.001733)
yy <- c(0.002275, 0.002012, 0.002078, 0.001305, 0.001159, 0.0007701, 0.01818, 0.01842,
        0.0004967, 0.001356, 0.00136, 0.001588, 0.0003329, 0.0009354, 0.0002778, 0.0008073)
t.test(x = xx, y = yy, paired = TRUE)

#-----PERFILES DE TRANSPORTE ------------------------------------------------
#pdf("Perfiles19-05-21-normal.pdf", height = 5, width = 10)
for (i in 1:8) {
  eval(parse(text = paste0("transPlot(trans = TransFrac$M.10.", i, ".Li, trend = TransNLS$M.10.", i, ",
                             secondary = TransFrac$M.10.", i, ".Na, xlim = c(0, 25), ylim = c(-0.05, 1.15),
                             ybreaks = c(0, 0.25, 0.50, 0.75, 1, 1.1))")))
}
#dev.off()
###########################################################################
#-----REGRESIONES MULTIPARAMÉTRICAS (PLANARES)-----------------------------
#pdf("calibrationplanes19-05-19",  height = 5, width = 10)
CalCurves$Lithium.Plane.1 <- rbind(CalCurves$Lithium.1, CalCurves$Lithium.2, CalCurves$Lithium.3)
CalCurves$Lithium.Plane.2 <- rbind(CalCurves$Lithium.4, CalCurves$Lithium.5)


CalModels$Lithium.Plane.1 <- calibPlane(plane = CalCurves$Lithium.Plane.1)
CalModels$Lithium.Plane.2 <- calibPlane(plane = CalCurves$Lithium.Plane.2)
#dev.off()

#pdf("Perfiles19-05-21-corr.pdf", height = 5, width = 10)
#-----NUEVOS PERFILES DE TRANSPORTE----------------------------------------
#pdf("Perfiles19-05-21-planar.pdf", height = 5, width = 10)
for (i in 1:7) {
  eval(parse(text = paste0("AliConc$Strip.10.", i, ".pl.Li <- signal2conc(signal = AliAbs$Strip.10.", i, ".Li,
                              model = CalModels$Lithium.Plane.1, planar = TRUE,
                              Conc.S = fixSecondary(metalConc = AliConc$Strip.10.", i, ".Na,
                                                    time = AliTimes$T.10.", i, "[c(1, 3, 5, 7, 9, 11)],
                                                    compTime = AliTimes$T.10.", i, ", order = 2))")))
  eval(parse(text = paste0("TransFrac$M.10.", i, ".pl.Li <- conc2frac(feed = AliConc$Feed.10.", i, ".Li,
                              strip = AliConc$Strip.10.", i, ".pl.Li, time = AliTimes$T.10.", i, ")")))
  eval(parse(text = paste0("TransNLS$M.10.", i, ".pl <- transTrend(TransFrac$M.10.", i, ".pl.Li)")))
  eval(parse(text = paste0("transPlot(trans = TransFrac$M.10.", i, ".pl.Li, trend = TransNLS$M.10.", i, ".pl,
                              secondary = TransFrac$M.10.", i, ".Na, xlim = c(0, 25), ylim = c(-0.05, 1.1),
                              ybreaks = c(0, 0.25, 0.50, 0.75, 1, 1.1))")))
}
#dev.off()

#-----PRUEBAS T DE IGUALDAD DE MEDIAS--------------------------------------
if(TRUE){
x <- matrix(ncol = 7, nrow = 8)
y <- matrix(ncol = 12, nrow = 7)
for (i in 1:8) {
  x[i, 1:4] <- eval(parse(text = paste0("c(as.vector(coefficients(TransNLS$M.10.", i, "$feed)),
                                           as.vector(coefficients(TransNLS$M.10.", i, "$strip)))")))

    if (i != 8) {
    x[i, 5:6] <- eval(parse(text = paste0("as.vector(coefficients(TransNLS$M.10.", i, ".pl$strip))")))

    y[i, ]    <- eval(parse(text = paste0("c(summary(TransNLS$M.10.", i, "$feed)$coefficients[1, 1:2],
                                             summary(TransNLS$M.10.", i, "$strip)$coefficients[1, 1:2],
                                             summary(TransNLS$M.10.", i, ".pl$strip)$coefficients[1, 1:2],
                                             summary(TransNLS$M.10.", i, "$feed)$coefficients[2, 1:2],
                                             summary(TransNLS$M.10.", i, "$strip)$coefficients[2, 1:2],
                                             summary(TransNLS$M.10.", i, ".pl$strip)$coefficients[2, 1:2])")))
  }
  x[i, 7] <- eval(parse(text = paste0("TransFrac$M.10.", i, ".Na$Frac[length(TransFrac$M.10.", i, ".Na$Frac)]")))
}
x <- data.frame(x)
y <- data.frame(y)

colnames(x) <- c("A_f", "B_f", "A_s1", "B_s1", "A_s2", "B_s2", "X_Na")
colnames(y) <- c("A_f", "sA_f", "A_s1", "sA_s1", "A_s2", "sA_s2", "B_f", "sdB_f",  "B_s1", "sdB_s1", "B_s2", "sdB_s2")

# Curva normal Vs. Plano
## Cómo efectuar una prueba de datos pareados cuando las medias de los miembros
## de las parejas tienen su propia incertidumbre?
### Enfoque tradicional:
#### Alphas
t.test(x = x[1:7, 1], y = x[1:7, 3], paired = TRUE)
t.test(x = x[1:7, 1], y = x[1:7, 5], paired = TRUE)

t.test(x = x[1:7, 2], y = x[1:7, 4], paired = TRUE)
t.test(x = x[1:7, 2], y = x[1:7, 6], paired = TRUE)

## Pruebas t para dos medias muestrales en cada punto:
(y[, 1] - y[, 3])/sqrt(apply(cbind(y[, 2], y[, 4]), 1, mean)/18)
(y[, 1] - y[, 5])/sqrt(apply(cbind(y[, 2], y[, 6]), 1, mean)/18)
qt(0.975, 18)

(y[, 7] - y[, 9])/sqrt(apply(cbind(y[, 8], y[, 10]), 1, mean)/18)
(y[, 7] - y[, 11])/sqrt(apply(cbind(y[, 8], y[, 12]), 1, mean)/18)
qt(0.975, 18)

#Comparing relative diferences
a12 <- ((x[, 1] - x[, 3]) / x[, 1] * 100)[1:7]
a13 <- ((x[, 1] - x[, 5]) / x[, 1] * 100)[1:7]
b12 <- ((x[, 2] - x[, 4]) / x[, 2] * 100)[1:7]
b13 <- ((x[, 2] - x[, 6]) / x[, 2] * 100)[1:7]

t.test(x = abs(a12), y = abs(a13), alternative = 'greater', paired = TRUE)
t.test(x = abs(b12), y = abs(b13), alternative = 'greater', paired = TRUE)

#######
## Pruebas para plastificante y las variables de bloque
x$plast <- as.factor(c(1, 0, 1, 0, 1, 0, 1, 0))
x$celda <- as.factor(c(1, 2, 2, 1, 1, 2, 2, 1))
x$dia   <- as.factor(c(1, 1, 2, 2, 3, 3, 4, 4))


# Plastificante Vs no plastificante
t.test(X1 ~ plast, data = x[1:7, ])
t.test(X2 ~ plast, data = x[1:7, ])
t.test(X5 ~ plast, data = x[1:7, ])
t.test(X6 ~ plast, data = x[1:7, ])
t.test(X7 ~ plast, data = x[1:7, ])
x[which(x$plast == 1), ]
x[which(x$plast == 0), ]

# Celda
t.test(X1 ~ celda, data = x[1:7, ])
t.test(X2 ~ celda, data = x[1:7, ])
t.test(X5 ~ celda, data = x[1:7, ])
t.test(X6 ~ celda, data = x[1:7, ])
t.test(X7 ~ celda, data = x[1:7, ])
x[which(x$plast == 1), ]
x[which(x$plast == 0), ]


# día
summary(aov(X1 ~ dia, data = x[1:7, ]))
summary(aov(X2 ~ dia, data = x[1:7, ]))
summary(aov(X3 ~ dia, data = x[1:7, ]))
summary(aov(X4 ~ dia, data = x[1:7, ]))
summary(aov(X7 ~ dia, data = x[1:7, ]))

#día + celda + plastificante
summary(aov(X1 ~ dia + celda + plast, data = x[1:7, ]))
summary(aov(X2 ~ dia + celda + plast, data = x[1:7, ]))
summary(aov(X3 ~ dia + celda + plast, data = x[1:7, ]))
summary(aov(X4 ~ dia + celda + plast, data = x[1:7, ]))
summary(aov(X7 ~ dia + celda + plast, data = x[1:7, ]))
}

library(lmodel2)
data(mod2ex1)
mod2ex1
Ex1.res <- lmodel2(Predicted_by_model ~ Survival, data=mod2ex1, nperm=99)
Ex1.res

for (i in c(1, 5, 9, 3, 7, 11)) print(shapiro.test(y[, i]))
lmodel2(A_s1 ~ A_f, data = y)
plot(A_s1 ~ A_f, data = y)
lmodel2(A_s2 ~ A_f, data = y)
plot(A_s2 ~ A_f, data = y)
