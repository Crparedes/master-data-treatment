library(transmem)
library(ggplot2)
PDF <- TRUE
if (PDF) pdf("SWN-SeveralpOH-27-01-19.pdf", height = 7/1.3, width = 9/1.3)

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
                         Signal = c(0, 0.008, 0.041, 0.209, 0.417, 0.537)),
  LithiumSW = data.frame(Conc = c(0, 0.0709, 0.2340, 1.2107, 2.3974, 3.2936) * StockLi.500u /
                           c(6, 6.0722, 6.0480, 6.0575, 6.0114, 6.0348),
                         Signal = c(0, 0.010, 0.038, 0.206, 0.345, 0.567)),
  Sodium    = data.frame(Conc = c(0, 0.6103, 1.1981, 1.7972, 2.3932, 3.0180) * StockNa.20 /
                           c(6, 6.0091, 6.0703, 6.0552, 6.0101, 6.0426),
                         Signal  = c(0, 0.402, 0.618, 0.765, 0.882, 0.981)),
  Potassium = data.frame(Conc = c(0, 0.1161, 0.3845, 0.7267, 0.9607, 1.2154) * StockK.10 /
                           c(6, 6.0537, 6.0643, 6.0318, 6.0002, 6.0493),
                         Signal = c(0, 0.130, 0.400, 0.704, 0.900, 1.074))
)
## for a cleaner workspace
#rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
CalModels <- list()
order = c(1, 1, 2, 2)
for (i in 1:4) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = order[i], plot = TRUE)
names(CalModels) <- names(CalCurves)
summary(CalModels$Lithium)
summary(CalModels$LithiumSW)
#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- c(0, 1, 3, 4)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(
  Feed.24.1.Li  = c(0.328, 0.180, 0.035, 0.032),
  Feed.24.2.Li  = c(0.306, 0.256, 0.168, 0.118),
  Feed.24.3.Li  = c(0.310, 0.150, 0.067, 0.051),
  Feed.24.4.Li  = c(0.301, 0.296, 0.269, 0.261),
  Strip.24.1.Li = c(0.006, 0.180, 0.360, 0.389),
  Strip.24.2.Li = c(0.000, 0.056, 0.181, 0.249),
  Strip.24.3.Li = c(0.000, 0.200, 0.309, 0.327),
  Strip.24.4.Li = c(0.000, 0.008, 0.017, 0.022),
  Feed.24.1.Na  = c(0.884, 0.878, 0.875, 0.858),
  Feed.24.2.Na  = c(0.905, 0.888, 0.887, 0.879),
  Feed.24.3.Na  = c(0.899, 0.902, 0.901, 0.883),
  Feed.24.4.Na  = c(0.918, 0.914, 0.925, 0.918),
  Strip.24.1.Na = c(0.008, 0.420, 0.755, 0.596),
  Strip.24.2.Na = c(0.034, 0.456, 0.662, 0.540),
  Strip.24.3.Na = c(0.049, 0.536, 0.676, 0.735),
  Strip.24.4.Na = c(0.312, 0.101, 0.171, 0.274),
  Feed.24.1.K   = c(0.578, 0.544, 0.549, 0.524),
  Feed.24.2.K   = c(0.568, 0.536, 0.538, 0.525),
  Feed.24.3.K   = c(0.534, 0.521, 0.534, 0.528),
  Feed.24.4.K   = c(0.540, 0.536, 0.550, 0.536),
  Strip.24.1.K  = c(0.020, 0.088, 0.290, 0.358),
  Strip.24.2.K  = c(0.026, 0.120, 0.139, 0.095),
  Strip.24.3.K  = c(0.025, 0.054, 0.118, 0.160),
  Strip.24.4.K  = c(0.026, 0.029, 0.032, 0.036)
)
#-----DILUCIÓN DE LAS MUESTRAS PARA SODIO Y POTASIO--------------------------
AliDil <- list(
  Feed.24.1.1_20  = c(1052/19710, 1054/19916, 1052/19946, 1063/20370)^(-1),
  Feed.24.2.1_20  = c(1084/19896, 1045/19903, 1045/19908, 1043/20179)^(-1),
  Feed.24.3.1_20  = c(1044/19887, 1060/19965, 1048/19835, 1047/19902)^(-1),
  Feed.24.4.1_20  = c(1049/19927, 1044/19882, 1042/19820, 1044/19919)^(-1)
)
AliDil$Feed.24.1.1_K <- AliDil$Feed.24.1.1_20 * c(1016/19810, 1019/19492, 1015/19414, 1019/19961)^(-1)
AliDil$Feed.24.2.1_K <- AliDil$Feed.24.2.1_20 * c(1022/19943, 1018/19882, 1021/19871, 1028/19907)^(-1)
AliDil$Feed.24.3.1_K <- AliDil$Feed.24.3.1_20 * c(1026/19763, 1022/20571, 1025/19932, 1025/20002)^(-1)
AliDil$Feed.24.4.1_K <- AliDil$Feed.24.4.1_20 * c(1027/19869, 1028/19883, 1033/19868, 1020/19821)^(-1)

AliDil$Strip.24.1.1_K = c(1002/10125, 1020/10265, 1026/10262, 1027/10239)^(-1)
AliDil$Strip.24.2.1_K = c(1024/10230, 1022/10291, 1018/10113, 1024/10265)^(-1)
AliDil$Strip.24.3.1_K = c(1017/10212, 1023/10215, 1023/10242, 1018/10225)^(-1)
AliDil$Strip.24.4.1_K = c(1025/10282, 1018/10240, 1023/10261, 1022/10217)^(-1)

AliDil$Feed.24.1.1_Na <- AliDil$Feed.24.1.1_K * c(3206/15279, 3202/15348, 3191/15329, 3197/15329)^(-1)
AliDil$Feed.24.2.1_Na <- AliDil$Feed.24.2.1_K * c(3197/15292, 3212/15293, 3223/15298, 3200/15306)^(-1)
AliDil$Feed.24.3.1_Na <- AliDil$Feed.24.3.1_K * c(3208/15228, 3217/15277, 3221/15289, 3227/15216)^(-1)
AliDil$Feed.24.4.1_Na <- AliDil$Feed.24.4.1_K * c(3225/15310, 3209/15284, 3203/15303, 3199/15257)^(-1)

AliDil$Strip.24.1.1_Na <- AliDil$Strip.24.1.1_K * c(1124/10089, 1009/10089, 1100/12576, 1008/20018)^(-1)
AliDil$Strip.24.2.1_Na <- AliDil$Strip.24.2.1_K * c(3321/09866, 1466/13367, 2800/14800, 1455/13476)^(-1)
AliDil$Strip.24.3.1_Na <- AliDil$Strip.24.3.1_K * c(2214/10228, 2223/10256, 2230/10250, 2234/10340)^(-1)
AliDil$Strip.24.4.1_Na <- AliDil$Strip.24.4.1_K * c(3188/10220, 3202/10184, 3213/10238, 3201/10203)^(-1)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- vector(mode = "list", length = length(AliAbs))
names(AliConc) <- names(AliAbs)
for (i in 1:4) {
  AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$LithiumSW)
}
for (i in 5:8) {
  AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Lithium)
}
for (i in 9:16) {
  AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], dilution = AliDil[[(i+4)]], model = CalModels$Sodium)
}
for (i in 17:24) {
  AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], dilution = AliDil[[(i-12)]], model = CalModels$Potassium)
}
#-----CONCENTRACIÓN POR ADICION ESTÁNDAR DE UN SOLO PUNTO--------------------
AliConc1 <- AliConc
FinalMass <- list(Strip.24.1 = c(6728, 6646, 6638, 6632),
                  Strip.24.2 = c(6590, 6580, 6591, 6577),
                  Strip.24.3 = c(6507, 6572, 6582, 6627),
                  Strip.24.4 = c(6484, 6588, 6556, 6557))
SpikeMass <- list(Strip.24.1 = c(565, 565, 562, 562),
                  Strip.24.2 = c(568, 565, 565, 567),
                  Strip.24.3 = c(567, 566, 566, 565),
                  Strip.24.4 = c(568, 567, 567, 564))
AliAbsSA <- list(Strip.24.1SA = c(0.102, 0.272, 0.437, 0.460),
                 Strip.24.2SA = c(0.118, 0.161, 0.275, 0.337),
                 Strip.24.3SA = c(0.099, 0.290, 0.385, 0.408),
                 Strip.24.4SA = c(0.117, 0.110, 0.119, 0.123))
for (i in 1:4) {
  AliConc[[i+4]] <- (AliAbs[[i+4]] * StockLi.500u * (SpikeMass[[i]] / FinalMass[[i]])) /
    (AliAbsSA[[i]] - AliAbs[[i+4]] * ((FinalMass[[i]] - SpikeMass[[i]]) / FinalMass[[i]]))
}
#Is really Single Point Standar Addition better than CCES?? Yes, absolutly
AliConc[[1]][1] - AliConc[[1]]; AliConc[[5]]; AliConc1[[5]]
AliConc[[2]][1] - AliConc[[2]]; AliConc[[6]]; AliConc1[[6]]
AliConc[[3]][1] - AliConc[[3]]; AliConc[[7]]; AliConc1[[7]]
AliConc[[4]][1] - AliConc[[4]]; AliConc[[8]]; AliConc1[[8]]

#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- vector(mode = "list", length = length(AliConc) / 2)
for (i in 1:4) {
  TransFrac[[i]] <- conc2frac(feed = AliConc[[i]], strip = AliConc[[4+i]],
                              time = AliTimes)
}
for (i in 5:8) {
  TransFrac[[i]] <- conc2frac(feed = AliConc[[4+i]], strip = AliConc[[8+i]],
                              time = AliTimes)
}
for (i in 9:12) {
  TransFrac[[i]] <- conc2frac(feed = AliConc[[8+i]], strip = AliConc[[12+i]],
                              time = AliTimes)
}
#-----MODELOS DE REGRESIÓN NO LINEAL-----------------------------------------
TransNLS <- vector(mode = "list", length = 4)
for (i in 1:4) {
  TransNLS[[i]] <- transTrend(TransFrac[[i]], model = 'paredes', eccen = 1.5)
}
#-----PERFILES DE TRANSPORTE ------------------------------------------------
for (i in 1:4) {
  transPlot(trans = TransFrac[[i]], trend = TransNLS[[i]], xlim = c(0, 4.5), ylim = c(-0.05, 1.1),
            ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:5, secondary = TransFrac[[4+i]],
            ternary = TransFrac[[8+i]], bw = TRUE, srs = 0.6, sec.trend = 'loess')
}

if (PDF) dev.off()
