library(transmem)
PDF <- FALSE
if (PDF) pdf("SSS-19-11-15.pdf", height = 7/1.3, width = 9/1.3)

#-----STOCK SOLUTIONS--------------------------------------------------------
StockLi.200_2 <- 130.3 * 0.187872 * 0.99 / 0.1205105
StockLi.05_1   <- StockLi.200_2 * 0.15847 / 60.15202 * 1000 #Concentrations in ug kg^{-1}

StockNa.5000_1  <- 0.6399 * 0.996 / 50.0160 * 0.393372 * 1000000
StockK.10000_1  <- 0.9592 * 0.996 / 50.0812 * 0.52445 * 1000000
StockNa.20_3   <- 1.2006 *  StockNa.5000_1 / 30.2104 * 3.0048 / 30.2730
StockK.2_3    <- 0.5958 * StockK.10000_1 / 30.2276 * 3.1167 / 30.1430 * 3.0613 / 30.2162
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CalCurves <- list(
  Lithium.1 = data.frame(Conc = c(0.00000, 0.06908, 0.13992, 0.31141, 0.61770, 1.20980, 2.45930, 3.10441) *
                                StockLi.05_1 / c(6.0000, 6.2395, 5.9189, 6.4980, 6.3358, 6.1472, 6.1844, 6.0843),
                         Signal = c(0.000, 0.015, 0.033, 0.065, 0.131, 0.269, 0.536, 0.679)),
  Sodium.1 = data.frame(Conc = c(0.0000, 0.2593, 0.7287, 1.4573, 1.9836, 2.7182) * StockNa.20_3 /
                          c(6.0000, 6.0433, 6.0563, 6.0386, 6.0264, 6.0257),
                        Signal  = c(0, 0.318, 0.643, 0.941, 1.126, 1.315)),
  Potassium.1 = data.frame(Conc = c(0.0000, 0.3086, 0.9087, 2.1088, 3.6125) * StockK.2_3 /
                             c(6.0000, 6.1873, 6.0336, 6.0505, 6.0061),
                           Signal = c(0.000, 0.051, 0.183, 0.501, 0.866))
)
## for a cleaner workspace
#rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
CalModels <- list()
order = c(1, 2, 1)
for (i in 1:3) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = order[i], plot = TRUE)
names(CalModels) <- names(CalCurves)
summary(CalModels$Lithium.1)
#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- c(0, 0.75, 1.5, 2.25, 3, 3.75, 4.5)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliEm <- list(
  Li.Feed.A = c(0.395, 0.259, 0.188, 0.141, 0.110, 0.095, 0.084),
  Li.Strip.A = c(0.000, 0.178, 0.284, 0.339, 0.384, 0.412, 0.440),
  Li.Feed.B = c(0.400, 0.276, 0.171, 0.118, 0.079, 0.064, 0.056),
  Li.Strip.B = c(0.008, 0.183, 0.323, 0.391, 0.438, 0.467, 0.480),
  Li.Feed.A.sa = c(0.465, 0.340, 0.310, 0.276, 0.243, 0.231, 0.222),
  Li.Strip.A.sa = c(0.110, 0.290, 0.375, 0.434, 0.474, 0.501, 0.525),
  Li.Feed.B.sa = c(0.473, 0.358, 0.261, 0.200, 0.166, 0.151, 0.142),
  Li.Strip.B.sa = c(0.133, 0.296, 0.423, 0.488, 0.527, 0.554, 0.572),
  #Sodium
  Na.Feed.A = c(0.380, 0.366, 0.371, 0.361),
  Na.Strip.A = c(0.000, 0.266, 0.419, 0.520),
  Na.Feed.B = c(0.390, 0.369, 0.362, 0.352),
  Na.Strip.B = c(0.000, 0.361, 0.538, 0.701),
  #Potassium
  K.Feed.A = c(0.480, 0.485, 0.503, 0.449),
  K.Strip.A = c(0.000, 0.082, 0.174, 0.214),
  K.Feed.B = c(0.415, 0.459, 0.511, 0.518),
  K.Strip.B = c(0.000, 0.118, 0.149, 0.290)
)
#-----DILUCIÓN DE LAS MUESTRAS-----------------------------------------------
Dilutions <- list(
  Feed.A.1.20 = c(20289/1276, 20354/1285, 19991/1286, 20167/1279),
  Strip.A.1.20 = c(20426/1254, 20192/1259, 20261/1239, 20401/1291),
  Feed.B.1.20 = c(20473/1284, 20420/1282, 20344/1216, 20349/1236),
  Strip.B.1.20 = c(20415/1264, 20427/1251, 20366/1260, 20375/1258)
)
Dilutions$Strip.A.1.400 <- Dilutions$Strip.A.1.20 * c(19846/1224, 19817/1235, 20094/1254, 20102/1246)
Dilutions$Strip.B.1.400 <- Dilutions$Strip.B.1.20 * c(20164/1245, 19878/1254, 20110/1255, 20138/1256)

Dilutions$Feed.A.1.1000 <- Dilutions$Feed.A.1.20 * c(50933/1255, 50838/1242, 50914/1255, 51235/1260)
Dilutions$Feed.B.1.1000 <- Dilutions$Feed.B.1.20 * c(50931/1260, 50992/1269, 50812/1258, 51026/1258)

Dilutions$Feed.A.1.20000 <- Dilutions$Feed.A.1.1000 * c(20281/1263, 20208/1273, 20275/1277, 20301/1256)
Dilutions$Feed.B.1.20000 <- Dilutions$Feed.B.1.1000 * c(20309/1266, 20333/1264, 20281/1256, 20180/1259)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- vector(mode = "list", length = length(AliEm))
Dil <- c(rep(NA, 8), 9, 5, 10, 6, 7, 2, 8, 4)
for (i in 1:4)  AliConc[[i]] <- signal2conc(signal = AliEm[[i]], model = CalModels$Lithium.1)
for (i in 9:12) AliConc[[i]] <- signal2conc(signal = AliEm[[i]], model = CalModels$Sodium.1,
                                            dilution = Dilutions[[Dil[i]]])
for (i in 13:16) AliConc[[i]] <- signal2conc(signal = AliEm[[i]], model = CalModels$Potassium.1,
                                             dilution = Dilutions[[Dil[i]]])
AliConc[[13]] <- AliConc[[13]][1] - AliConc[[14]]
AliConc[[15]] <- AliConc[[15]][1] - AliConc[[16]]
#-----CONCENTRACIÓN POR ADICION ESTÁNDAR DE UN SOLO PUNTO--------------------
FinalMass <- list(feed.A = c(7027, 7030, 6850, 6979, 6943, 6868, 6854),
                  strip.A = c(6856, 6814, 6870, 6839, 6904, 6852, 6866),
                  feed.B = c(7029, 6991, 7023, 7023, 6997, 7004, 7031),
                  strip.B = c(6848, 6879, 6890, 6910, 6841, 6814, 6952))
SpikeMass <- list(feed.A = c(592, 601, 605, 600, 595, 593, 601),
                  strip.A = c(593, 595, 571, 595, 598, 599, 596),
                  feed.B = c(599, 594, 596, 603, 600, 598, 596),
                  strip.B = c(593, 598, 601, 579, 583, 565, 636))
InitiMass <- list(feed.A = FinalMass[[1]] - SpikeMass[[1]], strip.A = FinalMass[[2]] - SpikeMass[[2]],
                  feed.B = FinalMass[[3]] - SpikeMass[[3]], strip.B = FinalMass[[4]] - SpikeMass[[4]])
for (i in 1:4) {
  AliConc[[i+4]] <- (AliEm[[i]] * StockLi.05_1 * (SpikeMass[[i]] / FinalMass[[i]])) /
    (AliEm[[4+i]] - AliEm[[i]] * (InitiMass[[i]] / FinalMass[[i]]))
}
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- vector(mode = "list", length = length(AliConc) / 2)
for (i in 1:8) {
  if (i %in% 1:4) AliTimes1 = AliTimes else AliTimes1 = AliTimes[c(1, 3, 5, 7)]
  TransFrac[[i]] <- conc2frac(feed = AliConc[[2 * i - 1]], strip = AliConc[[2 * i]],
                              time = AliTimes1)
}
#-----MODELOS DE REGRESIÓN NO LINEAL-----------------------------------------
TransNLSXot <- TransNLS <- vector(mode = "list", length = 2)
#names(TransNLSXot) <- names(TransNLS) <- names(TransFrac)
SS_xot <- SS_par <- vector()
for (i in 3:4) {
  TransNLS[[i]] <- X <- transTrend(TransFrac[[i]], model = 'paredes', eccen = 1.5)
  SS_par <- c(SS_par, sum(resid(X$feed)^2), sum(resid(X$strip)^2))

  TransNLSXot[[i]] <- X <- transTrend(TransFrac[[i]], model = 'rodriguez')
  SS_xot <- c(SS_xot, sum(resid(X$feed)^2), sum(resid(X$strip)^2))
}
t.test(x = SS_par, y = SS_xot, paired = TRUE)
#-----PERFILES DE TRANSPORTE ------------------------------------------------
Parameters <- data.frame()

for (i in 1:2) {
  transPlot(trans = TransFrac[[2+i]], trend = TransNLS[[2+i]], xlim = c(0, 4.5), ylim = c(-0.05, 1.1),
            ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:5, secondary = TransFrac[[4+i]])
  transPlot(trans = TransFrac[[2+i]], trend = TransNLS[[2+i]], xlim = c(0, 4.5), ylim = c(-0.05, 1.1),
            ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:5, secondary = TransFrac[[6+i]])
  Parameters <- rbind(Parameters, TransNLS[[2+i]]$Result)
}

colnames(Parameters) <- names(TransNLS[[1]]$Result)
round(Parameters, 3)

trans1 = list(TransFrac[[3]], TransFrac[[4]])
trend1 = list(TransNLS[[3]], TransNLS[[4]])

# Not so appropiate plot
transPlotWR(trans = list(TransFrac[[3]], TransFrac[[4]]), trend = list(TransNLS[[3]], TransNLS[[4]]),
            secondary = list(TransFrac[[5]], TransFrac[[6]]))
transPlotWR(trans = list(TransFrac[[3]], TransFrac[[4]]), trend = list(TransNLS[[3]], TransNLS[[4]]),
            secondary = list(TransFrac[[7]], TransFrac[[8]]))

p <- transPlotWR(trans = list(TransFrac[[3]], TransFrac[[4]]), trend = list(TransNLS[[3]], TransNLS[[4]]),
                 secondary = list(TransFrac[[5]], TransFrac[[6]]),
                 tertiary = list(TransFrac[[7]], TransFrac[[8]]), size = 2, plot = FALSE,
                 bw = FALSE, srs = 0.55, explicit = TRUE)
p <- p + theme(text = element_text(size = 9))
#pdf("SSSProfiles.pdf", height = 70/25.4, width = 90/25.4)
p
#dev.off()



#-----FACTORES DE SEPARACIÓN-------------------------------------------------
sepFactor <- vector(mode = "list", length = length(TransFrac)/2)
names(sepFactor) <- names(TransNLS)
ts <- c(1, 3, 5, 7)
for (i in 1:length(sepFactor)) {
  if (i %in% c(1, 3)) {
    oriFEED <- AliConc[[5]]
    oriSTRIP <- AliConc[[6]]
  } else {
    oriFEED <- AliConc[[7]]
    oriSTRIP <- AliConc[[8]]
  }
  sec <- fixSecondary(conc = AliConc[[8+2*i]], time = AliTimes[ts], compTime = AliTimes, order = 1)
  X <- data.frame(time = AliTimes,
                  factor = (oriSTRIP/sec) / (oriFEED/AliConc[[7+2*i]][1]))
  #X$factor[1] <- 1
  X[1, 2] <- 1
  sepFactor[[i]] <- X
}

ssepFactor <- data.frame()
for (i in 1:length(sepFactor)) ssepFactor <- rbind(ssepFactor, sepFactor[[i]])

ssepFactor$Membrana <- as.factor(rep(c("A", "B", "C", "D"), each = 7))
ssepFactor$System <- as.factor(rep(c("Sodio", "Potasio"), each = 14))
print(ggplot(data = ssepFactor, aes(x = time, y = factor, colour = System)) + geom_point() + theme_bw() +
        ggsci::scale_color_npg() + stat_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE,
                                               size = 0.4, aes(group = Membrana, colour = System)) +
        xlab(label = "Tiempo (horas)") + ylab(label = "Factor de separación"))

sF <- vector()
for (i in 1:length(sepFactor)) sF <- c(sF, mean(sepFactor[[i]][, 2]))



if (PDF) dev.off()
