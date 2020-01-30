library(ggplot2)
library(ggformula)
library(transmem)
PDF <- TRUE
if (PDF) pdf("Perfiles11-10-19p2.pdf", height = 7/1.8, width = 9/1.8)

#-----STOCK SOLUTIONS--------------------------------------------------------
{StockLi.200_2 <- 130.3 * 0.187872 * 0.99 / 0.1205105
StockNa.5000_1  <- 0.6399 * 0.996 / 50.0160 * 0.393372 * 1000000
StockK.10000_1  <- 0.9592 * 0.996 / 50.0812 * 0.52445 * 1000000
StockMg.10000_1  <- 1.7698 * 0.99 / 50.0892 * 0.28827 * 1000000

StockLi.5_7   <- StockLi.200_2 * 1.2673 / 50.0164
StockNa.200_3   <- 1.2006 *  StockNa.5000_1 / 30.2104
StockNa.20_3    <- 3.0048 *  StockNa.200_3 / 30.2730
StockNa.2_3     <- 3.0019 *  StockNa.20_3 / 29.5762
StockK.200_3    <- 0.5958 * StockK.10000_1 / 30.2276
StockK.20_3     <- 3.1167 * StockK.200_3 / 30.1430
StockK.2_3      <- 3.0613 * StockK.20_3 / 30.2162
StockMg.200_3   <- 0.6036 * StockMg.10000_1 / 30.4746
StockMg.40_3    <- 5.1314 * StockMg.200_3 / 20.5494
StockMg.4_3     <- 2.9958 * StockMg.40_3 / 30.0684}
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CalCurves <- list(
  Lithium.1 = data.frame(Conc = c(0.0000, 0.0597, 0.2989, 0.9115, 1.2053, 1.6816, 2.1670, 2.6474) *
                                StockLi.5_7 / c(6.0000, 6.1144, 6.0524, 6.0355, 6.1449, 6.0859, 6.0838, 6.0795),
                         Signal = c(0.000, 0.028, 0.142, 0.423, 0.539, 0.735, 0.916, 1.076)),
  Sodium.1 = data.frame(Conc = c(0.0000, 0.1566, 0.3131, 0.9215, 1.8151, 2.7010) * StockNa.2_3 /
                          c(6.0000, 6.0914, 6.1079, 6.1201, 6.0540, 6.0687),
                        Signal  = c(0, 0.072, 0.155, 0.451, 0.831, 1.165)),
  Sodium.2 = data.frame(Conc = c(0.0000, 0.2593, 0.7287, 1.4573, 1.9836, 2.7182) * StockNa.20_3 /
                          c(6.0000, 6.0433, 6.0563, 6.0386, 6.0264, 6.0257),
                        Signal  = c(0, 0.304, 0.646, 0.960, 1.113, 1.283)),
  Sodium.3 = data.frame(Conc = c(0.0000, 0.2642, 0.7164, 1.4557, 1.9928, 2.7089) * StockNa.200_3 /
                          c(6.0000, 6.0997, 6.0910, 6.0454, 6.1805, 6.1375),
                        Signal  = c(0, 0.468, 0.711, 0.958, 1.113, 1.246)),
  Potassium.1 = data.frame(Conc = c(0.0000, 0.3086, 0.9087, 2.1088, 3.6125) * StockK.2_3 /
                             c(6.0000, 6.1873, 6.0336, 6.0505, 6.0061),
                           Signal = c(0.000, 0.051, 0.237, 0.579, 0.975)),
  Potassium.2 = data.frame(Conc = c(0.0000, 0.3620, 1.0863, 2.0795, 3.0906, 3.6189) * StockK.20_3 /
                             c(6.0000, 6.1058, 6.0637, 6.0433, 6.0621, 6.0495),
                           Signal = c(0.000, 0.194, 0.520, 0.826, 1.051, 1.154)),
  Potassium.3 = data.frame(Conc = c(0.0000, 0.3819, 1.0238, 2.0579, 3.0836, 3.6174) * StockK.200_3 /
                             c(6.0000, 6.1784, 6.0885, 6.2225, 6.0750, 6.1471),
                           Signal = c(0.000, 0.351, 0.618, 0.881, 1.107, 1.196)),
  Magnessium.1 = data.frame(Conc = c(0.0000, 0.1692, 0.3373, 0.9324, 1.8073, 2.7109) * StockMg.4_3 /
                              c(6.0000, 6.0576, 6.1379, 6.1271, 6.0632, 6.1367),
                            Signal = c(0.000, 0.058, 0.114, 0.305, 0.555, 0.758)),
  Magnessium.2 = data.frame(Conc = c(0.0000, 0.2776, 0.7472, 1.4814, 2.1161, 2.7144) * StockMg.40_3 /
                              c(6.0000, 6.2208, 6.0171, 6.2113, 6.0664, 6.0479),
                            Signal = c(0.000, 0.027, 0.050, 0.067, 0.093, 0.107)),
  Magnessium.3 = data.frame(Conc = c(0.0000, 0.2559, 0.7527, 1.4119, 2.0479) * StockMg.200_3 /
                              c(6.0000, 6.1802, 6.0877, 6.2545, 6.0042),
                            Signal = c(0.000, 0.040, 0.103, 0.144, 0.148))
)
## for a cleaner workspace
rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
Order <- c(2, 2, 2, 2, 1, 2, 2, 2, 2, 2)
CalModels <- list()
for (i in 1:10) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = Order[i])
names(CalModels) <- names(CalCurves)
anova(CalModels$Lithium.1)
summary(CalModels$Lithium.1)
#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- list (
  T.17.8a  = c(0, 1, 2, 3, 4, 5),
  T.17.8b  = c(0, 1, 2, 3, 4.25, 5),
  T.17.9a  = c(0, 1, 2, 3, 4, 5),
  T.17.9b  = c(0, 1, 2, 3, 4, 5),
  T.17.10a = c(0, 1, 2, 3, 4.25, 5),
  T.17.10b = c(0, 1, 2, 3, 4, 5)
)
ts <- c(1, 3, 6)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
dilutions <- list(
  Feed.17.8a   = c(18708/1369, 18813/1366, 18735/1346),
  Strip.17.8a  = c(12600/2217, 12702/2312, 12614/2262*5.308),
  Feed.17.8b   = c(18143/0925, 18553/1366, 18742/1358),
  Strip.17.8b  = c(12549/2270, 12645/2261, 12605/2246*5.173),
  Feed.17.9a   = c(18591/1367, 18734/1352, 18793/1361),
  Strip.17.9a  = c(12731/2253, 12414/2256, 12408/2252),
  Feed.17.9b   = c(18339/1376, 18358/1367, 18369/1355),
  Strip.17.9b  = c(12317/2241, 12399/2256, 12374/2224),
  Feed.17.10a  = c(18448/1353, 18482/1363, 18559/1353),
  Strip.17.10a = c(12583/2243, 12560/2180, 12664/2239),
  Feed.17.10b  = c(18498/1351, 18570/1354, 16470/1364),
  Strip.17.10b = c(12680/2232, 12568/2252, 12624/2241)
)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(# M2 is Na, K and Mg for Mem.17.2, 17.3 and 17.4, respectively.,
  Feed.17.8.Li.a   = c(1.058, 0.933, 0.865, 0.817, 0.773, 0.743),
  Strip.17.8.Li.a  = c(0.003, 0.155, 0.248, 0.313, 0.366, 0.443),
  Feed.17.8.M2.a   = c(0.989, 0.967, 0.950),
  Strip.17.8.M2.a  = c(0.056, 0.787, 0.393),#Last measure made at mediumconcentration level... multiply by 5.308
  Feed.17.8.Li.b   = c(1.089, 0.889, 0.813, 0.752, 0.704, 0.667),
  Strip.17.8.Li.b  = c(0.009, 0.192, 0.302, 0.381, 0.437, 0.487),
  Feed.17.8.M2.b   = c(0.802, 0.973, 0.963),
  Strip.17.8.M2.b  = c(0.037, 0.545, 0.349),#Last measure made at mediumconcentration level... multiply by 5.173
  Feed.17.9.Li.a   = c(1.036, 0.866, 0.792, 0.724, 0.618, 0.646),
  Strip.17.9.Li.a  = c(0.016, 0.207, 0.322, 0.403, 0.464, 0.506),
  Feed.17.9.M2.a   = c(0.995, 0.980, 0.979),
  Strip.17.9.M2.a  = c(0.071, 0.264, 0.495),
  Feed.17.9.Li.b   = c(1.034, 0.817, 0.671, 0.559, 0.492, 0.437),
  Strip.17.9.Li.b  = c(0.007, 0.275, 0.457, 0.581, 0.645, 0.697),
  Feed.17.9.M2.b   = c(1.000, 0.999, 0.988),
  Strip.17.9.M2.b  = c(0.139, 0.428, 0.755),
  Feed.17.10.Li.a   = c(0.950, 0.944, 0.931, 0.929, 0.920, 0.921),
  Strip.17.10.Li.a  = c(0.009, 0.023, 0.038, 0.053, 0.062, 0.071),
  Feed.17.10.M2.a   = c(0.152, 0.148, 0.134),
  Strip.17.10.M2.a  = c(0.002, 0.052, 0.098),
  Feed.17.10.Li.b   = c(0.951, 0.907, 0.875, 0.851, 0.841, 0.831),
  Strip.17.10.Li.b  = c(0.011, 0.086, 0.139, 0.175, 0.200, 0.212),
  Feed.17.10.M2.b   = c(0.144, 0.137, 0.128),
  Strip.17.10.M2.b  = c(0.001, 0.084, 0.113)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- vector(mode = "list", length = length(AliAbs))
names(AliConc) <- names(AliAbs)
for (i in c(1, 2, 5, 6, 9, 10, 13, 14, 17, 18, 21, 22)) {
  AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Lithium.1)
}
for (i in c(3, 7)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Sodium.3,
                                               dilution = dilutions[[(i %/% 2)]])
for (i in c(4, 8)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Sodium.1,
                                               dilution = dilutions[[(i %/% 2)]])
for (i in c(11, 15)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Potassium.3,
                                                 dilution = dilutions[[(i %/% 2)]])
for (i in c(12, 16)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Potassium.1,
                                                 dilution = dilutions[[(i %/% 2)]])
for (i in c(19, 23)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Magnessium.3,
                                                 dilution = dilutions[[(i %/% 2)]])
for (i in c(20, 24)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Magnessium.2,
                                                 dilution = dilutions[[(i %/% 2)]])
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- vector(mode = "list", length = length(AliConc)/2)
names(TransFrac) <- paste0(rep(c("Lithium.", "Secondary."), length(TransFrac)/2),
                           paste0(rep(8:10, each = 4), rep(c('a', 'b'), each = 2)))
for (i in 1:(length(TransFrac)/2)) {
  #Lithium
  TransFrac[[i*2-1]] <- conc2frac(feed = AliConc[[4*i-3]], strip = AliConc[[4*i-2]], time = AliTimes[[i]],
                                  correct.strip = TRUE)
  #other metals
  TransFrac[[i*2]]   <- conc2frac(feed = AliConc[[4*i-1]], strip = AliConc[[4*i]], time = AliTimes[[i]][ts],
                                  correct.strip = TRUE)
}
#-----MODELOS DE REGRESIÓN NO LINEAL-----------------------------------------
TransNLS  <- vector(mode = "list", length = length(TransFrac)/2)
names(TransNLS) <- names(TransFrac)[seq(from = 1, to = length(TransFrac), by = 2)]
SS_par <- vector()
for (i in 1:length(TransNLS)) {
  TransNLS[[i]] <- transTrend(TransFrac[[2*i-1]], model = 'paredes', eccen = 1)
  SS_par <- c(SS_par, sum(resid(TransNLS[[i]]$feed)^2), sum(resid(TransNLS[[i]]$strip)^2))
}

TransNLSXot  <- vector(mode = "list", length = length(TransFrac)/2)
names(TransNLSXot) <- names(TransFrac)[seq(from = 1, to = length(TransFrac), by = 2)]
SS_xot <- vector()
for (i in 1:length(TransNLSXot)) {
  TransNLSXot[[i]] <- transTrend(TransFrac[[2*i-1]], model = 'rodriguez')
  SS_xot <- c(SS_xot, sum(resid(TransNLSXot[[i]]$feed)^2), sum(resid(TransNLSXot[[i]]$strip)^2))
}

t.test(x = SS_par, y = SS_xot, paired = TRUE)
plot(SS_par, SS_xot)
abline(lm(SS_xot~SS_par))
lm(SS_xot~SS_par)
#-----FACTORES DE SEPARACIÓN-------------------------------------------------
sepFactor <- vector(mode = "list", length = length(TransFrac)/2)
names(sepFactor) <- names(TransNLS)
for (i in 1:length(sepFactor)) {
  sec <- fixSecondary(metalConc = AliConc[[4*i]], time = AliTimes[[i]][ts], compTime = AliTimes[[i]], order = 2)
  X <- data.frame(time = AliTimes[[i]],
                  factor = (AliConc[[i*4-2]]/sec) / (AliConc[[i*4-3]][1]/AliConc[[i*4-1]][1]))
  #X$factor[1] <- 1
  X <- X[-1, ]
  sepFactor[[i]] <- X
}

ssepFactor <- data.frame()
for (i in 1:length(sepFactor)) ssepFactor <- rbind(ssepFactor, sepFactor[[i]])

ssepFactor$Membrana <- as.factor(paste0("Mem.", rep(c("1", "2", "3", "4", "5", "6"),
                                                    each = 5)))
ssepFactor$System <- as.factor(rep(c("Sodio", "Potasio", "Magnesio"), each = 10))
print(ggplot(data = ssepFactor, aes(x = time, y = factor, colour = System)) + geom_point() + theme_bw() +
  ggsci::scale_color_npg() + stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE,
                                         size = 0.4, aes(group = Membrana, colour = System)) +
  xlab(label = "Tiempo (horas)") + ylab(label = "Factor de separación"))

sF <- vector()
for (i in 1:length(sepFactor)) sF <- c(sF, mean(sepFactor[[i]][, 2]))

mean(c(sepFactor[[1]][5, 2], sepFactor[[2]][5, 2]))
sd(c(sepFactor[[1]][5, 2], sepFactor[[2]][5, 2]))
mean(c(sepFactor[[3]][5, 2], sepFactor[[4]][5, 2]))
sd(c(sepFactor[[3]][5, 2], sepFactor[[4]][5, 2]))
mean(c(sepFactor[[5]][5, 2], sepFactor[[6]][5, 2]))
sd(c(sepFactor[[5]][5, 2], sepFactor[[6]][5, 2]))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
#-----PERFILES DE TRANSPORTE ------------------------------------------------
pdf("Selectivity1-3.pdf", height = 75/25.4, width = 75/25.4)
for (i in 1:3) {
  p <- transPlotWR(trans = list(TransFrac[[4*i-3]], TransFrac[[4*i-1]]),
                   secondary = list(TransFrac[[4*i-2]], TransFrac[[4*i]]),
                   sec.trend = 'loess', span = 0.67, xlim = c(0, 5.2), ylim = c(-0.06, 1.08),
                   ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:5, plot = FALSE, size = 2.3,
                   bw = TRUE, srs = 0.55)
  p <- p + theme(text = element_text(size = 9))
  print(p)
}
dev.off()
# invisible(readline(prompt="Press [enter] to continue"))
#-----PARÁMETROS DE DESEMPEÑO------------------------------------------------
Parameters <- data.frame()
j = 0
for (i in 1:6) {
  Parameters <- rbind(Parameters, c(TransNLS[[i]]$Result, sF[i], TransFrac[[2*i-1]][12, 3]))
}

colnames(Parameters) <- c(names(TransNLS[[1]]$Result), "sF")
round(Parameters, 3)
if (PDF) dev.off()
