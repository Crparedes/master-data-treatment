library(ggplot2)
library(ggformula)
library(transmem)
PDF <- FALSE
if (PDF) pdf("Perfiles11-10-19p1.pdf", height = 7/1.8, width = 9/1.8)

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
                         Signal = c(0.000, 0.031, 0.160, 0.476, 0.611, 0.832, 1.042, 1.230)),
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
  T.17.5a = c(0, 1, 2, 3, 4, 5),
  T.17.5b = c(0, 1, 2, 3, 4.25, 5),
  T.17.6a = c(0, 1, 2, 3, 4, 5),
  T.17.6b = c(0, 1, 2, 3, 4, 5),
  T.17.7a = c(0, 1, 2, 3, 4.25, 5),
  T.17.7b = c(0, 1, 2, 3, 4, 5)
)
ts <- c(1, 3, 6)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
dilutions <- list(
  Feed.17.5a  = c(18669/1371, 18713/1369, 18727/1375),
  Strip.17.5a = c(12751/2252, 12595/2250, 12415/2253),
  Feed.17.5b  = c(18737/1370, 18730/1364, 18779/1373),
  Strip.17.5b = c(12568/2204, 12566/2172, 12636/2225),
  Feed.17.6a  = c(18698/1361, 18732/1374, 18738/1363),
  Strip.17.6a = c(12368/2252, 12364/2254, 12329/2244),
  Feed.17.6b  = c(18572/1367, 18637/1361, 18618/1364),
  Strip.17.6b = c(12337/2253, 12345/2241, 12379/2259),
  Feed.17.7a  = c(18606/1351, 18307/1362, 20273/3137),
  Strip.17.7a = c(12341/2149, 12692/2260, 12673/2253),
  Feed.17.7b  = c(18575/1356, 11359/1341, 18396/1349),
  Strip.17.7b = c(12635/2209, 12620/2238, 12429/2248)
)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(# M2 is Na, K and Mg for Mem.17.2, 17.3 and 17.4, respectively.,
  Feed.17.5.Li.a   = c(1.223, 0.915, 0.729, 0.528, 0.461, 0.372),
  Strip.17.5.Li.a  = c(0.001, 0.275, 0.498, 0.652, 0.772, 0.845),
  Feed.17.5.M2.a   = c(0.988, 0.987, 0.994),
  Strip.17.5.M2.a  = c(0.062, 0.112, 0.173),
  Feed.17.5.Li.b   = c(1.252, 0.811, 0.537, 0.349, 0.212, 0.153),
  Strip.17.5.Li.b  = c(0.001, 0.501, 0.769, 0.921, 1.042, 1.062),
  Feed.17.5.M2.b   = c(0.998, 0.996, 0.988),
  Strip.17.5.M2.b  = c(0.067, 0.274, 0.550),
  Feed.17.6.Li.a   = c(1.246, 0.853, 0.461, 0.319, 0.220, 0.154),
  Strip.17.6.Li.a  = c(0.003, 0.536, 0.793, 0.929, 1.005, 1.049),
  Feed.17.6.M2.a   = c(0.883, 0.887, 0.874),
  Strip.17.6.M2.a  = c(0.098, 0.321, 0.624),
  Feed.17.6.Li.b   = c(1.285, 0.812, 0.562, 0.391, 0.276, 0.204),
  Strip.17.6.Li.b  = c(0.002, 0.533, 0.779, 0.928, 1.025, 1.081),
  Feed.17.6.M2.b   = c(0.879, 0.878, 0.873),
  Strip.17.6.M2.b  = c(0.083, 0.173, 0.218),
  Feed.17.7.Li.a   = c(1.183, 1.070, 1.005, 0.958, 0.916, 0.883),
  Strip.17.7.Li.a  = c(0.001, 0.143, 0.232, 0.296, 0.362, 0.394),
  Feed.17.7.M2.a   = c(0.060, 0.041, 0.031),
  Strip.17.7.M2.a  = c(0.000, 0.056, 0.077),
  Feed.17.7.Li.b   = c(1.178, 1.003, 0.897, 0.822, 0.764, 0.710),
  Strip.17.7.Li.b  = c(0.004, 0.263, 0.394, 0.486, 0.546, 0.601),
  Feed.17.7.M2.b   = c(0.056, 0.058, 0.032),
  Strip.17.7.M2.b  = c(0.000, 0.067, 0.090)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- vector(mode = "list", length = length(AliAbs))
names(AliConc) <- names(AliAbs)
for (i in c(1, 2, 5, 6, 9, 10, 13, 14, 17, 18, 21, 22)) {
  AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Lithium.1)
}
for (i in c(3, 7)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Sodium.2,
                                               dilution = dilutions[[(i %/% 2)]])
for (i in c(4, 8)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Sodium.1,
                                               dilution = dilutions[[(i %/% 2)]])
for (i in c(11, 15)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Potassium.2,
                                                 dilution = dilutions[[(i %/% 2)]])
for (i in c(12, 16)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Potassium.1,
                                                 dilution = dilutions[[(i %/% 2)]])
for (i in c(19, 23)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Magnessium.2,
                                                 dilution = dilutions[[(i %/% 2)]])
for (i in c(20, 24)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Magnessium.2,
                                                 dilution = dilutions[[(i %/% 2)]])
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- vector(mode = "list", length = length(AliConc)/2)
names(TransFrac) <- paste0(rep(c("Lithium.", "Secondary."), length(TransFrac)/2),
                           paste0(rep(2:4, each = 4), rep(c('a', 'b'), each = 2)))
for (i in 1:(length(TransFrac)/2)) {
  #Lithium
  TransFrac[[i*2-1]] <- conc2frac(feed = AliConc[[4*i-3]], strip = AliConc[[4*i-2]], time = AliTimes[[i]],
                                  correct.strip = TRUE)
  #Sodium
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
pdf("Selectivity1-2.pdf", height = 75/25.4, width = 75/25.4)
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
