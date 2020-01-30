library(ggplot2)
library(ggformula)
library(transmem)
PDF <- FALSE
if (PDF) pdf("Perfiles20-09-19p2.pdf", height = 7/1.8, width = 9/1.8)

#-----STOCK SOLUTIONS--------------------------------------------------------
StockLi.200_2 <- 130.3 * 0.187872 * 0.99 / 0.1205105
StockNa.5000_1  <- 0.6399 * 0.996 / 50.0160 * 0.393372 * 1000000
StockK.10000_1  <- 0.9592 * 0.996 / 50.0812 * 0.52445 * 1000000
StockMg.10000_1  <- 1.7698 * 0.99 / 50.0892 * 0.28827 * 1000000

StockLi.5_7   <- StockLi.200_2 * 1.2673 / 50.0164
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
  Lithium.1 = data.frame(Conc = c(0.0000, 0.0597, 0.2989, 0.9115, 1.2053, 1.6816, 2.1670, 2.6474) *
                                StockLi.5_7 / c(6.0000, 6.1144, 6.0524, 6.0355, 6.1449, 6.0859, 6.0838, 6.0795),
                         Signal = c(0.000, 0.032, 0.162, 0.482, 0.614, 0.832, 1.028, 1.213)),
  Sodium.1 = data.frame(Conc = c(0.0000, 0.7204, 1.4464, 1.9935, 2.4448) *
                               StockNa.2_2 / c(6.0000, 6.3596, 6.1113, 6.1979, 6.0654),
                        Signal  = c(0, 0.015, 0.057, 0.095, 0.151)),
  Potassium.1 = data.frame(Conc = c(0.0000, 1.0503, 2.1497, 3.0142, 3.5976) * StockK.2_2 /
                             c(6.0000, 6.1914, 6.2460, 6.1937, 6.0239),
                           Signal = c(0.000, 0.035, 0.101, 0.210, 0.359)),
  Magnessium.1 = data.frame(Conc = c(0.0000, 0.2737, 0.7372, 1.4480, 2.0974, 2.7042) * StockMg.4_2 /
                              c(6.0000, 6.1481, 6.1110, 6.1526, 6.1464, 6.1803),
                            Signal = c(0.000, 0.049, 0.084, 0.251, 0.434, 0.604))
)
## for a cleaner workspace
#rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
CalModels <- list(
  Lithium.1 = calibCurve(curve = CalCurves$Lithium.1, order = 2),
  Sodium.1 = calibCurve(curve = CalCurves$Sodium.1, order = 2),
  Potassium.1 = calibCurve(curve = CalCurves$Potassium.1, order = 2),
  Magnessium.1 = calibCurve(curve = CalCurves$Magnessium.1, order = 2)
)
anova(CalModels$Lithium.1)
summary(CalModels$Lithium.1)
#-----MUESTRAS CIEGAS--------------------------------------------------------
BlindeP <- data.frame(LiRe = c(1.0042, 1.0079, 1.0128, 1.0050, 1.0238, 0.9993, 1.0325, 1.0409, 1.0411) * StockLi.5_7 /
                        c(6.2832, 6.1462, 6.0555, 6.1426, 6.1622, 6.1856, 6.0860, 6.2385, 6.2381),
                      LiSg = c(0.524, 0.550, 0.568, 0.532, 0.561, 0.554, 0.554, 0.539, 0.511))
BlindeP$LiIn <- signal2conc(signal = BlindeP$LiSg, model = CalModels$Lithium.1)
plot(x = BlindeP$LiRe, y = BlindeP$LiIn)
abline(a = 0, b = 1, col = 2, lty = 3)
abline(lm(BlindeP$LiIn ~ BlindeP$LiRe))
summary(lm(BlindeP$LiIn ~ BlindeP$LiRe))

t.test(x = BlindeP$LiIn, y = BlindeP$LiRe, paired = TRUE)
#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- list (
  T.17.2a = c(0, 1, 2.17, 3, 4, 5),
  T.17.2b = c(0, 1, 2, 3, 4, 5),
  T.17.3a = c(0, 1, 2, 3, 4, 5),
  T.17.3b = c(0, 1, 2, 3, 4, 5),
  T.17.4a = c(0, 1, 2, 3, 4, 5),
  T.17.4b = c(0, 1, 2, 3, 4, 5)
)
ts <- c(1, 3, 6)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
dilutions <- list(
  Feed.17.2a  = c(15012/1099, 14990/1098, 15032/1107),
  Strip.17.2a = c(13132/5044, 14002/5929, 12080/4076),
  Feed.17.2b  = c(14988/1144, 15059/1004, 15007/1108),
  Strip.17.2b = c(11370/3205, 11374/3245, 12970/4866),
  Feed.17.3a  = c(18156/1240, 18166/1258, 18127/1259),
  Strip.17.3a = c(11343/3250, 11371/3269, 10855/2746),
  Feed.17.3b  = c(18280/1285, 18221/1280, 18192/1264),
  Strip.17.3b = c(11445/3261, 11412/3288, 11374/3272),
  Feed.17.4a  = c(18131/1255, 11253/3235, 11314/3232),
  Strip.17.4a = c(18138/1204, 18236/1260, 18303/1255),
  Feed.17.4b  = c(18275/1260, 11258/3201, 11268/3201),
  Strip.17.4b = c(18165/1256, 18209/1249, 18201/1256)
)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(# M2 is Na, K and Mg for Mem.17.2, 17.3 and 17.4, respectively.,
  Feed.17.2.Li.a   = c(1.016, 0.750, 0.479, 0.350, 0.246, 0.175),
  Strip.17.2.Li.a  = c(0.002, 0.408, 0.709, 0.836, 0.919, 0.986),
  Feed.17.2.M2.a   = c(0.097, 0.095, 0.084),
  Strip.17.2.M2.a  = c(0.076, 0.073, 0.045),
  Feed.17.2.Li.b   = c(1.095, 0.698, 0.446, 0.290, 0.191, 0.131),
  Strip.17.2.Li.b  = c(0.005, 0.454, 0.723, 0.871, 0.953, 1.015),
  Feed.17.2.M2.b   = c(0.096, 0.082, 0.085),
  Strip.17.2.M2.b  = c(0.051, 0.031, 0.048),
  Feed.17.3.Li.a   = c(1.015, 0.850, 0.658, 0.500, 0.389, 0.308),
  Strip.17.3.Li.a  = c(0.002, 0.250, 0.499, 0.671, 0.783, 0.871),
  Feed.17.3.M2.a   = c(0.253, 0.291, 0.301),
  Strip.17.3.M2.a  = c(0.014, 0.031, 0.039),
  Feed.17.3.Li.b   = c(1.109, 0.736, 0.440, 0.258, 0.156, 0.098),
  Strip.17.3.Li.b  = c(0.002, 0.432, 0.747, 0.900, 0.978, 1.028),
  Feed.17.3.M2.b   = c(0.441, 0.491, 0.334),
  Strip.17.3.M2.b  = c(0.094, 0.083, 0.093),
  Feed.17.4.Li.a   = c(1.080, 0.809, 0.571, 0.398, 0.276, 0.190),
  Strip.17.4.Li.a  = c(0.002, 0.364, 0.619, 0.784, 0.883, 0.949),
  Feed.17.4.M2.a   = c(0.356, 0.081, 0.045),
  Strip.17.4.M2.a  = c(0.007, 0.274, 0.334),
  Feed.17.4.Li.b   = c(1.081, 0.914, 0.705, 0.512, 0.366, 0.273),
  Strip.17.4.Li.b  = c(0.002, 0.235, 0.489, 0.692, 0.818, 0.905),
  Feed.17.4.M2.b   = c(0.340, 0.089, 0.060),
  Strip.17.4.M2.b  = c(0.002, 0.274, 0.315)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- vector(mode = "list", length = length(AliAbs))
names(AliConc) <- names(AliAbs)
for (i in 1:(length(AliConc)/4)) {
  j = (i + 1) %/% 2 + 1
  AliConc[[4*i-1]] <- signal2conc(signal = AliAbs[[4*i-1]], model = CalModels[[j]],
                                      dilution = dilutions[[2*i-1]])
  AliConc[[4*i]]   <- signal2conc(signal = AliAbs[[4*i]], model = CalModels[[j]],
                                      dilution = dilutions[[2*i]])
  AliConc[[4*i-3]] <- signal2conc(signal = AliAbs[[4*i-3]], model = CalModels$Lithium.1)
  AliConc[[4*i-2]] <- signal2conc(signal = AliAbs[[4*i-2]], model = CalModels$Lithium.1)
}
AliConc[[4]] <- AliConc[[4]][c(3,2,1)]
AliConc[[7]] <- AliConc[[7]][c(2,1,3)]
AliConc[[8]] <- AliConc[[8]][c(3,2,1)]
AliConc[[11]] <- AliConc[[11]][c(3,2,1)]
AliConc[[15]] <- AliConc[[15]][c(2,1,3)]
AliConc[[16]] <- AliConc[[16]][c(1,3,2)]
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

sepFactor[[1]]
NewSepFac <- list(sepfactor(main = TransFrac[[1]], main0 = AliConc[[1]][1],
                            secon = TransFrac[[2]], secon0 = AliConc[[3]][1]))

sF <- vector()
sFsd <- vector()
for (i in 1:length(sepFactor)) {
  sF <- c(sF, mean(sepFactor[[i]][, 2]))
  sFsd <- c(sFsd, sd(sepFactor[[i]][, 2]))
}

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
pdf("Selectivity1-1.pdf", height = 75/25.4, width = 75/25.4)
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
