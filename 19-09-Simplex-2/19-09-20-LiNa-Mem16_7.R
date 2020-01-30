library(ggplot2)
library(ggformula)
library(transmem)
PDF <- FALSE
if (PDF) pdf("Perfiles20-09-19.pdf", height = 7/1.8, width = 9/1.8)

#-----STOCK SOLUTIONS--------------------------------------------------------
StockLi.200_2 <- 130.3 * 0.187872 * 0.99 / 0.1205105
StockNa.11000 <- 1.1693 * 0.996 /41.5065 * 0.393372 * 1000000

StockLi.5_6   <- StockLi.200_2 * 1.2650 / 50.0864

StockNa.600_2 <- StockNa.11000  * 1.6605 / 30.0755
StockNa.10_3  <- StockNa.600_2  * 0.6065 / 30.0068
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CalCurves <- list(
  Lithium.P = data.frame(Conc = c(0.0000, 0.0566, 0.0573, 0.1302, 0.1222, 0.1264, 0.2505, 0.2676, 0.6035, 0.6022,
                                  1.2167, 1.2060, 1.2341, 2.4143, 2.4166, 2.6897, 2.6934, 2.6938) *
                                StockLi.5_6 / c(6.0000, 6.1509, 6.0088, 6.0399, 6.0856, 6.0786, 6.0121, 6.0258,
                                                6.0866, 6.0290, 6.0289, 6.0364, 6.0655, 6.0202, 6.0293, 6.0689,
                                                6.0541, 6.1592),
                         Signal = c(0.000, 0.007, 0.007, 0.015, 0.016, 0.017, 0.032, 0.035, 0.075, 0.076,
                                    0.142, 0.147, 0.154, 0.293, 0.296, 0.316, 0.323, 0.310),
                         Conc.S = c(0.0000, 0.2770, 1.5102, 0.0000, 0.5191, 2.0127, 0.5132, 1.5121, 0.5081, 1.6378,
                                    0.0000, 0.9990, 2.0486, 0.2315, 1.5022, 0.0000, 0.5067, 2.0409) *
                                  StockNa.600_2 / c(6.0000, 6.1509, 6.0088, 6.0399, 6.0856, 6.0786, 6.0121, 6.0258,
                                                    6.0866, 6.0290, 6.0289, 6.0364, 6.0655, 6.0202, 6.0293, 6.0689,
                                                    6.0541, 6.1592)),
  Sodium.1 = data.frame(Conc = c(0.0000, 0.0672, 0.1321, 0.3215, 0.6450, 1.5131, 3.0879, 4.1388) *
                               StockNa.10_3 / c(6.0000, 6.0089, 6.3138, 6.1288, 6.3744, 6.0450, 6.0895, 6.3559),
                        Signal  = c(0.000, 0.028, 0.048, 0.099, 0.169, 0.389, 0.751, 0.921))
)
## for a cleaner workspace
#rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
CalModels <- list(
  Lithium.P = calibPlane(plane = CalCurves$Lithium.P),
  Sodium.1 = calibCurve(curve = CalCurves$Sodium.1, order = 2)
)
anova(CalModels$Lithium.P$model)
summary(CalModels$Lithium.P$model)
#-----MUESTRAS CIEGAS--------------------------------------------------------
BlindeP <- data.frame(LiRe = c(1.0245, 0.4836) * StockLi.5_6 /
                        c(6.1086, 6.1369),
                      LiSg = c(0.126, 0.060),
                      NaRe = c(1.0255, 0.2008) * StockNa.600_2 /
                        c(6.1086, 6.1369))
BlindeP$LiIn <- signal2conc(signal = BlindeP$LiSg, model = CalModels$Lithium.P, planar = TRUE,
                            Conc.S = BlindeP$NaRe)
plot(x = BlindeP$LiRe, y = BlindeP$LiIn)
abline(a = 0, b = 1, col = 2, lty = 3)
abline(lm(BlindeP$LiIn ~ BlindeP$LiRe))
summary(lm(BlindeP$LiIn ~ BlindeP$LiRe))

t.test(x = BlindeP$LiIn, y = BlindeP$LiRe, paired = TRUE)

#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- list (
  T.16.7a = c(0, 1, 2, 3, 4, 5),
  T.16.7b = c(0, 1, 2, 3, 4, 5)
)
ts <- c(1, 3, 6)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
dilutions <- list(
  Feed.16.7a  = c(2.0532/0.0600, 2.0508/0.0598, 2.0597/0.0615),
  Strip.16.7a = c(1.0383/0.1967, 1.0221/0.1977, 1.0138/0.1970),
  Feed.16.7b  = c(2.0590/0.0599, 2.0665/0.0598, 2.0598/0.0610),
  Strip.16.7b = c(1.0300/0.1978, 1.0147/0.1977, 1.0117/0.1979)
)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(
  Feed.16.7.Li.a   = c(0.308, 0.210, 0.155, 0.114, 0.088, 0.069),
  Strip.16.7.Li.a  = c(0.001, 0.097, 0.155, 0.193, 0.218, 0.236),
  Feed.16.7.Na.a   = c(0.337, 0.333, 0.333),
  Strip.16.7.Na.a  = c(0.007, 0.023, 0.040),
  Feed.16.7.Li.b   = c(0.314, 0.188, 0.126, 0.087, 0.062, 0.046),
  Strip.16.7.Li.b  = c(0.002, 0.122, 0.185, 0.220, 0.243, 0.259),
  Feed.16.7.Na.b   = c(0.325, 0.323, 0.331),
  Strip.16.7.Na.b  = c(0.007, 0.030, 0.070)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- vector(mode = "list", length = length(AliAbs))
names(AliConc) <- names(AliAbs)
for (i in 1:(length(AliConc)/4)) {
  #Feed sodium
  AliConc[[4*i-1]] <- signal2conc(signal = AliAbs[[4*i-1]], model = CalModels$Sodium.1,
                                  dilution = dilutions[[2*i-1]])
  #Strip sodium
  AliConc[[4*i]]   <- signal2conc(signal = AliAbs[[4*i]], model = CalModels$Sodium.1,
                                  dilution = dilutions[[2*i]])
  #Feed lithium
  AliConc[[4*i-3]] <- signal2conc(signal = AliAbs[[4*i-3]], model = CalModels$Lithium.P, planar = TRUE,
                                  Conc.S = fixSecondary(metalConc = AliConc[[4*i-1]],
                                                        time = AliTimes[[i]][ts], compTime = AliTimes[[i]],
                                                        order = 2))
  #Strip litium
  AliConc[[4*i-2]] <- signal2conc(signal = AliAbs[[4*i-2]], model = CalModels$Lithium.P, planar = TRUE,
                                  Conc.S = fixSecondary(metalConc = AliConc[[4*i]],
                                                        time = AliTimes[[i]][ts], compTime = AliTimes[[i]],
                                                        order = 2))
}
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- vector(mode = "list", length = length(AliConc)/2)
names(TransFrac) <- paste0(rep(c("Lithium.", "Sodium."), length(TransFrac)/2),
                           rep(c("0a", "0b"), each = 2))
for (i in 1:(length(TransFrac)/2)) {
  #Lithium
  TransFrac[[i*2-1]] <- conc2frac(feed = AliConc[[4*i-3]], strip = AliConc[[4*i-2]], time = AliTimes[[i]])
  #Sodium
  TransFrac[[i*2]]   <- conc2frac(feed = AliConc[[4*i-1]], strip = AliConc[[4*i]], time = AliTimes[[i]][ts])
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

ssepFactor$Membrana <- as.factor(paste0("Mem.", rep(c("0a", "0b"), each = 5)))
ggplot(data = ssepFactor, aes(x = time, y = factor, colour = Membrana)) + geom_point() + theme_bw() +
  ggsci::scale_color_npg() + stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, size = 0.4) +
  xlab(label = "Tiempo (horas)") + ylab(label = "Factor de separación")

sF <- vector()
for (i in 1:length(sepFactor)) sF <- c(sF, mean(sepFactor[[i]][, 2]))


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
#-----PERFILES DE TRANSPORTE ------------------------------------------------
for (i in 1:1) {
  transPlotWR(trans = list(TransFrac[[4*i-3]], TransFrac[[4*i-1]]),
              trend = list(TransNLS[[2*i-1]], TransNLS[[2*i]]),
              secondary = list(TransFrac[[4*i-2]], TransFrac[[4*i]]),
              lin.secon = TRUE, xlim = c(0, 5.2), ylim = c(-0.05, 1.08),
              ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:5)
}
# invisible(readline(prompt="Press [enter] to continue"))
#-----PARÁMETROS DE DESEMPEÑO------------------------------------------------
Parameters <- data.frame()
j = 0
for (i in 1:2) {
  Parameters <- rbind(Parameters, c(TransNLS[[i]]$Result, sF[i], TransFrac[[2*i-1]][12, 3]))
}

colnames(Parameters) <- c(names(TransNLS[[1]]$Result), "sF")
round(Parameters, 3)
if (PDF) dev.off()
