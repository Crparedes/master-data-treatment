library(ggplot2)
library(ggformula)
library(transmem)
PDF <- FALSE
if (PDF) pdf("Perfiles19-09-10.pdf", height = 7/1.8, width = 9/1.8)

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
                        Signal  = c(0.000, 0.026, 0.034, 0.091, 0.166, 0.398, 0.724, 0.882))
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
  T.16.1a = c(0, 1, 2, 3, 4, 5),
  T.16.1b = c(0, 1, 2, 3, 4, 5),
  T.16.2b = c(0, 1, 2, 3, 4, 5),
  T.16.3a = c(0, 1, 2, 3, 4, 5),
  T.16.4a = c(0, 1, 2, 3, 4, 5),
  T.16.4b = c(0, 1, 2, 3, 4, 5),
  T.16.5a = c(0, 1, 2, 3, 4, 5),
  T.16.5b = c(0, 1, 2, 3, 4, 5)
)
ts <- c(1, 3, 6)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
dilutions <- list(
  Feed.16.1a  = c(2.0319/0.0491, 2.0081/0.0502, 2.0117/0.0474),
  Strip.16.1a = c(1.0793/0.3244, 1.0672/0.3191, 1.0811/0.3225),
  Feed.16.1b  = c(2.0348/0.0509, 2.0196/0.0482, 2.0105/0.0497),
  Strip.16.1b = c(1.0772/0.3250, 1.0693/0.3227, 1.0671/0.3151),
  Feed.16.2b  = c(2.0273/0.0484, 2.0260/0.0515, 2.0184/0.0506),
  Strip.16.2b = c(1.0717/0.3238, 1.0691/0.3197, 1.0760/0.3237),
  Feed.16.3a  = c(2.0210/0.0501, 2.0254/0.0463, 2.0160/0.0451),
  Strip.16.3a = c(1.0666/0.3244, 1.0710/0.3193, 1.0729/0.3241),
  Feed.16.4a  = c(2.0065/0.0448, 2.0440/0.0480, 2.0107/0.0485),
  Strip.16.4a = c(1.0821/0.3265, 1.0612/0.3132, 1.0739/0.3279),
  Feed.16.4b  = c(2.0017/0.0500, 2.0039/0.0522, 2.0062/0.0497),
  Strip.16.4b = c(1.0792/0.3218, 1.0678/0.3156, 1.0523/0.3003),
  Feed.16.5a  = c(2.0408/0.0494, 2.0164/0.0487, 2.0546/0.0565),
  Strip.16.5a = c(1.0708/0.3121, 1.0692/0.3234, 1.0611/0.3246),
  Feed.16.5b  = c(2.0378/0.0456, 2.0025/0.0497, 2.0221/0.0542),
  Strip.16.5b = c(1.0770/0.3252, 1.0537/0.3067, 1.0665/0.3124)
)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(
  Feed.16.1.Li.a   = c(0.302, 0.175, 0.107, 0.071, 0.047, 0.032),
  Strip.16.1.Li.a  = c(0.000, 0.123, 0.188, 0.222, 0.246, 0.267),
  Feed.16.1.Na.a   = c(0.255, 0.250, 0.240),
  Strip.16.1.Na.a  = c(0.027, 0.027, 0.052),
  Feed.16.1.Li.b   = c(0.307, 0.198, 0.132, 0.106, 0.081, 0.061),
  Strip.16.1.Li.b  = c(0.000, 0.103, 0.167, 0.193, 0.216, 0.237),
  Feed.16.1.Na.b   = c(0.276, 0.270, 0.265),
  Strip.16.1.Na.b  = c(0.001, 0.037, 0.100),
  Feed.16.2.Li.b   = c(0.300, 0.205, 0.150, 0.117, 0.093, 0.076),
  Strip.16.2.Li.b  = c(0.001, 0.090, 0.146, 0.174, 0.195, 0.238),
  Feed.16.2.Na.b   = c(0.276, 0.274, 0.265),
  Strip.16.2.Na.b  = c(0.004, 0.025, 0.166),
  Feed.16.3.Li.a   = c(0.302, 0.223, 0.165, 0.129, 0.102, 0.082),
  Strip.16.3.Li.a  = c(0.000, 0.063, 0.125, 0.163, 0.186, 0.216),
  Feed.16.3.Na.a   = c(0.269, 0.254, 0.253),
  Strip.16.3.Na.a  = c(0.000, 0.011, 0.017),
  Feed.16.4.Li.a   = c(0.307, 0.254, 0.220, 0.193, 0.178, 0.162),
  Strip.16.4.Li.a  = c(0.001, 0.051, 0.092, 0.112, 0.128, 0.144),
  Feed.16.4.Na.a   = c(0.257, 0.263, 0.261),
  Strip.16.4.Na.a  = c(0.002, 0.009, 0.017),
  Feed.16.4.Li.b   = c(0.301, 0.241, 0.207, 0.178, 0.159, 0.143),
  Strip.16.4.Li.b  = c(0.001, 0.056, 0.095, 0.120, 0.140, 0.154),
  Feed.16.4.Na.b   = c(0.267, 0.271, 0.267),
  Strip.16.4.Na.b  = c(0.025, 0.020, 0.034),
  Feed.16.5.Li.a   = c(0.303, 0.227, 0.183, 0.148, 0.124, 0.108),
  Strip.16.5.Li.a  = c(0.000, 0.077, 0.122, 0.151, 0.173, 0.189),
  Feed.16.5.Na.a   = c(0.269, 0.267, 0.300),
  Strip.16.5.Na.a  = c(0.004, 0.023, 0.027),
  Feed.16.5.Li.b   = c(0.313, 0.233, 0.194, 0.162, 0.140, 0.122),
  Strip.16.5.Li.b  = c(0.001, 0.075, 0.112, 0.142, 0.163, 0.176),
  Feed.16.5.Na.b   = c(0.263, 0.273, 0.304),
  Strip.16.5.Na.b  = c(0.007, 0.028, 0.045)
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
                                  Conc.S = fixSecondary(conc = AliConc[[4*i-1]],
                                                        time = AliTimes[[i]][ts], compTime = AliTimes[[i]],
                                                        order = 2))
  #Strip litium
  AliConc[[4*i-2]] <- signal2conc(signal = AliAbs[[4*i-2]], model = CalModels$Lithium.P, planar = TRUE,
                                  Conc.S = fixSecondary(conc = AliConc[[4*i]],
                                                        time = AliTimes[[i]][ts], compTime = AliTimes[[i]],
                                                        order = 2))
}
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- vector(mode = "list", length = length(AliConc)/2)
names(TransFrac) <- paste0(rep(c("Lithium.", "Sodium."), length(TransFrac)/2),
                           rep(c("1a", "1b", "2b", "3a", "4a", "4b", "5a", "5b"), each = 2))
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
  sec <- fixSecondary(conc = AliConc[[4*i]], time = AliTimes[[i]][ts], compTime = AliTimes[[i]], order = 2)
  X <- data.frame(time = AliTimes[[i]],
                  factor = (AliConc[[i*4-2]]/sec) / (AliConc[[i*4-3]][1]/AliConc[[i*4-1]][1]))
  #X$factor[1] <- 1
  X <- X[-1, ]
  sepFactor[[i]] <- X
}

ssepFactor <- data.frame()
for (i in 1:length(sepFactor)) ssepFactor <- rbind(ssepFactor, sepFactor[[i]])

ssepFactor$Membrana <- as.factor(paste0("Mem.", rep(c("1a", "1b", "2b", "3a", "4a", "4b", "5a", "5b"), each = 5)))
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
for (i in 1:4) {
  if (i != 2) {
    transPlotWR(trans = list(TransFrac[[4*i-3]], TransFrac[[4*i-1]]),
                trend = list(TransNLS[[2*i-1]], TransNLS[[2*i]]),
                secondary = list(TransFrac[[4*i-2]], TransFrac[[4*i]]),
                lin.secon = TRUE, xlim = c(0, 5.2), ylim = c(-0.05, 1.08),
                ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:5)
  } else {
    transPlot(trans = TransFrac[[(4*i-3)]],
              trend = TransNLS[[2*i-1]],
              secondary = TransFrac[[4*i-2]], lin.secon = TRUE,
              xlim = c(0, 5.2), ylim = c(-0.05, 1.08),
              ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:5)
    transPlot(trans = TransFrac[[(4*i-1)]],
              trend = TransNLS[[2*i]],
              secondary = TransFrac[[4*i]], lin.secon = TRUE,
              xlim = c(0, 5.2), ylim = c(-0.05, 1.08),
              ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:5)
  }
}
  # invisible(readline(prompt="Press [enter] to continue"))
#-----PARÁMETROS DE DESEMPEÑO------------------------------------------------
Parameters <- data.frame()
j = 0
for (i in 1:8) {
  Parameters <- rbind(Parameters, c(TransNLS[[i]]$Result, sF[i], TransFrac[[2*i-1]][12, 3]))
}

colnames(Parameters) <- c(names(TransNLS[[1]]$Result), "sF")
round(Parameters, 3)
if (PDF) dev.off()

p <- transPlotWR(trans = list(TransFrac[[1]], TransFrac[[3]]),
                 trend = list(TransNLS[[1]], TransNLS[[2]]),
                 secondary = list(TransFrac[[2]], TransFrac[[4]]),
                 lin.secon = TRUE, xlim = c(0, 5.2), ylim = c(-0.01, 1.01),
                 ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:5, xlab = 'Tiempo (h)', bw = TRUE, srs = 0.5)
p

