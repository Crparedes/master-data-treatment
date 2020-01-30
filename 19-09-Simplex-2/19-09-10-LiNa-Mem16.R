library(ggplot2)
library(ggformula)
library(transMem)
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
  T.16.1 = c(0, 1, 2, 3, 4, 5),
  T.16.2 = c(0, 1, 2, 3, 4, 5),
  T.16.3 = c(0, 1, 2, 3, 4, 5),
  T.16.4 = c(0, 1, 2, 3, 4, 5),
  T.16.5 = c(0, 1, 2, 3, 4, 5)
)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
dilutions <- list(
  Feed.16.1  = c(2.0319/0.0491, 2.0081/0.0502, 2.0117/0.0474),
  Strip.16.1 = c(1.0793/0.3244, 1.0672/0.3191, 1.0811/0.3225),
  Feed.16.2  = c(2.0273/0.0484, 2.0260/0.0515, 2.0184/0.0506),
  Strip.16.2 = c(1.0717/0.3238, 1.0691/0.3197, 1.0760/0.3237),
  Feed.16.3  = c(2.0210/0.0501, 2.0254/0.0463, 2.0160/0.0451),
  Strip.16.3 = c(1.0666/0.3244, 1.0710/0.3193, 1.0729/0.3241),
  Feed.16.4  = c(2.0065/0.0448, 2.0440/0.0480, 2.0107/0.0485),
  Strip.16.4 = c(1.0821/0.3265, 1.0612/0.3132, 1.0739/0.3279),
  Feed.16.5  = c(2.0408/0.0494, 2.0164/0.0487, 2.0546/0.0565),
  Strip.16.5 = c(1.0708/0.3121, 1.0692/0.3234, 1.0611/0.3246)
)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(
  Feed.16.1.Li   = 0.5 * (c(0.302, 0.175, 0.107, 0.071, 0.047, 0.032) + c(0.307, 0.198, 0.132, 0.106, 0.081, 0.061)),
  Strip.16.1.Li  = 0.5 * (c(0.000, 0.123, 0.188, 0.222, 0.246, 0.267) + c(0.000, 0.103, 0.167, 0.193, 0.216, 0.237)),
  Feed.16.1.Na   = c(0.255, 0.262, 0.263),
  Strip.16.1.Na  = c(0.027, 0.027, 0.052),
  Feed.16.2.Li   = c(0.300, 0.205, 0.150, 0.117, 0.093, 0.076),
  Strip.16.2.Li  = c(0.001, 0.090, 0.146, 0.174, 0.195, 0.238),
  Feed.16.2.Na   = c(0.276, 0.274, 0.265),
  Strip.16.2.Na  = c(0.004, 0.025, 1.066),
  Feed.16.3.Li   = c(0.302, 0.223, 0.165, 0.129, 0.102, 0.082),
  Strip.16.3.Li  = c(0.000, 0.063, 0.125, 0.163, 0.186, 0.216),
  Feed.16.3.Na   = c(0.255, 0.254, 0.263),
  Strip.16.3.Na  = c(0.000, 0.011, 0.017),
  Feed.16.4.Li   = 0.5 * (c(0.307, 0.254, 0.220, 0.193, 0.178, 0.162) + c(0.301, 0.241, 0.207, 0.178, 0.159, 0.143)),
  Strip.16.4.Li  = 0.5 * (c(0.001, 0.051, 0.092, 0.112, 0.128, 0.144) + c(0.001, 0.056, 0.095, 0.120, 0.140, 0.154)),
  Feed.16.4.Na   = c(0.257, 0.263, 0.261),
  Strip.16.4.Na  = c(0.002, 0.009, 0.017),
  Feed.16.5.Li   = 0.5 * (c(0.303, 0.227, 0.183, 0.148, 0.124, 0.108) + c(0.313, 0.233, 0.194, 0.162, 0.140, 0.122)),
  Strip.16.5.Li  = 0.5 * (c(0.000, 0.077, 0.122, 0.151, 0.173, 0.189) + c(0.001, 0.075, 0.112, 0.142, 0.163, 0.176)),
  Feed.16.5.Na   = c(0.269, 0.267, 0.300),
  Strip.16.5.Na  = c(0.004, 0.023, 0.027)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- list()
for (i in 1:5) {
  ts <- c(1, 3, 6)
  eval(parse(text = paste0("AliConc$Feed.16.", i, ".Na <- signal2conc(signal = AliAbs$Feed.16.", i, ".Na,
                            model = CalModels$Sodium.1, dilution = dilutions$Feed.16.", i, ")")))
  eval(parse(text = paste0("AliConc$Strip.16.", i, ".Na <- signal2conc(signal = AliAbs$Strip.16.", i, ".Na,
                            model = CalModels$Sodium.1, dilution = dilutions$Strip.16.", i, ")")))
  eval(parse(text = paste0("AliConc$Feed.16.", i, ".Li <- signal2conc(signal = AliAbs$Feed.16.", i, ".Li,
                            model = CalModels$Lithium.P, planar = TRUE,
                            Conc.S = fixSecondary(metalConc = AliConc$Feed.16.", i, ".Na,
                                                  time = AliTimes$T.16.", i, "[ts],
                                                  compTime = AliTimes$T.16.", i, ", order = 2))")))
  eval(parse(text = paste0("AliConc$Strip.16.", i, ".Li <- signal2conc(signal = AliAbs$Strip.16.", i, ".Li,
                            model = CalModels$Lithium.P, planar = TRUE,
                            Conc.S = fixSecondary(metalConc = AliConc$Strip.16.", i, ".Na,
                                                  time = AliTimes$T.16.", i, "[ts],
                                                  compTime = AliTimes$T.16.", i, ", order = 2))")))
}
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- list()
for (i in 1:5) {
  ts <- c(1, 3, 6)
  eval(parse(text = paste0("TransFrac$M.16.", i, ".Li <- conc2frac(feed = AliConc$Feed.16.", i, ".Li,
                            strip = AliConc$Strip.16.", i, ".Li, time = AliTimes$T.16.", i, ")")))
  eval(parse(text = paste0("TransFrac$M.16.", i, ".Na <- conc2frac(feed = AliConc$Feed.16.", i, ".Na,
                            strip = AliConc$Strip.16.", i, ".Na, time = AliTimes$T.16.", i, "[ts])")))
  #eval(parse(text = paste0("TransFrac$M.16.", i, ".Na$Fraction[which(TransFrac$M.16.", i, ".Na$Phase == 'Strip')] <-
  #  1 - TransFrac$M.16.", i, ".Na$Fraction[which(TransFrac$M.16.", i, ".Na$Phase == 'Feed')]")))
}
#rm(sub.Na)
#-----MODELOS DE REGRESIÓN NO LINEAL-----------------------------------------
TransNLS <- list()
SS_par <- vector()
for (i in 1:5) {
  eval(parse(text = paste0("X <- transTrend(TransFrac$M.16.", i, ".Li, model = 'paredes', eccen = 1)")))
  SS_par <- c(SS_par, sum(resid(X$feed)^2), sum(resid(X$strip)^2))
  eval(parse(text = paste0("TransNLS$M.16.", i, " <- X")))

}

TransNLSXot <- list()
SS_xot <- vector()
for (i in 1:5) {
  eval(parse(text = paste0("X <- transTrend(TransFrac$M.16.", i, ".Li, model = 'rodriguez')")))
  SS_xot <- c(SS_xot, sum(resid(X$feed)^2), sum(resid(X$strip)^2))
    eval(parse(text = paste0("TransNLSXot$M.16.", i, " <- X")))
}
t.test(x = SS_par, y = SS_xot, paired = TRUE)
plot(SS_par, SS_xot)
abline(lm(SS_xot~SS_par))
lm(SS_xot~SS_par)
#-----FACTORES DE SEPARACIÓN-------------------------------------------------
order = c(1, 8, 2, 7, 3, 6, 4, 5)
sepFactor <- list()
for (i in 1:5) {
  eval(parse(text = paste0("X <- data.frame(time = AliTimes$T.16.", i, ",
                                   factor = (AliConc$Strip.16.", i, ".Li /
                                     fixSecondary(metalConc = AliConc$Strip.16.", i, ".Na,
                                                  time = AliTimes$T.16.", i, "[c(1, 3, 6)],
                                                  compTime = AliTimes$T.16.", i, ", order = 2)) /
                                            (AliConc$Feed.16.", i, ".Li[1]/AliConc$Feed.16.", i, ".Na[1]))")))
  
  X <- X[-1, ]
  eval(parse(text = paste0("sepFactor$M.16.", i, " <- X")))
}
ssepFactor <- data.frame()

for (i in 1:5) ssepFactor <- rbind(ssepFactor, sepFactor[[i]])

ssepFactor$Membrana <- as.factor(paste0('Mem.16.', rep(1:5, each = 5)))
ggplot(data = ssepFactor, aes(x = time, y = factor, colour = Membrana)) + geom_point() + theme_bw() +
  ggsci::scale_color_npg() + stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, size = 0.4) +
  xlab(label = "Tiempo (horas)") + ylab(label = "Factor de separación")

sF <- vector()
for (i in 1:5) sF <- c(sF, mean(sepFactor[[i]][, 2]))


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
#-----PERFILES DE TRANSPORTE ------------------------------------------------

Parameters <- data.frame()

for (i in 1:5) {
# invisible(readline(prompt="Press [enter] to continue"))
  eval(parse(text = paste0("transPlot(trans = TransFrac$M.16.", i, ".Li, trend = TransNLS$M.16.", i, ",
                             secondary = TransFrac$M.16.", i, ".Na, lin.secon = TRUE,
                             xlim = c(0, 5.2), ylim = c(-0.05, 1.08),
                             ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:5)")))
  eval(parse(text = paste0("Parameters <- rbind(Parameters, c(TransNLS$M.16.", i, "$Result, sF[i],
                           TransFrac$M.16.", i, ".Li[12, 3], TransFrac$M.16.", i, ".Li[11, 3],
                           TransFrac$M.16.", i, ".Li[10, 3], TransFrac$M.16.", i, ".Li[9, 3],
                           TransFrac$M.16.", i, ".Li[8, 3]))")))
}

for (i in 1:5) {
  # invisible(readline(prompt="Press [enter] to continue"))
  transPlot(trans = TransFrac[[(i*2-1)]], trend = TransNLS[[i]],
                             secondary = TransFrac[[i*2]], lin.secon = TRUE,
                             xlim = c(0, 5.2), ylim = c(-0.05, 1.08),
                             ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:5)
  
  eval(parse(text = paste0("Parameters <- rbind(Parameters, c(TransNLS$M.16.", i, "$Result, sF[i],
                           TransFrac$M.16.", i, ".Li[12, 3], TransFrac$M.16.", i, ".Li[11, 3],
                           TransFrac$M.16.", i, ".Li[10, 3], TransFrac$M.16.", i, ".Li[9, 3],
                           TransFrac$M.16.", i, ".Li[8, 3]))")))
}

#for (i in 1:7) {
#  eval(parse(text = paste0("transPlot(trans = TransFrac$M.16.", i, ".Li, trend = TransNLS$M.16.", i, ",
#                             xlim = c(0, 26.1), ylim = c(-0.05, 1.08),
#                             ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = c(0, 4, 8, 12, 16, 20, 24))")))
#}
colnames(Parameters) <- c(names(TransNLS$M.16.1$Result), "sF")
round(Parameters, 3)


if(F){
#-----REPETEABILITY BETWEEN RUNS---------------------------------------------
vel <- c("Exp 1", "Exp 2", "Exp 3", "Exp 4", "Exp 4", "Exp 3", "Exp 2", "Exp 1")
frac <- vector()
for (i in 1:5) frac <- c(frac, TransFrac[[2*i-1]]$Fraction[7:12])
velData <- data.frame(Agit. = as.factor(rep(vel, each = 6)), Phi_strip = frac, Time = rep(AliTimes[[1]][1:6], 8))

p <- ggplot(data = velData, aes(x = Time, y = Phi_strip, group = Agit., color = Agit.)) +
  theme_bw() + geom_point(size = 3, shape = 15)  +
  labs(y = expression(Phi), x = 'Time (h)') +
  theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(color = "black"),
        axis.text.y = ggplot2::element_text(color = "black"))


cols = gg_color_hue(4)
e <- TransNLS[[1]]$eccen

(p <- p + stat_function(fun = function(x) ((coefficients(TransNLS[[1]]$strip)[1] * x^e)
                                           / (1 / coefficients(TransNLS[[1]]$strip)[2] + x^e)),
                        color = cols[1], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                        xlim = c(0, 5), size = 0.4) +
    stat_function(fun = function(x) ((coefficients(TransNLS[[2]]$strip)[1] * x^e)
                                     / (1 / coefficients(TransNLS[[2]]$strip)[2] + x^e)),
                  color = cols[2], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                  xlim = c(0, 5), size = 0.4) +
    stat_function(fun = function(x) ((coefficients(TransNLS[[3]]$strip)[1] * x^e)
                                     / (1 / coefficients(TransNLS[[3]]$strip)[2] + x^e)),
                  color = cols[3], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                  xlim = c(0, 5), size = 0.4) +
    stat_function(fun = function(x) ((coefficients(TransNLS[[4]]$strip)[1] * x^e)
                                     / (1 / coefficients(TransNLS[[4]]$strip)[2] + x^e)),
                  color = cols[4], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                  xlim = c(0, 5), size = 0.4) +
    stat_function(fun = function(x) ((coefficients(TransNLS[[5]]$strip)[1] * x^e)
                                     / (1 / coefficients(TransNLS[[5]]$strip)[2] + x^e)),
                  color = cols[4], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                  xlim = c(0, 5), size = 0.4) +
    stat_function(fun = function(x) ((coefficients(TransNLS[[6]]$strip)[1] * x^e)
                                     / (1 / coefficients(TransNLS[[6]]$strip)[2] + x^e)),
                  color = cols[3], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                  xlim = c(0, 5), size = 0.4) +
    stat_function(fun = function(x) ((coefficients(TransNLS[[7]]$strip)[1] * x^e)
                                     / (1 / coefficients(TransNLS[[7]]$strip)[2] + x^e)),
                  color = cols[2], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                  xlim = c(0, 5), size = 0.4) +
    stat_function(fun = function(x) ((coefficients(TransNLS[[8]]$strip)[1] * x^e)
                                     / (1 / coefficients(TransNLS[[8]]$strip)[2] + x^e)),
                  color = cols[1], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                  xlim = c(0, 5), size = 0.4)# +
    #geom_point(size = 3, shape = 15, color = rep(gg_color_hue(4), each = 6))
  )


if (PDF) dev.off()

#-----FACTORES DE SEPARACIÓN-------------------------------------------------
sepFactor <- list()
for (i in 1:8) {
  eval(parse(text = paste0("X <- data.frame(time = AliTimes$T.16.", i, "[1:6],
                           factor = (AliConc$Strip.16.", i, ".Li[1:6] /
                           fixSecondary(metalConc = AliConc$Strip.16.", i, ".Na[1:3],
                           time = AliTimes$T.16.", i, "[c(1, 3, 6)],
                           compTime = AliTimes$T.16.", i, "[1:6], order = 2)) /
                           (AliConc$Feed.16.", i, ".Li[1]/AliConc$Feed.16.", i, ".Na[1]))")))

  X <- X[-1, ]
  eval(parse(text = paste0("sepFactor$M.16.", i, " <- X")))
}
ssepFactor <- data.frame()

for (i in 1:8) ssepFactor <- rbind(ssepFactor, sepFactor[[i]])

ssepFactor$Membrana <- as.factor(paste0('Mem.16.', rep(1:8, each = 5)))
ggplot(data = ssepFactor, aes(x = time, y = factor, colour = Membrana)) + geom_point() + theme_bw() +
  ggsci::scale_color_npg() + stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, size = 0.4) +
  xlab(label = "Tiempo (horas)") + ylab(label = "Factor de separación")

sF <- vector()
for (i in 1:8) sF <- c(sF, mean(sepFactor[[i]][, 2]))

if (PDF) dev.off()

# TEMPERATURE----------------------------------------------------
Temp <- data.frame(Mem.16.1 = mean(c(68, 72, 73, 72, 72)), 
             Mem.16.2 = mean(c(68, 68, 66, 66, 64)),
             Mem.16.3 = mean(c(61, 59, 59, 59, 61)),
             Mem.16.4 = mean(c(68, 70, 72, 75, 73)),
             Mem.16.5 = mean(c(72, 70, 68, 68, 68)),
             Mem.16.6 = mean(c(68, 72, 75, 75, 77)),
             Mem.16.7 = mean(c(70, 70, 66, 66, 66)),
             Mem.16.8 = mean(c(70, 72, 73, 75, 77)))
Temp[order]
Parameters
}
dev.off()
