library(ggplot2)
library(ggformula)
library(transMem)
PDF <- FALSE
if (PDF) pdf("Perfiles19-08-28.pdf", height = 7/1.8, width = 9/1.8)

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
                         Signal = c(0.000, 0.009, 0.009, 0.019, 0.021, 0.022, 0.041, 0.045, 0.098, 0.100,
                                    0.183, 0.193, 0.204, 0.370, 0.384, 0.392, 0.416, 0.416),
                         Conc.S = c(0.0000, 0.2770, 1.5102, 0.0000, 0.5191, 2.0127, 0.5132, 1.5121, 0.5081, 1.6378,
                                    0.0000, 0.9990, 2.0486, 0.2315, 1.5022, 0.0000, 0.5067, 2.0409) *
                                  StockNa.600_2 / c(6.0000, 6.1509, 6.0088, 6.0399, 6.0856, 6.0786, 6.0121, 6.0258,
                                                    6.0866, 6.0290, 6.0289, 6.0364, 6.0655, 6.0202, 6.0293, 6.0689,
                                                    6.0541, 6.1592)),
  Sodium.1 = data.frame(Conc = c(0.0000, 0.0672, 0.1321, 0.3215, 0.6450, 1.5131, 3.0879, 4.1388) *
                               StockNa.10_3 / c(6.0000, 6.0089, 6.3138, 6.1288, 6.3744, 6.0450, 6.0895, 6.3559),
                        Signal  = c(0.000, 0.027, 0.036, 0.100, 0.198, 0.465, 0.848, 1.014))
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
BlindeP <- data.frame(LiRe = c(1.0245, 0.4836, 1.8482) * StockLi.5_6 /
                        c(6.1086, 6.1369, 6.0411),
                      LiSg = c(0.168, 0.078, 0.295),
                      NaRe = c(1.0255, 0.2008, 0.5482) * StockNa.600_2 /
                        c(6.1086, 6.1369, 6.0411))
BlindeP$LiIn <- signal2conc(signal = BlindeP$LiSg, model = CalModels$Lithium.P, planar = TRUE,
                            Conc.S = BlindeP$NaRe)
plot(x = BlindeP$LiRe, y = BlindeP$LiIn)
abline(a = 0, b = 1, col = 2, lty = 3)
abline(lm(BlindeP$LiIn ~ BlindeP$LiRe))
summary(lm(BlindeP$LiIn ~ BlindeP$LiRe))

t.test(x = BlindeP$LiIn, y = BlindeP$LiRe, paired = TRUE)

#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- list (
  T.15.1 = c(0, 1, 2, 3, 4, 5, 20.5),
  T.15.2 = c(0, 1, 2, 3, 4, 5),
  T.15.3 = c(0, 1, 2, 3, 4, 5),
  T.15.4 = c(0, 1, 2, 3, 4.25, 5),
  T.15.5 = c(0, 1, 2, 3, 4, 5, 19.25),
  T.15.6 = c(0, 1, 2, 3.25, 4, 5),
  T.15.7 = c(0, 1, 2, 3, 4, 5),
  T.15.8 = c(0, 1, 2, 3, 4, 5)
)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
dilutions <- list(
  Feed.15.1  = c(2.0016/0.0497, 1.9762/0.0494, 1.9970/0.0490, 2.0066/0.0495),
  Strip.15.1 = c(1.0327/0.2210, 1.0385/0.2228, 2.0279/0.2194, 1.0324/0.2221),
  Feed.15.2  = c(2.0213/0.0489, 1.9685/0.0502, 1.9675/0.0495),
  Strip.15.2 = c(1.0337/0.2212, 1.0386/0.2232, 1.0349/0.2206),
  Feed.15.3  = c(1.9936/0.0501, 2.0085/0.0502, 2.0056/0.0495),
  Strip.15.3 = c(1.0384/0.2214, 1.0450/0.2224, 1.0293/0.2214),
  Feed.15.4  = c(1.9921/0.0498, 1.9906/0.0491, 1.9881/0.0496),
  Strip.15.4 = c(1.0398/0.2229, 1.0393/0.2234, 1.0418/0.2210),
  Feed.15.5  = c(1.9893/0.0493, 1.9837/0.0494, 1.9782/0.0496, 1.9881/0.0495),
  Strip.15.5 = c(1.0402/0.2216, 1.0355/0.2231, 1.0268/0.2197, 1.0132/0.2214),
  Feed.15.6  = c(1.9976/0.0495, 1.9892/0.0495, 1.9806/0.0496),
  Strip.15.6 = c(1.0363/0.2227, 1.0412/0.2237, 1.0177/0.2204),
  Feed.15.7  = c(1.9927/0.0499, 1.9853/0.0496, 1.9926/0.0496),
  Strip.15.7 = c(1.0366/0.2220, 1.0391/0.2235, 1.0306/0.2198),
  Feed.15.8  = c(1.9970/0.0601, 2.0382/0.0493, 2.0122/0.0487),
  Strip.15.8 = c(1.0393/0.2287, 1.0390/0.2221, 1.0310/0.2204)
)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(
  Feed.15.1.Li   = c(0.379, 0.297, 0.251, 0.204, 0.175, 0.150, 0.092),
  Strip.15.1.Li  = c(0.000, 0.071, 0.120, 0.168, 0.200, 0.226, 0.286),
  Feed.15.1.Na   = c(0.320, 0.322, 0.317, 0.318),
  Strip.15.1.Na  = c(0.002, 0.034, 0.012, 0.019),
  Feed.15.2.Li   = c(0.371, 0.312, 0.278, 0.239, 0.223, 0.208),
  Strip.15.2.Li  = c(0.000, 0.058, 0.103, 0.137, 0.157, 0.171),
  Feed.15.2.Na   = c(0.311, 0.246, 0.180),
  Strip.15.2.Na  = c(0.009, 0.660, 1.004),
  Feed.15.3.Li   = c(0.374, 0.274, 0.205, 0.152, 0.111, 0.092),
  Strip.15.3.Li  = c(0.000, 0.093, 0.161, 0.211, 0.252, 0.266),
  Feed.15.3.Na   = c(0.318, 0.320, 0.320),
  Strip.15.3.Na  = c(0.000, 0.004, 0.025),
  Feed.15.4.Li   = c(0.376, 0.284, 0.226, 0.174, 0.136, 0.111),
  Strip.15.4.Li  = c(0.000, 0.073, 0.133, 0.181, 0.220, 0.247),
  Feed.15.4.Na   = c(0.314, 0.316, 0.322),
  Strip.15.4.Na  = c(0.000, 0.017, 0.019),
  Feed.15.5.Li   = c(0.374, 0.265, 0.182, 0.132, 0.097, 0.072, 0.009),
  Strip.15.5.Li  = c(0.000, 0.104, 0.179, 0.228, 0.259, 0.281, 0.339),
  Feed.15.5.Na   = c(0.313, 0.313, 0.322, 0.315),
  Strip.15.5.Na  = c(0.001, 0.023, 0.023, 0.045),
  Feed.15.6.Li   = c(0.373, 0.233, 0.168, 0.093, 0.069, 0.051),
  Strip.15.6.Li  = c(0.000, 0.135, 0.213, 0.270, 0.295, 0.312),
  Feed.15.6.Na   = c(0.322, 0.335, 0.325),
  Strip.15.6.Na  = c(0.008, 0.018, 0.061),
  Feed.15.7.Li   = c(0.369, 0.314, 0.266, 0.232, 0.203, 0.184),
  Strip.15.7.Li  = c(0.000, 0.046, 0.096, 0.131, 0.160, 0.181),
  Feed.15.7.Na   = c(0.318, 0.322, 0.316),
  Strip.15.7.Na  = c(0.003, 0.005, 0.014),
  Feed.15.8.Li   = c(0.372, 0.310, 0.262, 0.219, 0.189, 0.161),
  Strip.15.8.Li  = c(0.000, 0.053, 0.098, 0.137, 0.161, 0.193),
  Feed.15.8.Na   = c(0.383, 0.314, 0.305),
  Strip.15.8.Na  = c(0.000, 0.010, 0.010)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- list()
for (i in 1:8) {
  if (any(i == c(1, 5))) {ts <- c(1, 3, 6, 7)} else {ts <- c(1, 3, 6)}
  eval(parse(text = paste0("AliConc$Feed.15.", i, ".Na <- signal2conc(signal = AliAbs$Feed.15.", i, ".Na,
                            model = CalModels$Sodium.1, dilution = dilutions$Feed.15.", i, ")")))
  eval(parse(text = paste0("AliConc$Strip.15.", i, ".Na <- signal2conc(signal = AliAbs$Strip.15.", i, ".Na,
                            model = CalModels$Sodium.1, dilution = dilutions$Strip.15.", i, ")")))
  eval(parse(text = paste0("AliConc$Feed.15.", i, ".Li <- signal2conc(signal = AliAbs$Feed.15.", i, ".Li,
                            model = CalModels$Lithium.P, planar = TRUE,
                            Conc.S = fixSecondary(metalConc = AliConc$Feed.15.", i, ".Na,
                                                  time = AliTimes$T.15.", i, "[ts],
                                                  compTime = AliTimes$T.15.", i, ", order = 2))")))
  eval(parse(text = paste0("AliConc$Strip.15.", i, ".Li <- signal2conc(signal = AliAbs$Strip.15.", i, ".Li,
                            model = CalModels$Lithium.P, planar = TRUE,
                            Conc.S = fixSecondary(metalConc = AliConc$Strip.15.", i, ".Na,
                                                  time = AliTimes$T.15.", i, "[ts],
                                                  compTime = AliTimes$T.15.", i, ", order = 2))")))
}
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- list()
for (i in 1:8) {
  if (any(i == c(1, 5))) {ts <- c(1, 3, 6, 7)} else {ts <- c(1, 3, 6)}
  eval(parse(text = paste0("TransFrac$M.15.", i, ".Li <- conc2frac(feed = AliConc$Feed.15.", i, ".Li[1:6],
                            strip = AliConc$Strip.15.", i, ".Li[1:6], time = AliTimes$T.15.", i, "[1:6])")))
  eval(parse(text = paste0("TransFrac$M.15.", i, ".Na <- conc2frac(feed = AliConc$Feed.15.", i, ".Na[1:4],
                            strip = AliConc$Strip.15.", i, ".Na[1:4], time = AliTimes$T.15.", i, "[ts][1:4])")))
  #eval(parse(text = paste0("TransFrac$M.15.", i, ".Na$Fraction[which(TransFrac$M.15.", i, ".Na$Phase == 'Strip')] <-
  #  1 - TransFrac$M.15.", i, ".Na$Fraction[which(TransFrac$M.15.", i, ".Na$Phase == 'Feed')]")))
}
#rm(sub.Na)
#-----MODELOS DE REGRESIÓN NO LINEAL-----------------------------------------
TransNLS <- list()
SS_par <- vector()
for (i in 1:8) {
  eval(parse(text = paste0("X <- transTrend(TransFrac$M.15.", i, ".Li, model = 'paredes', eccen = 1)")))
  SS_par <- c(SS_par, sum(resid(X$feed)^2), sum(resid(X$strip)^2))
  eval(parse(text = paste0("TransNLS$M.15.", i, " <- X")))

}

TransNLSXot <- list()
SS_xot <- vector()
for (i in 1:8) {
  eval(parse(text = paste0("X <- transTrend(TransFrac$M.15.", i, ".Li, model = 'rodriguez')")))
  SS_xot <- c(SS_xot, sum(resid(X$feed)^2), sum(resid(X$strip)^2))
    eval(parse(text = paste0("TransNLSXot$M.15.", i, " <- X")))
}
t.test(x = SS_par, y = SS_xot, paired = TRUE)
plot(SS_par, SS_xot)
abline(lm(SS_xot~SS_par))
lm(SS_xot~SS_par)
#-----FACTORES DE SEPARACIÓN-------------------------------------------------
order = c(1, 8, 2, 7, 3, 6, 4, 5)
sepFactor <- list()
for (i in order) {
  eval(parse(text = paste0("X <- data.frame(time = AliTimes$T.15.", i, "[1:6],
                                   factor = (AliConc$Strip.15.", i, ".Li[1:6] /
                                     fixSecondary(metalConc = AliConc$Strip.15.", i, ".Na[1:3],
                                                  time = AliTimes$T.15.", i, "[c(1, 3, 5)],
                                                  compTime = AliTimes$T.15.", i, "[1:6], order = 2)) /
                                            (AliConc$Feed.15.", i, ".Li[1]/AliConc$Feed.15.", i, ".Na[1]))")))
  
  X <- X[-1, ]
  eval(parse(text = paste0("sepFactor$M.15.", i, " <- X")))
}
ssepFactor <- data.frame()

for (i in 1:8) ssepFactor <- rbind(ssepFactor, sepFactor[[i]])

ssepFactor$Membrana <- as.factor(paste0('Mem.15.', rep(order, each = 5)))
ggplot(data = ssepFactor, aes(x = time, y = factor, colour = Membrana)) + geom_point() + theme_bw() +
  ggsci::scale_color_npg() + stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, size = 0.4) +
  xlab(label = "Tiempo (horas)") + ylab(label = "Factor de separación")

sF <- vector()
for (i in 1:8) sF <- c(sF, mean(sepFactor[[i]][, 2]))


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
#-----PERFILES DE TRANSPORTE ------------------------------------------------

Parameters <- data.frame()

for (i in order) {
# invisible(readline(prompt="Press [enter] to continue"))
  eval(parse(text = paste0("transPlot(trans = TransFrac$M.15.", i, ".Li, trend = TransNLS$M.15.", i, ",
                             secondary = TransFrac$M.15.", i, ".Na, lin.secon = TRUE,
                             xlim = c(0, 5.2), ylim = c(-0.05, 1.08),
                             ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:5)")))
  eval(parse(text = paste0("Parameters <- rbind(Parameters, c(TransNLS$M.15.", i, "$Result, sF[i],
                           TransFrac$M.15.", i, ".Li[12, 3], TransFrac$M.15.", i, ".Li[11, 3],
                           TransFrac$M.15.", i, ".Li[10, 3], TransFrac$M.15.", i, ".Li[9, 3],
                           TransFrac$M.15.", i, ".Li[8, 3]))")))
}
#for (i in 1:7) {
#  eval(parse(text = paste0("transPlot(trans = TransFrac$M.15.", i, ".Li, trend = TransNLS$M.15.", i, ",
#                             xlim = c(0, 26.1), ylim = c(-0.05, 1.08),
#                             ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = c(0, 4, 8, 12, 16, 20, 24))")))
#}
colnames(Parameters) <- c(names(TransNLS$M.15.1$Result), "sF")
round(Parameters, 3)


#-----REPETEABILITY BETWEEN RUNS---------------------------------------------
vel <- c("Exp 1", "Exp 2", "Exp 3", "Exp 4", "Exp 4", "Exp 3", "Exp 2", "Exp 1")
frac <- vector()
for (i in 1:8) frac <- c(frac, TransFrac[[2*i-1]]$Fraction[7:12])
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
  eval(parse(text = paste0("X <- data.frame(time = AliTimes$T.15.", i, "[1:6],
                           factor = (AliConc$Strip.15.", i, ".Li[1:6] /
                           fixSecondary(metalConc = AliConc$Strip.15.", i, ".Na[1:3],
                           time = AliTimes$T.15.", i, "[c(1, 3, 6)],
                           compTime = AliTimes$T.15.", i, "[1:6], order = 2)) /
                           (AliConc$Feed.15.", i, ".Li[1]/AliConc$Feed.15.", i, ".Na[1]))")))

  X <- X[-1, ]
  eval(parse(text = paste0("sepFactor$M.15.", i, " <- X")))
}
ssepFactor <- data.frame()

for (i in 1:8) ssepFactor <- rbind(ssepFactor, sepFactor[[i]])

ssepFactor$Membrana <- as.factor(paste0('Mem.15.', rep(1:8, each = 5)))
ggplot(data = ssepFactor, aes(x = time, y = factor, colour = Membrana)) + geom_point() + theme_bw() +
  ggsci::scale_color_npg() + stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, size = 0.4) +
  xlab(label = "Tiempo (horas)") + ylab(label = "Factor de separación")

sF <- vector()
for (i in 1:8) sF <- c(sF, mean(sepFactor[[i]][, 2]))

if (PDF) dev.off()

# TEMPERATURE----------------------------------------------------
Temp <- data.frame(Mem.15.1 = mean(c(68, 72, 73, 72, 72)), 
             Mem.15.2 = mean(c(68, 68, 66, 66, 64)),
             Mem.15.3 = mean(c(61, 59, 59, 59, 61)),
             Mem.15.4 = mean(c(68, 70, 72, 75, 73)),
             Mem.15.5 = mean(c(72, 70, 68, 68, 68)),
             Mem.15.6 = mean(c(68, 72, 75, 75, 77)),
             Mem.15.7 = mean(c(70, 70, 66, 66, 66)),
             Mem.15.8 = mean(c(70, 72, 73, 75, 77)))
Temp[order]
Parameters
