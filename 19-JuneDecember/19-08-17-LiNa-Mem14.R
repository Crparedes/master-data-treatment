library(ggplot2)
library(transmem)
PDF <- FALSE
#-----STOCK SOLUTIONS--------------------------------------------------------
StockLi.200 <- 129.5 * 0.187872 * 0.99 / 0.1200962
StockNa.11000 <- 1.1693 * 0.996 /41.5065 * 0.393372 * 1000000

StockLi.5_4   <- StockLi.200 * 1.2447 / 50.0609
StockLi.5_5   <- StockLi.200 * 1.2621 / 50.0062

StockNa.600_1 <- StockNa.11000  * 1.6505 / 30.1080
StockNa.10_1  <- StockNa.600_1  * 0.6065 / 30.0068
StockNa.200_3 <- StockNa.11000  * 0.3699 / 20.1313
StockNa.10_2  <- StockNa.200_3  * 0.7736 / 15.5600
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CalCurves <- list(
  Lithium.P = data.frame(Conc = c(0.0000, 0.1220, 0.5995, 2.4008, 0.0623, 0.3031, 1.2507, 2.7362, 0.0000, 0.1253,
                                  0.6163, 2.4954, 0.0625, 0.3036, 1.2362, 2.7323, 0.0000, 0.1260, 0.6249, 2.4205,
                                  0.0596, 0.3088, 1.2048, 2.7345) *
                                StockLi.5_4 / c(6.0000, 6.0991, 6.1728, 6.0965, 6.0264, 6.0103, 6.0094, 6.0192,
                                                6.0388, 6.1271, 6.1162, 6.0195, 6.1010, 6.0030, 6.1259, 6.0507,
                                                6.1173, 6.0616, 6.1872, 6.0193, 6.0224, 6.1524, 6.1101, 6.0337),
                         Signal = c(0.000, 0.012, 0.066, 0.268, 0.007, 0.035, 0.145, 0.330, 0.000, 0.015, 0.073,
                                    0.298, 0.007, 0.037, 0.145, 0.313, 0.000, 0.015, 0.072, 0.280, 0.007, 0.037,
                                    0.139, 0.311),
                         Conc.S = c(0.0000, 0.0000, 0.0000, 0.0000, 0.2621, 0.2507, 0.2686, 0.2700, 0.5049, 0.5253,
                                    0.5298, 0.5256, 0.8370, 0.7558, 0.8183, 0.8020, 1.0036, 1.0174, 1.0144, 1.0304,
                                    2.0503, 2.0534, 2.0552, 2.0457) *
                                  StockNa.600_1 / c(6.0000, 6.0991, 6.1728, 6.0965, 6.0264, 6.0103, 6.0094, 6.0192,
                                                    6.0388, 6.1271, 6.1162, 6.0195, 6.1010, 6.0030, 6.1259, 6.0507,
                                                    6.1173, 6.0616, 6.1872, 6.0193, 6.0224, 6.1524, 6.1101, 6.0337)),
  Sodium.1 = data.frame(Conc = c(0.0000, 0.0637, 0.1232, 0.3032, 0.5969, 1.5201, 3.0023, 4.2438) *
                               StockNa.10_2 / c(6.0000, 6.0237, 6.0394, 6.1137, 6.1714, 6.1931, 6.0364, 6.0162),
                        Signal  = c(0.000, 0.014, 0.026, 0.067, 0.135, 0.329, 0.630, 0.861))
)
## for a cleaner workspace
rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
CalModels <- list(
  Lithium.P = calibPlane(plane = CalCurves$Lithium.P[1:24, ]),
  Sodium.1 = calibCurve(curve = CalCurves$Sodium.1, order = 2)
)
anova(CalModels$Lithium.P$model)
summary(CalModels$Lithium.P$model)
#-----MUESTRAS CIEGAS--------------------------------------------------------
#BlindeP <- data.frame(LiRe = c(0.7889, 0.7626, 0.7800, 1.5533) * StockLi.5_5 /
#                        c(6.2458, 6.0504, 6.3987, 6.0039),
#                      LiSg = c(0.088, 0.083, 0.085, 0.168),
#                      NaRe = c(0.7711, 0.0000, 0.7745, 0.0000) * StockNa.200_3 /
#                        c(6.2458, 6.0504, 6.3987, 6.0039))
#BlindeP$LiIn <- signal2conc(signal = BlindeP$LiSg, model = CalModels$Lithium.P, planar = TRUE,
#                            Conc.S = BlindeP$NaRe)
#plot(x = BlindeP$LiRe, y = BlindeP$LiIn)
#abline(a = 0, b = 1, col = 2, lty = 3)
#abline(lm(BlindeP$LiIn ~ BlindeP$LiRe))
#summary(lm(BlindeP$LiIn ~ BlindeP$LiRe))
## NO HAY EFECTO MATRIZ POR EL ÁCIDO CLORHÍDRICO O EL HIDRÓXIDO DE AMONIO:
#t.test(x = BlindeP$LiIn, y = BlindeP$LiRe, paired = TRUE)

#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- list (
  T.14.1 = c(0, 1, 2, 3, 4, 5),
  T.14.2 = c(0, 1, 2, 3, 4, 5, 23),
  T.14.3 = c(0, 1, 2, 3, 4, 5),
  T.14.4 = c(0, 1, 2, 3, 4, 5),
  T.14.5 = c(0, 1, 2, 3, 4, 5, 19),
  T.14.6 = c(0, 1, 2, 3, 4, 5, 24),
  T.14.7 = c(0, 1, 2, 3, 4, 5, 18.5)
)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
dilutions <- list(
  Feed.14.1  = c(6.0894/0.0608, 6.0831/0.0618, 6.0918/0.0598, 6.0127/0.0579),
  Strip.14.1 = c(1.5383/0.2209, 1.5524/0.2248, 1.5590/0.2453, 1.4174/0.2448),
  Feed.14.2  = c(6.1094/0.0597, 6.1146/0.0598, 6.0711/0.0597, 6.0840/0.0592, 6.0808/0.0590),
  Strip.14.2 = c(1.5350/0.2181, 1.5483/0.2264, 1.5388/0.2240, 1.5448/0.2232, 1.5505/0.2231),
  Feed.14.3  = c(6.0758/0.0601, 6.0475/0.0591, 6.0876/0.0591, 6.0863/0.0569),
  Strip.14.3 = c(1.5403/0.2226, 1.5373/0.2245, 1.5455/0.2236, 1.5461/0.2237),
  Feed.14.4  = c(6.0657/0.0599, 6.0879/0.0597, 6.0872/0.0588, 6.0653/0.0596),
  Strip.14.4 = c(1.5390/0.2222, 1.5386/0.2215, 1.5481/0.2266, 1.5373/0.2249),
  Feed.14.5  = c(6.0624/0.0602, 6.0728/0.0589, 6.0774/0.0594, 6.1014/0.0601, 6.0639/0.0592),
  Strip.14.5 = c(1.5371/0.2226, 1.5367/0.2237, 1.5408/0.2248, 1.5475/0.2244, 1.5387/0.2241),
  Feed.14.6  = c(6.0831/0.0597, 6.0787/0.0598, 6.0782/0.0596, 6.0667/0.0598, 6.0998/0.0597),
  Strip.14.6 = c(1.5408/0.2208, 1.5391/0.2237, 1.5374/0.2247, 1.5308/0.2233, 1.5432/0.2235),
  Feed.14.7  = c(6.0551/0.0607, 6.0728/0.0589, 6.0546/0.0603, 6.0563/0.0594, 6.0334/0.0604),
  Strip.14.7 = c(1.5261/0.2135, 1.5413/0.2246, 1.5419/0.2239, 1.5411/0.2257, 1.5378/0.2256*1.4608/0.1449)
)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(
  Feed.14.1.Li   = c(0.273, 0.198, 0.147, 0.117, 0.093, 0.076),
  Strip.14.1.Li  = c(0.001, 0.060, 0.105, 0.136, 0.157, 0.174),
  Feed.14.1.Na   = c(0.191, 0.198, 0.192, 0.187),
  Strip.14.1.Na  = c(0.019, 0.044, 0.084, 0.075),
  Feed.14.2.Li   = c(0.265, 0.207, 0.160, 0.135, 0.110, 0.095, 0.029),
  Strip.14.2.Li  = c(0.000, 0.052, 0.092, 0.120, 0.143, 0.155, 0.221),
  Feed.14.2.Na   = c(0.199, 0.196, 0.196, 0.194, 0.196),
  Strip.14.2.Na  = c(0.017, 0.024, 0.034, 0.034, 0.052),
  Feed.14.3.Li   = c(0.268, 0.222, 0.183, 0.157, 0.135, 0.115),
  Strip.14.3.Li  = c(0.000, 0.041, 0.083, 0.114, 0.139, 0.158),
  Feed.14.3.Na   = c(0.176, 0.175, 0.173, 0.166),
  Strip.14.3.Na  = c(0.020, 0.028, 0.038, 0.045),
  Feed.14.4.Li   = c(0.267, 0.208, 0.167, 0.138, 0.114, 0.095),
  Strip.14.4.Li  = c(0.000, 0.049, 0.088, 0.117, 0.138, 0.158),
  Feed.14.4.Na   = c(0.204, 0.200, 0.203, 0.203),
  Strip.14.4.Na  = c(0.024, 0.029, 0.038, 0.043),
  Feed.14.5.Li   = c(0.265, 0.196, 0.150, 0.116, 0.095, 0.078, 0.026),
  Strip.14.5.Li  = c(0.000, 0.062, 0.107, 0.135, 0.157, 0.169, 0.224),
  Feed.14.5.Na   = c(0.208, 0.204, 0.203, 0.200, 0.201),
  Strip.14.5.Na  = c(0.019, 0.035, 0.044, 0.046, 0.022),
  Feed.14.6.Li   = c(0.270, 0.193, 0.148, 0.105, 0.093, 0.077, 0.041),
  Strip.14.6.Li  = c(0.000, 0.065, 0.109, 0.143, 0.159, 0.173, 0.217),
  Feed.14.6.Na   = c(0.209, 0.204, 0.209, 0.206, 0.204),
  Strip.14.6.Na  = c(0.025, 0.040, 0.054, 0.058, 0.071),
  Feed.14.7.Li   = c(0.263, 0.217, 0.196, 0.190, 0.182, 0.177, 0.139),
  Strip.14.7.Li  = c(0.001, 0.048, 0.068, 0.077, 0.083, 0.093, 0.134),
  Feed.14.7.Na   = c(0.200, 0.178, 0.173, 0.162, 0.117),
  Strip.14.7.Na  = c(0.033, 0.266, 0.474, 0.593, 0.133)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- list()
for (i in 1:6) {
  if (any(i == c(1, 3, 4))) {ts <- c(1, 3, 5, 6)} else {ts <- c(1, 3, 5, 6, 7)}
  eval(parse(text = paste0("AliConc$Feed.14.", i, ".Na <- signal2conc(signal = AliAbs$Feed.14.", i, ".Na,
                            model = CalModels$Sodium.1, dilution = dilutions$Feed.14.", i, ")")))
  eval(parse(text = paste0("AliConc$Strip.14.", i, ".Na <- signal2conc(signal = AliAbs$Strip.14.", i, ".Na,
                            model = CalModels$Sodium.1, dilution = dilutions$Strip.14.", i, ")")))
  eval(parse(text = paste0("AliConc$Feed.14.", i, ".Li <- signal2conc(signal = AliAbs$Feed.14.", i, ".Li,
                            model = CalModels$Lithium.P, planar = TRUE,
                            Conc.S = fixSecondary(conc = AliConc$Feed.14.", i, ".Na,
                                                  time = AliTimes$T.14.", i, "[ts],
                                                  compTime = AliTimes$T.14.", i, ", order = 2))")))
  eval(parse(text = paste0("AliConc$Strip.14.", i, ".Li <- signal2conc(signal = AliAbs$Strip.14.", i, ".Li,
                            model = CalModels$Lithium.P, planar = TRUE,
                            Conc.S = fixSecondary(conc = AliConc$Strip.14.", i, ".Na,
                                                  time = AliTimes$T.14.", i, "[ts],
                                                  compTime = AliTimes$T.14.", i, ", order = 2))")))
}
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- list()
for (i in 1:6) {
  if (any(i == c(1, 3, 4))) {ts <- c(1, 3, 5, 6)} else {ts <- c(1, 3, 5, 6, 7)}
  eval(parse(text = paste0("TransFrac$M.14.", i, ".Li <- conc2frac(feed = AliConc$Feed.14.", i, ".Li[1:6],
                            strip = AliConc$Strip.14.", i, ".Li[1:6], time = AliTimes$T.14.", i, "[1:6])")))
  eval(parse(text = paste0("TransFrac$M.14.", i, ".Na <- conc2frac(feed = AliConc$Feed.14.", i, ".Na[1:4],
                            strip = AliConc$Strip.14.", i, ".Na[1:4], time = AliTimes$T.14.", i, "[ts][1:4])")))
  #eval(parse(text = paste0("TransFrac$M.14.", i, ".Na$Fraction[which(TransFrac$M.14.", i, ".Na$Phase == 'Strip')] <-
  #  1 - TransFrac$M.14.", i, ".Na$Fraction[which(TransFrac$M.14.", i, ".Na$Phase == 'Feed')]")))
}
#rm(sub.Na)
#-----MODELOS DE REGRESIÓN NO LINEAL-----------------------------------------
TransNLS <- list()
SS_par <- vector()
for (i in 1:6) {
  eval(parse(text = paste0("X <- transTrend(TransFrac$M.14.", i, ".Li, model = 'paredes', eccen = 1)")))
  SS_par <- c(SS_par, sum(resid(X$feed)^2), sum(resid(X$strip)^2))
  eval(parse(text = paste0("TransNLS$M.14.", i, " <- X")))

}

#TransNLSXot <- list()
#SS_xot <- vector()
#for (i in 1:7) {
#  eval(parse(text = paste0("X <- transTrend(TransFrac$M.14.", i, ".Li, model = 'rodriguez')")))
#  SS_xot <- c(SS_xot, sum(resid(X$feed)^2), sum(resid(X$strip)^2))
#    eval(parse(text = paste0("TransNLSXot$M.14.", i, " <- X")))
#}
#t.test(x = SS_par, y = SS_xot, paired = TRUE)
#-----PERFILES DE TRANSPORTE ------------------------------------------------
#if (PDF) pdf("Perfiles19-08-17.pdf", height = 7/1.8, width = 9/1.8)

Parameters <- data.frame()
for (i in 1:6) {
# invisible(readline(prompt="Press [enter] to continue"))

  eval(parse(text = paste0("transPlot(trans = TransFrac$M.14.", i, ".Li, trend = TransNLS$M.14.", i, ",
                             secondary = TransFrac$M.14.", i, ".Na, xlim = c(0, 5.2), ylim = c(-0.05, 1.08),
                             ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:5)")))
  eval(parse(text = paste0("Parameters <- rbind(Parameters, TransNLS$M.14.", i, "$Result)")))
}
#for (i in 1:7) {
#  eval(parse(text = paste0("transPlot(trans = TransFrac$M.14.", i, ".Li, trend = TransNLS$M.14.", i, ",
#                             xlim = c(0, 26.1), ylim = c(-0.05, 1.08),
#                             ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = c(0, 4, 8, 12, 16, 20, 24))")))
#}
colnames(Parameters) <- names(TransNLS$M.14.1$Result)
round(Parameters, 3)

#-----FACTORES DE SEPARACIÓN-------------------------------------------------
sepFactor <- list()
for (i in 1:6) {
  eval(parse(text = paste0("X <- data.frame(time = AliTimes$T.14.", i, "[1:6],
                                   factor = (AliConc$Strip.14.", i, ".Li[1:6] /
                                     fixSecondary(conc = AliConc$Strip.14.", i, ".Na[1:4],
                                                  time = AliTimes$T.14.", i, "[c(1, 3, 4, 5)],
                                                  compTime = AliTimes$T.14.", i, "[1:6], order = 2)) /
                                            (AliConc$Feed.14.", i, ".Li[1]/AliConc$Feed.14.", i, ".Na[1]))")))

  X <- X[-1, ]
  eval(parse(text = paste0("sepFactor$M.14.", i, " <- X")))
}
ssepFactor <- data.frame()

#for (i in 1:6) ssepFactor <- rbind(ssepFactor, sepFactor[[i]])
for (i in 1:6) ssepFactor <- rbind(ssepFactor, rbind(c(0, 1), sepFactor[[i]]))


ssepFactor$Membrana <- as.factor(paste0('Mem.14.', rep(1:6, each = 6)))
ggplot(data = ssepFactor, aes(x = time, y = factor, colour = Membrana)) + geom_point() + theme_bw() +
  ggsci::scale_color_npg() + stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, size = 0.4) +
  xlab(label = "Tiempo (horas)") + ylab(label = "Factor de separación")

ssepFactor$PIM <- as.factor(paste0('H.', rep(1:6, each = 6)))

p_sf <- ggplot(data = ssepFactor, aes(x = time, y = factor, shape = PIM)) + 
  theme_bw() +
  stat_smooth(method = "loess", se = FALSE, size = 0.4, color = 'black', span = 0.9) +
  scale_shape_manual(values=c(24, 17, 17, 17, 24, 24)) + geom_point(size = 3, col = 'black', fill = 'white') +
  xlab(label = "Tiempo (h)") + ylab(label = "Factor de separación") + theme(legend.position = 'none')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))
p_sf

sF <- vector()
for (i in 1:6) sF <- c(sF, mean(sepFactor[[i]][, 2]))


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#-----AGITATION SPEED EFFECTS------------------------------------------------
vel <- c("a) 600-520", "b) 560-560", "c) 730-600", "d) 640-500", "e) 540-480", "f) 580-590")#, "g) 535-550")

vel <- c(600+520, 560+560, 730+600, 640+500, 540+480, 580+590)/2#535-550")

frac <- vector()
for (i in 1:6) frac <- c(frac, TransFrac[[2*i-1]]$Fraction[7:12])
velData <- data.frame(Agit. = as.factor(rep(vel, each = 6)), Phi_strip = frac, Time = rep(AliTimes[[1]], 6))

p <- ggplot(data = velData, aes(x = Time, y = Phi_strip, shape = Agit., group = Agit.)) +
  theme_bw() +
  labs(y = expression(Phi), x = 'Tiempo (h)') +
  scale_shape_manual(values = c(15, 16, 17, 21, 22, 23, 24)) + theme(legend.position = 'none') + ylim(-0.01, 0.7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))

p
cols = gg_color_hue(7)
cols <- rep('black', 6)
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
                  color = cols[5], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                  xlim = c(0, 5), size = 0.4) +
    stat_function(fun = function(x) ((coefficients(TransNLS[[6]]$strip)[1] * x^e)
                                     / (1 / coefficients(TransNLS[[6]]$strip)[2] + x^e)),
                  color = cols[6], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                  xlim = c(0, 5), size = 0.4))#+
(p <- p + geom_point(size = 3, color = 'black', fill = 'white', shape = rep(c(24, 17, 17, 17, 24, 24), each = 6)))

#    stat_function(fun = function(x) ((coefficients(TransNLS[[7]]$strip)[1] * x^e)
#                                     / (1 / coefficients(TransNLS[[7]]$strip)[2] + x^e)),
#                  color = cols[7], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
#                  xlim = c(0, 5), size = 0.4) +
#    geom_point(size = 3, shape = 15, color = rep(gg_color_hue(7), each = 6)))


if (PDF) dev.off()

#-----FACTORES DE SEPARACIÓN-------------------------------------------------
sepFactor <- list()
for (i in 1:6) {
  eval(parse(text = paste0("X <- data.frame(time = AliTimes$T.14.", i, "[1:6],
                           factor = (AliConc$Strip.14.", i, ".Li[1:6] /
                           fixSecondary(conc = AliConc$Strip.14.", i, ".Na[1:4],
                           time = AliTimes$T.14.", i, "[c(1, 3, 4, 5)],
                           compTime = AliTimes$T.14.", i, "[1:6], order = 2)) /
                           (AliConc$Feed.14.", i, ".Li[1]/AliConc$Feed.14.", i, ".Na[1]))")))

  X <- X[-1, ]
  eval(parse(text = paste0("sepFactor$M.14.", i, " <- X")))
}
ssepFactor <- data.frame()

for (i in 1:6) ssepFactor <- rbind(ssepFactor, sepFactor[[i]])

ssepFactor$Membrana <- as.factor(paste0('Mem.14.', rep(1:6, each = 5)))
ggplot(data = ssepFactor, aes(x = time, y = factor, colour = Membrana)) + geom_point() + theme_bw() +
  ggsci::scale_color_npg() + stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, size = 0.4) +
  xlab(label = "Tiempo (horas)") + ylab(label = "Factor de separación")

sF <- vector()
for (i in 1:6) sF <- c(sF, mean(sepFactor[[i]][, 2]))


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#-----AGITATION SPEED EFFECTS------------------------------------------------
caja <- c("plana", "rugosa", "rugosa", "plana", "plana", "rugosa")
frac <- vector()
for (i in 1:6) frac <- c(frac, TransFrac[[2*i-1]]$Fraction[7:12])
velData <- data.frame(Agit. = as.factor(rep(caja, each = 6)), Phi_strip = frac, Time = rep(AliTimes[[1]], 6))

p1 <- ggplot(data = velData, aes(x = Time, y = Phi_strip, group = Agit., color = Agit.)) +
  theme_bw() + geom_point(size = 3, shape = 15)  +
  labs(y = expression(Phi), x = 'Time (h)') +
  theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(color = "black"),
        axis.text.y = ggplot2::element_text(color = "black"))


cols = gg_color_hue(7)
e <- TransNLS[[1]]$eccen

(p1 <- p1 + stat_function(fun = function(x) ((coefficients(TransNLS[[1]]$strip)[1] * x^e)
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
                  color = cols[5], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                  xlim = c(0, 5), size = 0.4) +
    stat_function(fun = function(x) ((coefficients(TransNLS[[6]]$strip)[1] * x^e)
                                     / (1 / coefficients(TransNLS[[6]]$strip)[2] + x^e)),
                  color = cols[6], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                  xlim = c(0, 5), size = 0.4) +
    #stat_function(fun = function(x) ((coefficients(TransNLS[[7]]$strip)[1] * x^e)
    #                                 / (1 / coefficients(TransNLS[[7]]$strip)[2] + x^e)),
    #              color = cols[7], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
    #              xlim = c(0, 5), size = 0.4) +
    geom_point(size = 3, shape = 15, color = rep(gg_color_hue(6), each = 6)))


if (PDF) dev.off()
ConsolidLi <- matrix(nrow = 12, ncol = 7)
ConsolidNa <- matrix(nrow = 8, ncol = 7)
for (i in 1:6) {
  ConsolidLi[, i] <- TransFrac[[(2*i-1)]][, 3]
  ConsolidNa[, i] <- TransFrac[[(2*i)]][, 3]
}

TransFrac[[3]]

