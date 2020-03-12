library(transmem)
library(ggplot2)
PDF <- FALSE
if (PDF) pdf("Reuse.pdf", height = 70/25.4, width = 90/25.4)

#-----STOCK SOLUTIONS--------------------------------------------------------
StockLi.200_2 <- 130.3 * 0.187872 * 0.99 / 0.1205105
StockLi.5_8   <- StockLi.200_2 * 0.2646 / 10.3841
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CalCurves <- list(
  Lithium.1 = data.frame(Conc = c(0.0000, 0.0685, 0.2891, 0.9453, 1.1944, 1.6853, 2.2121, 2.7284) *
                                StockLi.5_8 / c(6.0000, 6.6725, 6.3378, 6.6590, 6.6845, 6.6557, 6.8208, 6.6317),
                         Signal = c(0.000, 0.036, 0.162, 0.477, 0.586, 0.797, 0.971, 1.169)),
  Lithium.2 = data.frame(Conc = c(0.0000, 0.0597, 0.2989, 0.9115, 1.2053, 1.6816, 2.1670, 2.6474) *
                           StockLi.5_8 / c(6.0000, 6.1144, 6.0524, 6.0355, 6.1449, 6.0859, 6.0838, 6.0795),
                         Signal = c(0.000, 0.037, 0.168, 0.497, 0.618, 0.839, 1.032, 1.241)),
  Lithium.3 = data.frame(Conc = c(0.0000, 0.0090, 0.0221, 0.0615, 0.2714) *
                           StockLi.5_8 / c(6.0000, 6.2369, 6.3507, 6.1755, 6.1861),
                         Signal = c(0.000, 0.034, 0.078, 0.224, 1.031))

)
## for a cleaner workspace
#rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
CalModels <- list()
for (i in 1:3) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = 2, plot = TRUE)
names(CalModels) <- names(CalCurves)
summary(CalModels$Lithium.2)
summary(CalModels$Lithium.1)
#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- c(0, 1.5, 3, 4.75, 6, 6.01, 7.5, 9, 10.5, 12, 12.01, 13.5, 15.25,
              16.5, 18, 18.01, 19.5, 21, 22.5, 24, 24.01, 25.5, 27, 28.5, 30,
              30.01, 31.5, 33, 34.5, 36, 36.01, 37.5, 39, 40.5, 42, 42.01,
              43.5, 45, 46.5, 48, 48.01, 49.5, 51, 52.5, 54, 54.01, 55.5, 57, 60)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliEm <- list(
  Cycle.1.F.a = c(1.070, 0.564, 0.291, 0.157, 0.087),
  Cycle.1.S.a = c(0.000, 0.691, 0.937, 1.036, 1.061),
  Cycle.2.F.a = c(1.151, 0.736, 0.480, 0.326, 0.226),
  Cycle.2.S.a = c(0.004, 0.580, 0.851, 0.974, 1.049),
  Cycle.3.F.a = c(1.178, 0.749, 0.564, 0.402, 0.303),
  Cycle.3.S.a = c(0.004, 0.614, 0.814, 0.893, 0.969),
  Cycle.4.F.a = c(1.113, 0.807, 0.600, 0.455, 0.362),
  Cycle.4.S.a = c(0.002, 0.470, 0.717, 0.866, 0.945),
  Cycle.5.F.a = c(1.192, 0.892, 0.698, 0.500, 0.456),
  Cycle.5.S.a = c(0.005, 0.433, 0.658, 0.800, 0.882),
  Cycle.6.F.a = c(1.216, 0.982, 0.835, 0.715, 0.626),
  Cycle.6.S.a = c(0.001, 0.364, 0.569, 0.696, 0.776),
  Cycle.7.F.a = c(1.166, 0.976, 0.863, 0.782, 0.711),
  Cycle.7.S.a = c(0.002, 0.322, 0.487, 0.594, 0.675),
  Cycle.8.F.a = c(1.230, 1.042, 0.913, 0.829, 0.763),
  Cycle.8.S.a = c(0.003, 0.310, 0.480, 0.580, 0.649),
  Cycle.9.F.a = c(1.255, 1.088, 0.988, 0.923, 0.881),
  Cycle.9.S.a = c(0.004, 0.275, 0.423, 0.500, 0.557),
  Cycle.10.F.a = c(1.249, 1.134, 1.067, 1.020, 0.986),
  Cycle.10.S.a = c(0.004, 0.210, 0.310, 0.383, 0.426),

  Cycle.1.F.b = c(1.115, 0.622, 0.342, 0.194, 0.116),
  Cycle.1.S.b = c(0.000, 0.636, 0.898, 1.021, 1.078),
  Cycle.2.F.b = c(1.138, 0.751, 0.482, 0.321, 0.217),
  Cycle.2.S.b = c(0.006, 0.578, 0.852, 0.983, 1.059),
  Cycle.3.F.b = c(1.142, 0.713, 0.518, 0.361, 0.264),
  Cycle.3.S.b = c(0.006, 0.602, 0.802, 0.938, 1.015),
  Cycle.4.F.b = c(1.167, 0.809, 0.573, 0.402, 0.300),
  Cycle.4.S.b = c(0.006, 0.499, 0.757, 0.908, 0.990),
  Cycle.5.F.b = c(1.136, 0.850, 0.609, 0.400, 0.335),
  Cycle.5.S.b = c(0.008, 0.541, 0.793, 0.900, 1.046),
  Cycle.6.F.b = c(1.215, 0.921, 0.693, 0.535, 0.417),
  Cycle.6.S.b = c(0.003, 0.451, 0.706, 0.856, 0.963),
  Cycle.7.F.b = c(1.209, 0.923, 0.726, 0.582, 0.470),
  Cycle.7.S.b = c(0.004, 0.440, 0.687, 0.830, 0.935),
  Cycle.8.F.b = c(1.234, 0.979, 0.789, 0.658, 0.553),
  Cycle.8.S.b = c(0.005, 0.402, 0.630, 0.764, 0.863),
  Cycle.9.F.b = c(1.249, 1.014, 0.814, 0.682, 0.587),
  Cycle.9.S.b = c(0.008, 0.377, 0.621, 0.758, 0.846),
  Cycle.10.F.b = c(1.248, 1.028, 0.874, 0.738, 0.656),
  Cycle.10.S.b = c(0.007, 0.355, 0.565, 0.714, 0.792)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- vector(mode = "list", length = length(AliEm))
names(AliConc) <- names(AliEm)
for (i in 1:length(AliEm)) {
  if(i %in% c(1:12, 21:32)) {
    AliConc[[i]] <- signal2conc(signal = AliEm[[i]], model = CalModels$Lithium.1)
  } else {
    AliConc[[i]] <- signal2conc(signal = AliEm[[i]], model = CalModels$Lithium.2)
  }
}
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- vector(mode = "list", length = length(AliEm) / 2)
names(TransFrac) <- names(AliEm)[seq(1, 40, 2)]
for (i in 1:20) {
  TransFrac[[i]] <- conc2frac(feed = AliConc[[2 * i - 1]], strip = AliConc[[2 * i]],
                              time = c(0, 1.5, 3, 4.5, 6))
}
#-----MODELOS DE REGRESIÓN NO LINEAL-----------------------------------------
TransNLSXot <- TransNLS <- vector(mode = "list", length = length(TransFrac))
names(TransNLSXot) <- names(TransNLS) <- names(TransFrac)
SS_xot <- SS_par <- vector()
for (i in 1:20) {
  TransNLS[[i]] <- X <- transTrend(TransFrac[[i]], model = 'paredes', eccen = 1)
  SS_par <- c(SS_par, sum(resid(X$feed)^2), sum(resid(X$strip)^2))

  TransNLSXot[[i]] <- X <- transTrend(TransFrac[[i]], model = 'rodriguez')
  SS_xot <- c(SS_xot, sum(resid(X$feed)^2), sum(resid(X$strip)^2))
}
t.test(x = SS_par, y = SS_xot, paired = TRUE)
#-----PERFILES DE TRANSPORTE ------------------------------------------------
Parameters <- data.frame()

for (i in 1:20) {
#  transPlot(trans = TransFrac[[i]], trend = TransNLS[[i]], xlim = c(0, 6.1), ylim = c(-0.05, 1.08),
#                             ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:6)
  Parameters <- rbind(Parameters, TransNLS[[i]]$Result)
}


colnames(Parameters) <- names(TransNLS[[1]]$Result)
round(Parameters, 3)

trans1 = list(TransFrac[[1]], TransFrac[[2]], TransFrac[[3]], TransFrac[[4]], TransFrac[[5]],
              TransFrac[[6]], TransFrac[[7]], TransFrac[[8]], TransFrac[[9]], TransFrac[[10]])
trend1 = list(TransNLS[[1]], TransNLS[[2]], TransNLS[[3]], TransNLS[[4]], TransNLS[[5]],
              TransNLS[[6]], TransNLS[[7]], TransNLS[[8]], TransNLS[[9]], TransNLS[[10]])
trans2 = list(TransFrac[[11]], TransFrac[[12]], TransFrac[[13]], TransFrac[[14]], TransFrac[[15]],
              TransFrac[[16]], TransFrac[[17]], TransFrac[[18]], TransFrac[[19]], TransFrac[[20]])
trend2 = list(TransNLS[[11]], TransNLS[[12]], TransNLS[[13]], TransNLS[[14]], TransNLS[[15]],
              TransNLS[[16]], TransNLS[[17]], TransNLS[[18]], TransNLS[[19]], TransNLS[[20]])

# Not so appropiate plot
transPlotWR(trans = trans1, trend = trend1, explicit = FALSE)
# Nicer plot
hues = seq(15, 375, length = length(trans1) + 1)
cols = hcl(h = hues, l = 65, c = 100)[1:length(trans1)]
p <- multiPlotSP(trans = trans1, phase = 'Strip', trend = trend1, legend = TRUE, plot = FALSE)

p + theme(legend.position =  'right') + scale_colour_manual(name = "Ciclo", labels = 1:10, values = cols[1:10])
p <- multiPlotSP(trans = trans1, phase = 'Feed', trend = trend1, legend = TRUE, plot = FALSE)
p + theme(legend.position =  'right') + scale_colour_manual(name = "Ciclo", labels = 1:10, values = cols[1:10])

p <- multiPlotSP(trans = trans2, phase = 'Strip', trend = trend2, legend = TRUE, plot = FALSE)
p + theme(legend.position =  'right') + scale_colour_manual(name = "Ciclo", labels = 1:10, values = cols[1:10])
p <- multiPlotSP(trans = trans2, phase = 'Feed', trend = trend2, legend = TRUE, plot = FALSE)
p + theme(legend.position =  'right') + scale_colour_manual(name = "Ciclo", labels = 1:10, values = cols[1:10])

# AVERAGED INFORMATION
FracAve <- vector(mode = "list", length = length(TransFrac) / 2)
names(FracAve) <- names(TransFrac[1:10])
for (i in 1:10) {
  FracAve[[i]] <- data.frame(Time = TransFrac[[i]]$Time, Phase = TransFrac[[i]]$Phase,
                             Fraction = colMeans(rbind(TransFrac[[i]]$Fraction,
                                                       TransFrac[[10 + i]]$Fraction)))
}
TransNLSAve <- vector(mode = "list", length = length(TransFrac) / 2)
names(TransNLSAve) <- names(FracAve)
for (i in 1:10) {
  TransNLSAve[[i]] <- X <- transTrend(FracAve[[i]], model = 'paredes', eccen = 1)
}
p <- multiPlotSP(trans = FracAve, phase = 'Strip', trend = TransNLSAve, legend = FALSE,
                 plot = FALSE, bw = FALSE, size = 2.8, shape = 16)
multiPlotSP(trans = FracAve, phase = 'Strip', trend = TransNLSAve, legend = FALSE,
            plot = FALSE, bw = TRUE, size = 2.8, shape = 16, arw = TRUE, arw.pos = c(1, 1, 0.5, 0.8),
            arw.txt = "Fuck")
p <- p + theme(text = element_text(size = 9)) + scale_x_continuous(breaks = 0:6, limits = c(0, 6.4)) +
  annotate("segment", x = 6.3, xend = 6.3, y = 0.9, yend = 0.5, arrow = arrow(angle = 12)) +
  geom_text(x = 6.5, y = 0.7, label = "Cycle", angle = 90, size = 3.1)
p

p1 <- multiPlotSP(trans = FracAve, phase = 'Feed', trend = TransNLSAve, legend = FALSE,
                 plot = FALSE, bw = FALSE, size = 2.8, shape = 16)
p1 <- p1 + theme(text = element_text(size = 9)) + scale_x_continuous(breaks = 0:6, limits = c(0, 6.4)) +
  annotate("segment", x = 6.3, xend = 6.3, y = 0.1, yend = 0.5, arrow = arrow(angle = 12)) +
  geom_text(x = 6.5, y = 0.3, label = "Cycle", angle = 90, size = 3.1)
p1

p2 <- multiPlotSP(trans = trans2, phase = 'Feed', trend = trend2, legend = FALSE,
                  plot = FALSE, bw = FALSE, size = 2.8, shape = 16)
p2 <- p2 + theme(text = element_text(size = 9)) + scale_x_continuous(breaks = 0:6, limits = c(0, 6.4)) +
  annotate("segment", x = 6.3, xend = 6.3, y = 0.1, yend = 0.5, arrow = arrow(angle = 12)) +
  geom_text(x = 6.5, y = 0.3, label = "Cycle", angle = 90, size = 3.1) +
  scale_y_continuous(limits = c(0, 1))
p2

final <- vector()
for (i in 1:10) final <- c(final, trans2[[i]]$Fraction[10])
q <- ggplot(data = data.frame(Cycle = 1:10, Frac = final), aes(x = Cycle, y = Frac)) +
  geom_point(size=2.5) + geom_line() + theme_bw() + scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")) +
  labs(y = expression(Phi['6 hours']), x = 'Cycle number') + theme(text = element_text(size = 9))
print(q)

if (PDF) dev.off()

#pdf("ReuseProfiles.pdf", height = 70/25.4, width = 90/25.4)
p2
#dev.off()

#pdf("ReuseSumm.pdf", height = 70/25.4, width = 90/25.4)
q
#dev.off()

po <- transPlot(trans = FracAve[[1]], trend = TransNLSAve[[1]], xbreaks = 0:6, size = 2)

po <- po + theme(text = element_text(size = 9))
p1 <- transPlotWR(trans = list(TransFrac[[1]], TransFrac[[11]]),
                  trend = list(TransNLSAve[[1]], TransNLSAve[[2]]),
                  bw = TRUE, srs = 0.55) +
  theme(text = element_text(size = 9))
#p1 <- transPlotWR(trans = list(TransFrac[[1]], TransFrac[[11]]))

#pdf("OptimProfiles.pdf", height = 70/25.4, width = 90/25.4)
#po
p1
#dev.off()

permcoef(trans = FracAve[[1]], conc_0 = AliConc[[1]][1], vol = 85, area = pi*1.25^2)

