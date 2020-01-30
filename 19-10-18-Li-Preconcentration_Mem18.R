library(ggplot2)
library(ggformula)
library(transmem)
PDF <- TRUE
#-----STOCK SOLUTIONS--------------------------------------------------------
StockLi.200_2 <- 130.3 * 0.187872 * 0.99 / 0.1205105
StockLi.5_7   <- StockLi.200_2 * 1.2673 / 50.0164
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CalCurves <- list(
  Lithium.1 = data.frame(Conc = c(0.0000, 0.0597, 0.2989, 0.9115, 1.2053, 1.6816, 2.1670, 2.6474) *
                                StockLi.5_7 / c(6.0000, 6.1144, 6.0524, 6.0355, 6.1449, 6.0859, 6.0838, 6.0795),
                         Signal = c(0.000, 0.033, 0.161, 0.483, 0.619, 0.848, 1.050, 1.251)),
  Lithium.2 = data.frame(Conc = c(0.0000, 0.0597, 0.2989, 0.9115, 1.2053, 1.6816, 2.1670, 2.6474) *
                           StockLi.5_7 / c(6.0000, 6.1144, 6.0524, 6.0355, 6.1449, 6.0859, 6.0838, 6.0795),
                         Signal = c(0.000, 0.031, 0.152, 0.445, 0.562, 0.764, 0.958, 1.128))
)
## for a cleaner workspace
rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
CalModels <- list()
for (i in 1:2) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = 2, plot = TRUE)
names(CalModels) <- names(CalCurves)
summary(CalModels$Lithium.2)
summary(CalModels$Lithium.1)
#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- c(0, 1.5, 3, 4.75, 6, 6.01, 7.5, 9, 10.5, 12, 12.01, 13.5, 15.25,
              16.5, 18, 18.01, 19.5, 21, 22.5, 24, 24.01, 25.5, 27, 28.5, 30)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
dilutions <- list(
  Feed.18.1a = rep(1, 25),
  Strip.18.1a = c(rep(1, 5), 7708/3659, 7675/3639, 7863/3687, 7815/3763, 7755/3684,
                  7856/3770, 7903/3852, 13234/3797, 13150/3774, 12231/3742,
                  12137/2667, 11984/2738, 12062/2780, 11959/2755, 12075/2769,
                  12092/2769, 12001/2753, 12089/2784, 12035/2760, 12039/2780),
  Feed.18.1b = rep(1, 25),
  Strip.18.1b = c(rep(1, 5), 8517/3701, 8296/3734, 7863/3439, 8278/3662, 7967/3554,
                  12305/3293, 11113/3265, 11160/3267, 11150/3277, 11142/3281,
                  12838/2797, 12759/2807, 12747/2775, 12740/2780, 12666/2760,
                  12582/2758, 13710/2775, 13681/2753, 13662/2769, 13718/2786)
)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(# M2 is Na, K and Mg for Mem.17.2, 17.3 and 17.4, respectively.,
  Feed.18.1.Li.a   = c(1.173, 0.549, 0.288, 0.149, 0.110,
                       1.135, 0.769, 0.577, 0.453, 0.376,
                       1.141, 0.871, 0.705, 0.624, 0.563,
                       1.158, 0.937, 0.823, 0.739, 0.680,
                       1.176, 0.989, 0.893, 0.825, 0.789),
  Strip.18.1.Li.a  = c(0.005, 0.649, 0.849, 0.951, 0.991,
                       0.535, 0.730, 0.822, 0.874, 0.889,
                       0.899, 1.044, 0.723, 0.743, 0.806,
                       0.622, 0.691, 0.724, 0.736, 0.755,
                       0.748, 0.802, 0.833, 0.844, 0.855),
  Feed.18.1.Li.a   = c(1.170, 0.601, 0.358, 0.208, 0.155,
                       1.140, 0.750, 0.554, 0.427, 0.341,
                       1.160, 0.871, 0.672, 0.581, 0.499,
                       1.157, 0.940, 0.788, 0.688, 0.610,
                       1.156, 0.983, 0.868, 0.782, 0.727),
  Strip.18.1.Li.a  = c(0.005, 0.574, 0.798, 0.909, 0.930,
                       0.483, 0.687, 0.760, 0.829, 0.850,
                       0.552, 0.690, 0.745, 0.773, 0.802,
                       0.614, 0.687, 0.714, 0.735, 0.751,
                       0.752, 0.749, 0.771, 0.790, 0.805)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- vector(mode = "list", length = length(AliAbs))
names(AliConc) <- names(AliAbs)
for (i in c(1, 3)) {
  AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Lithium.1)
}
for (i in c(2, 4)) {
  AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Lithium.1, dilution = dilutions[[i]])
}
#-----PERFILES DE TRANSPORTE ------------------------------------------------

PDF <- TRUE


A <- data.frame(Fase = rep(c('Alim.', 'Rec.'), each = 25), Conc = c(AliConc[[1]], AliConc[[2]]), Tiempo = AliTimes)
ggplot(data = A, aes(x = Tiempo, y = Conc, color = Fase)) + geom_point() + theme_bw()
B <- data.frame(Fase = rep(c('Alim.', 'Rec.'), each = 25), Conc = c(AliConc[[3]], AliConc[[4]]), Tiempo = AliTimes)
ggplot(data = B, aes(x = Tiempo, y = Conc, color = Fase)) + geom_point() + theme_bw()

C <- data.frame(Phase = rep(c('Feed', 'Strip'), each = 25), Conc = apply(cbind(A$Conc, B$Conc), 1, mean),
                SD = apply(cbind(A$Conc, B$Conc), 1, sd), Tiempo = AliTimes)

if (PDF) pdf("PreconcProfiles.pdf", height = 70/25.4, width = 90/25.4)
ggplot(data = C, aes(x = Tiempo, y = Conc, shape = Phase)) +
  geom_vline(xintercept = c(0, 6, 12, 18, 24, 30), linetype = 'dashed', color = 'gray') +
  geom_point() + theme_bw() +
  geom_errorbar(aes(ymin = Conc - SD, ymax = Conc + SD), width = 0.4) +
  scale_x_continuous(breaks = seq(0, 30, 3), limits = c(0, 30)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")) +
  scale_color_manual(values = c("black", "red")) +
  labs(y = expression(paste('Conc. (mg k', g^{-1}, ')')), x = 'Time (h)') +
  theme(text = element_text(size = 9), legend.position = "none")
if (PDF) dev.off()






if (F) {


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

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
#-----PARÁMETROS DE DESEMPEÑO------------------------------------------------
Parameters <- data.frame()
j = 0
for (i in 1:6) {
  Parameters <- rbind(Parameters, c(TransNLS[[i]]$Result, sF[i], TransFrac[[2*i-1]][12, 3]))
}

colnames(Parameters) <- c(names(TransNLS[[1]]$Result), "sF")
round(Parameters, 3)
if (PDF) dev.off()
}
