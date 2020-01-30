library(transmem)
library(ggplot2)
PDF <- FALSE
if (PDF) pdf("SWS-29-01-19.pdf", height = 7/1.3, width = 9/1.3)
n = 2
#-----STOCK SOLUTIONS--Prepared January 18th, 2020---------------------------
StockLi.622  <- 100.4 * 0.18787 * 0.990 / 0.0300314
StockNa.6627 <- 508.4 * 0.39337 * 0.995 / 0.0300249
StockK.1768  <- 101.8 * 0.52445 * 0.995 / 0.0300397
StockMg.979  <- 304.7 * 0.09479 * 1.014 / 0.0300828
StockCa.1349 <- 102.3 * 0.40043 * 0.990 / 0.0300715

StockLi.500u <- StockLi.622 * 0.6197 / 6.1725 * 0.4114 / 50.0378 * 1000
StockNa.20   <- StockNa.6627 * 0.1546 / 50.3288
StockK.10    <- StockK.1768 * 0.3235 / 50.0196
StockMg.2    <- StockMg.979 * 0.1182 / 51.2115
StockCa.10   <- StockCa.1349 * 0.3882 / 51.5038
#-----CALIBRATION CURVES AND MODELS------------------------------------------
CalCurves <- list(
  Lithium1   = data.frame(Conc = c(0, 0.0572, 0.2463, 1.2093, 2.4374, 3.1012) * StockLi.500u /
                            c(6, 6.1763, 6.1816, 6.1501, 6.2182, 6.1090),
                          Signal = c(0, 0.008, 0.041, 0.209, 0.417, 0.537)),
  LithiumSW1 = data.frame(Conc = c(0, 0.0709, 0.2340, 1.2107, 2.3974, 3.2936) * StockLi.500u /
                           c(6, 6.0722, 6.0480, 6.0575, 6.0114, 6.0348),
                         Signal = c(0, 0.010, 0.039, 0.210, 0.387, 0.598)),
  Sodium1    = data.frame(Conc = c(0, 0.6103, 1.1981, 1.7972, 2.3932, 3.0180) * StockNa.20 /
                            c(6, 6.0091, 6.0703, 6.0552, 6.0101, 6.0426),
                          Signal  = c(0, 0.444, 0.680, 0.846, 0.969, 1.082)),
  Potassium1 = data.frame(Conc = c(0, 0.1161, 0.3845, 0.7267, 0.9607, 1.2154) * StockK.10 /
                            c(6, 6.0537, 6.0643, 6.0318, 6.0002, 6.0493),
                          Signal = c(0, 0.133, 0.393, 0.692, 0.863, 0.932))
)
## for a cleaner workspace
#rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
CalModels <- list()
order = c(1, 1, 2, 2)
for (i in 1:4) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = order[i], plot = TRUE)
names(CalModels) <- names(CalCurves)
#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- c(0, 1, 3, 4.5, 4.51, 5.5, 7.5, 9)
#-----SEÑALES DE EMISIÓN DE LAS MUESTRAS-------------------------------------
AliEm <- list(
  Feed.25.1.Li     = c(0.322, 0.222, 0.098, 0.081),
  Feed.25.2.Li     = c(0.322, 0.230, 0.212, 0.205),
  
  Strip.25.1.Li    = c(0.000, 0.107, 0.229, 0.276),
  Strip.25.2.Li    = c(0.264, 0.402, 0.429, 0.433),
  
  Strip.25.1.Li.SA = c(0.084, 0.191, 0.319, 0.345),
  Strip.25.2.Li.SA = c(0.334, 0.469, 0.490, 0.499),

  Feed.25.1.Na     = c(0.850, 0.849, 0.854, 0.856),
  Feed.25.2.Na     = c(0.893, 0.881, 0.866, 0.839),
  Strip.25.1.Na    = c(0.032, 0.470, 0.698, 0.748),
  Strip.25.2.Na    = c(0.723, 0.558, 0.807, 0.932),

  Feed.25.1.K      = c(0.376, 0.362, 0.365, 0.378),
  Feed.25.2.K      = c(0.406, 0.514, 0.363, 0.365),
  Strip.25.1.K     = c(0.050, 0.063, 0.140, 0.249),
  Strip.25.2.K     = c(0.265, 0.099, 0.212, 0.291)
)
#-----DILUCIÓN DE LAS MUESTRAS PARA SODIO Y POTASIO--------------------------
AliDil <- list(
  Feed.25.1.1_25  = c(811/20222, 813/20320, 813/20315, 812/20334)^(-1),
  Feed.25.2.1_25  = c(820/20338, 820/20383, 820/20348, 818/20380)^(-1),
  Strip.25.1.1_10 = c(787/8152, 788/8133, 793/8135, 792/8125)^(-1),
  Strip.25.2.1_10 = c(792/8122, 798/8141, 794/8151, 793/8152)^(-1)
)
AliDil$Feed.25.1.1_K  <- AliDil$Feed.25.1.1_25 * c(0998/19869, 0983/20113, 1004/20053, 1007/20013)^(-1)
AliDil$Feed.25.2.1_K  <- AliDil$Feed.25.2.1_25 * c(1011/20051, 1010/19964, 1002/20041, 1008/19951)^(-1)
AliDil$Feed.25.1.1_Na <- AliDil$Feed.25.1.1_K * c(2208/10390, 2239/10329, 2253/10309, 2226/10290)^(-1)
AliDil$Feed.25.2.1_Na <- AliDil$Feed.25.2.1_K * c(2268/10381, 2255/10459, 2264/10389, 2109/10164)^(-1)

AliDil$Strip.25.1.1_K  <- AliDil$Strip.25.1.1_10
AliDil$Strip.25.2.1_K  <- AliDil$Strip.25.2.1_10 * c(1, 1835/14845, 1767/14672, 1774/14667)^(-1)

AliDil$Strip.25.1.1_Na <- AliDil$Strip.25.1.1_10 * c(2206/10276, 2063/10389, 1937/10045, 1774/11339)^(-1)
AliDil$Strip.25.2.1_Na <- AliDil$Strip.25.2.1_10 * c(1758/12219, 0989/20056, 0991/19907, 0992/19976)^(-1)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- vector(mode = "list", length = length(AliEm))
names(AliConc) <- names(AliEm)
for (i in 1:n) AliConc[[i]] <- signal2conc(signal = AliEm[[i]], model = CalModels$LithiumSW1)
for (i in (3*n+1):(3*n+2)) AliConc[[i]] <- signal2conc(signal = AliEm[[i]], model = CalModels$Sodium1,
                                                       dilution = )

for (i in (4*n+1):(4*n+2)) AliConc[[i]] <- signal2conc(signal = AliEm[[i]], model = CalModels$Sodium1)

for (i in (5*n+1):(5*n+2)) AliConc[[i]] <- signal2conc(signal = AliEm[[i]], model = CalModels$Potassium1)

for (i in (6*n+1):(6*n+2)) AliConc[[i]] <- signal2conc(signal = AliEm[[i]], model = CalModels$Potassium1)
#-----CONCENTRACIÓN POR ADICION ESTÁNDAR DE UN SOLO PUNTO--------------------
AliDilLi <- list(Strip.25.1 = rep(1, 4), Strip.25.2 = rep(1, 4),
                 Strip.25.3 = c(), Strip.25.3 = c())
SpikeMass <- list(Strip.25.1 = c(493, 453, 540, 494),
                  Strip.25.2 = c(499, 498, 495, 528),
                  Strip.25.3 = c(567, 566, 566, 565),
                  Strip.25.4 = c(568, 567, 567, 584))
FinalMass <- list(Strip.25.1 = c(5691, 5646, 5755, 5690),
                  Strip.25.2 = c(5692, 5689, 5695, 5715),
                  Strip.25.3 = c(6507, 6572, 6582, 6627),
                  Strip.25.4 = c(6484, 6588, 6556, 6557))
for (i in 1:2) {
  AliConc[[i+n]] <- ((AliEm[[i+n]] * StockLi.500u * (SpikeMass[[i]] / FinalMass[[i]])) /
    (AliEm[[i+(2*n)]] - AliEm[[i+n]] * ((FinalMass[[i]] - SpikeMass[[i]]) / FinalMass[[i]]))) * 
    AliDilLi[[i]]
}
#-----PERFILES DE TRANSPORTE ------------------------------------------------
A <- data.frame(Phase = rep(c('Rec.', 'Alim.'), each = 20), Conc = c(AliConc[[1]], AliConc[[2]]), Tiempo = AliTimes)
ggplot(data = A, aes(x = Tiempo, y = Conc, color = Phase)) + geom_point() + theme_bw()

ggplot(data = A, aes(x = Tiempo, y = Conc, shape = Phase)) +
  geom_vline(xintercept = c(0, 4.5, 9, 13.5, 18), linetype = 'dashed', color = 'gray') +
  geom_point(size = 2) + theme_bw() +
#  geom_errorbar(aes(ymin = Conc - 5, ymax = Conc + 5), width = 0.4) +
  scale_x_continuous(breaks = seq(0, 24, 2), limits = c(-0.2, 22.7)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")) +
  scale_color_manual(values = c("black", "red")) +
  labs(y = expression(paste('Conc. (mg k', g^{-1}, ')')), x = 'Time (h)') +
  theme(text = element_text(size = 9), legend.position = "none")
if (PDF) dev.off()
