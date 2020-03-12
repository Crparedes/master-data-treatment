library(ggplot2)
library(ggformula)
library(transmem)
PDF <- FALSE
if (PDF) pdf("Perfiles-19-11-10.pdf", height = 7/1.3, width = 9/1.3)

#-----STOCK SOLUTIONS--------------------------------------------------------
StockLi.200_2 <- 130.3 * 0.187872 * 0.99 / 0.1205105
StockLi.05_1   <- StockLi.200_2 * 0.15847 / 60.15202 * 1000 #Concentrations in ug kg^{-1}
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CalCurves <- list(
  Lithium.1 = data.frame(Conc = c(0.00000, 0.06908, 0.13992, 0.31141, 0.61770, 1.20980, 2.45930, 3.10441) *
                                StockLi.05_1 / c(6.0000, 6.2395, 5.9189, 6.4980, 6.3358, 6.1472, 6.1844, 6.0843),
                         Signal = c(0.000, 0.012, 0.025, 0.051, 0.107, 0.220, 0.446, 0.573))
)
## for a cleaner workspace
#rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
CalModels <- list()
for (i in 1:1) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = 1, plot = TRUE)
names(CalModels) <- names(CalCurves)
summary(CalModels$Lithium.1)
#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- c(0, 0.25, 0.50, 1.00, 1.50, 2.00, 3.00, 3.50)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(
  Feed = c(0.400, 0.315, 0.260, 0.180, 0.136, 0.104, 0.075, 0.065),
  Strip = c(0.001, 0.091, 0.165, 0.260, 0.322, 0.355, 0.396, 0.406),
  FeedSp = c(0.470, 0.340, 0.230, 0.163),
  StripSp = c(0.113, 0.264, 0.410, 0.480)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- vector(mode = "list", length = length(AliAbs))
for (i in 1:2) {
  AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Lithium.1)
}
#-----CONCENTRACIÓN POR ADICION ESTÁNDAR DE UN SOLO PUNTO--------------------
FinalMass <- list(feed = c(0.59664, 0.58113, 0.59624, 0.59912), 
                  strip = c(0.58077, 0.57426, 0.58028, 0.58136))
SpikeMass <- list(feed = c(0.05281, 0.05231, 0.05345, 0.04975), 
                  strip = c(0.05440, 0.04880, 0.05384, 0.05439))
InitiMass <- list(feed = FinalMass[[1]] - SpikeMass[[1]], strip = FinalMass[[2]] - SpikeMass[[2]])
SubSeq <- seq(1, 8, 2)
for (i in 1:2) {
  AliConc[[i+2]] <- (AliAbs[[i]][SubSeq] * StockLi.05_1 * (SpikeMass[[i]] / FinalMass[[i]])) / 
    (AliAbs[[2+i]] - AliAbs[[i]][SubSeq] * (InitiMass[[i]] / FinalMass[[i]]))
}
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- vector(mode = "list", length = length(AliConc) / 2)
for (i in 1:2) {
  if (i == 2) AliTimes1 = AliTimes[SubSeq] else AliTimes1 = AliTimes
  TransFrac[[i]] <- conc2frac(feed = AliConc[[2 * i - 1]], strip = AliConc[[2 * i]],
                              time = AliTimes1)
}
#-----MODELOS DE REGRESIÓN NO LINEAL-----------------------------------------
TransNLSXot <- TransNLS <- vector(mode = "list", length = (length(TransFrac) * 2))
#names(TransNLSXot) <- names(TransNLS) <- names(TransFrac)
SS_xot <- SS_par <- vector()
for (i in 1:2) {
  TransNLS[[i]] <- X <- transTrend(TransFrac[[i]], model = 'paredes', eccen = 1)
  SS_par <- c(SS_par, sum(resid(X$feed)^2), sum(resid(X$strip)^2))

  TransNLSXot[[i]] <- X <- transTrend(TransFrac[[i]], model = 'rodriguez')
  SS_xot <- c(SS_xot, sum(resid(X$feed)^2), sum(resid(X$strip)^2))
}
t.test(x = SS_par, y = SS_xot, paired = TRUE)
#-----PERFILES DE TRANSPORTE ------------------------------------------------
Parameters <- data.frame()

for (i in 1:2) {
  transPlot(trans = TransFrac[[i]], trend = TransNLS[[i]], xlim = c(0, 3.5), ylim = c(-0.05, 1.08),
            ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:3)
  Parameters <- rbind(Parameters, TransNLS[[i]]$Result)
}


colnames(Parameters) <- names(TransNLS[[1]]$Result)
round(Parameters, 3)

TransFrac[[1]][1:8, 3] + TransFrac[[1]][9:16, 3]
TransFrac[[2]][1:4, 3] + TransFrac[[2]][5:8, 3]

if (PDF) dev.off()
