library(transmem)
PDF <- FALSE
if (PDF) pdf("Perfiles20-09-19p1.pdf", height = 7/1.8, width = 9/1.8)

#-----STOCK SOLUTIONS--------------------------------------------------------
StockLi.200_2 <- 130.3 * 0.187872 * 0.99 / 0.1205105
StockLi.5_7   <- StockLi.200_2 * 1.2673 / 50.0164
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CalCurves <- list(
  Lithium.1 = data.frame(Conc = c(0.0000, 0.0597, 0.2989, 0.9115, 1.2053, 1.6816, 2.1670, 2.6474) *
                                StockLi.5_7 / c(6.0000, 6.1144, 6.0524, 6.0355, 6.1449, 6.0859, 6.0838, 6.0795),
                         Signal = c(0.000, 0.032, 0.162, 0.482, 0.614, 0.832, 1.028, 1.213))
)
## for a cleaner workspace
#rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
CalModels <- list(
  Lithium.1 = calibCurve(curve = CalCurves$Lithium.1, order = 2)
)
anova(CalModels$Lithium.1)
summary(CalModels$Lithium.1)
#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- list (
  T.17.1a = c(0, 1, 2, 3, 4, 5),
  T.17.1b = c(0, 1, 2.17, 3, 4, 5)
)
ts <- c(1, 3, 6)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(# M2 is Na, K and Mg for Mem.17.2, 17.3 and 17.4, respectively.,
  Feed.17.1.Li.a   = c(1.084, 0.694, 0.452, 0.293, 0.194, 0.130),
  Strip.17.1.Li.a  = c(0.004, 0.454, 0.721, 0.866, 0.952, 1.012),
  Feed.17.1.Li.b   = c(1.068, 0.637, 0.343, 0.218, 0.130, 0.080),
  Strip.17.1.Li.b  = c(0.001, 0.525, 0.816, 0.921, 0.995, 1.028)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- vector(mode = "list", length = length(AliAbs))
names(AliConc) <- names(AliAbs)
for (i in 1:2) {
  AliConc[[2*i-1]] <- signal2conc(signal = AliAbs[[2*i-1]], model = CalModels$Lithium.1)
  AliConc[[2*i]] <- signal2conc(signal = AliAbs[[2*i]], model = CalModels$Lithium.1)
}
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- vector(mode = "list", length = length(AliConc)/2)
names(TransFrac) <- paste0("Lithium.", paste0(rep(1, each = 2), c('a', 'b')))
for (i in 1:length(TransFrac)) {
  TransFrac[[i]] <- conc2frac(feed = AliConc[[2*i-1]], strip = AliConc[[2*i]], time = AliTimes[[i]])
}
#-----MODELOS DE REGRESIÓN NO LINEAL-----------------------------------------
TransNLS  <- vector(mode = "list", length = length(TransFrac))
names(TransNLS) <- names(TransFrac)
SS_par <- vector()
for (i in 1:length(TransNLS)) {
  TransNLS[[i]] <- transTrend(trans = TransFrac[[i]], model = 'paredes', eccen = 1)
  SS_par <- c(SS_par, sum(resid(TransNLS[[i]]$feed)^2), sum(resid(TransNLS[[i]]$strip)^2))
}

TransNLSXot  <- vector(mode = "list", length = length(TransFrac))
names(TransNLSXot) <- names(TransFrac)
SS_xot <- vector()
for (i in 1:length(TransNLSXot)) {
  TransNLSXot[[i]] <- transTrend(TransFrac[[i]], model = 'rodriguez')
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
#-----PERFILES DE TRANSPORTE ------------------------------------------------
transPlotWR(trans = TransFrac, trend = TransNLS, xlim = c(0, 5.2), ylim = c(-0.05, 1.08),
            ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:5)

# invisible(readline(prompt="Press [enter] to continue"))
#-----PARÁMETROS DE DESEMPEÑO------------------------------------------------
Parameters <- data.frame()
j = 0
for (i in 1:2) {
  Parameters <- rbind(Parameters, c(TransNLS[[i]]$Result, TransFrac[[i]][12, 3], TransFrac[[i]][6, 3]))
}

colnames(Parameters) <- c(names(TransNLS[[1]]$Result), "FinalStrip", "FinalFeed")
round(Parameters, 3)
if (PDF) dev.off()
