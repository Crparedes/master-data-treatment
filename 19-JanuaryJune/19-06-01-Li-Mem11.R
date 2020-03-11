library(transmem)
#-----STOCK SOLUTIONS--------------------------------------------------------
StockLi.1 <- 129.5 * 0.187872 * 0.99 / 0.1200962

StockLi.1.5_3   <- StockLi.1 * 1.2484 / 50.2322
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CalCurves <- list(
  Lithium.1 = data.frame(Conc = c(0.0000, 0.0615, 0.1223, 0.3027, 0.5948, 1.2023, 2.4034, 2.7270) *
                                StockLi.1.5_3 / c(6.0000, 6.0084, 6.0084, 6.0344, 6.1404, 6.0674, 6.0297, 6.1499),
                         Signal = c(0.000, 0.007, 0.013, 0.034, 0.065, 0.134, 0.266, 0.295))
)
## for a cleaner workspace
rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
CalModels <- list(
  Lithium.1 = calibCurve(curve = CalCurves$Lithium.1)
)
#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- list (
  T.11.1 = c(0, 1, 2, 3.5, 4.5, 5.25, 6, 6.75, 10, 23, 24),
  T.11.2 = c(0, 1, 1.75, 2.5, 3.5, 5, 6, 7, 8, 23, 24),
  T.11.3 = c(0, 1, 1.75, 2.5, 3.5, 5, 6, 7, 8, 23, 24)
)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(
  Feed.11.1.Li   = c(0.277, 0.173, 0.113, 0.068, 0.053, 0.043, 0.034, 0.030, 0.015, 0.003, 0.004),
  Strip.11.1.Li  = c(0.000, 0.100, 0.161, 0.206, 0.221, 0.232, 0.240, 0.241, 0.258, 0.272, 0.275),
  Feed.11.2.Li   = c(0.272, 0.207, 0.174, 0.154, 0.128, 0.099, 0.087, 0.074, 0.066, 0.016, 0.015),
  Strip.11.2.Li  = c(0.001, 0.061, 0.097, 0.122, 0.147, 0.174, 0.188, 0.202, 0.211, 0.257, 0.258),
  Feed.11.3.Li   = c(0.267, 0.160, 0.110, 0.086, 0.060, 0.035, 0.026, 0.019, 0.015, 0.001, 0.001),
  Strip.11.3.Li  = c(0.002, 0.110, 0.160, 0.183, 0.210, 0.234, 0.243, 0.243, 0.252, 0.268, 0.267)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- list()
for (i in 1:3) {
  eval(parse(text = paste0("AliConc$Feed.11.", i, ".Li = signal2conc(signal = AliAbs$Feed.11.", i, ".Li,
                            model = CalModels$Lithium.1)")))
  eval(parse(text = paste0("AliConc$Strip.11.", i, ".Li = signal2conc(signal = AliAbs$Strip.11.", i, ".Li,
                            model = CalModels$Lithium.1)")))
}
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- list()
for (i in 1:3) {
  eval(parse(text = paste0("TransFrac$M.11.", i, ".Li = conc2frac(feed = AliConc$Feed.11.", i, ".Li,
                            strip = AliConc$Strip.11.", i, ".Li, time = AliTimes$T.11.", i, ")")))
}
#-----MODELOS DE REGRESIÓN NO LINEAL-----------------------------------------
TransNLS <- list()
for (i in 1:3) {
  eval(parse(text = paste0("TransNLS$M.11.", i, " <- transTrend(TransFrac$M.11.", i, ".Li, eccen = 1)")))
}
#-----PERFILES DE TRANSPORTE ------------------------------------------------
#pdf("Perfiles19-06-01.pdf", height = 5, width = 10)
for (i in 1:3) {
  eval(parse(text = paste0("transPlot(trans = TransFrac$M.11.", i, ".Li, trend = TransNLS$M.11.", i, ",
                             xlim = c(0, 25), ylim = c(-0.05, 1.15),
                             ybreaks = c(0, 0.25, 0.50, 0.75, 1, 1.1))")))
}


#dev.off()
###########################################################################
#-----PRUEBA T DE IGUALDAD DE MEDIAS-----------------------------------------
x <- matrix(ncol = 4, nrow = 3)
for (i in 1:3) {
  x[i, 1:4] <- eval(parse(text = paste0("c(as.vector(coefficients(TransNLS$M.11.", i, "$feed)),
                                           as.vector(coefficients(TransNLS$M.11.", i, "$strip)))")))
}
x <- data.frame(x)
round(x, 3)
AliConc
