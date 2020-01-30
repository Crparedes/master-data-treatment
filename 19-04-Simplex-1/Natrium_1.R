library(ggplot2)
NaStock  <- 0.0861 * 22990 * 0.5136 / 5.1747 * 1.0230 / 10.0315
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CurveNa.1 <- c(c(0.0000, 0.0548, 0.1433, 0.3205, 0.6178, 0.9372, 1.4929, 2.0799, 3.0523) * NaStock /
                c(6.0000, 6.1328, 6.0089, 6.1254, 6.1521, 6.1531, 6.1615, 6.1384, 6.1277))
AbsNaCurve.1_1 <- c(0.000, 0.036, 0.091, 0.195, 0.362, 0.535, 0.814, 1.085, 1.449)
AbsNaCurve.1_2 <- c(0.000, 0.010, 0.011, 0.018, 0.022, 0.026, 0.040)

#----------------------------------------------------------------------------
#-----MODELOS DE LAS CURVAS--------------------------------------------------
ModelNa.1_1 <- lm(AbsNaCurve.1_1[1:7] ~ CurveNa.1[1:7])
ModelNa.1_2 <- lm(AbsNaCurve.1_1[1:8] ~ CurveNa.1[1:8])
ModelNa.1_3 <- lm(AbsNaCurve.1_1[1:9] ~ CurveNa.1[1:9])
ModelNa.1_4 <- lm(AbsNaCurve.1_1[1:9] ~ CurveNa.1[1:9] + I(CurveNa.1[1:9]^2))

plot(AbsNaCurve.1_1 ~ CurveNa.1, pch = c(rep(1, 8), 2))
abline(ModelNa.1_1, col = 2, lty = 2)
abline(ModelNa.1_2, col = 6, lty = 2)
abline(ModelNa.1_3, col = 8, lty = 2)
curve(0.0009461 + 0.1889774 * x - 0.0043859 * x^2, add = TRUE, col = 4)

par(mfrow = c(2, 2))
plot(ModelNa.1_1, which = 1)
plot(ModelNa.1_2, which = 1)
plot(ModelNa.1_3, which = 1)
plot(ModelNa.1_4, which = 1)
par(mfrow = c(1, 1))

#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
Abs.Feed.8.X  <- c(0.494, 0.388, 0.394, 0.362, 0.575, 0.292, 0.337, 0.267, 0.460, 0.374, 0.460, 0.452)
Dil.Feed.8.X  <- c(6.0073, 6.0388, 6.1971, 6.0710, 6.0644, 6.2320, 6.0183, 6.1255, 6.1018, 6.0820, 6.1268, 6.1494) /
                 c(0.0583, 0.0585, 0.0585, 0.0591, 0.0600, 0.0584, 0.0586, 0.0586, 0.0586, 0.0591, 0.0593, 0.0586)
Samples <- paste0('Mem.', c(7, '3b', 8, '6b', 9, '9b'))
#-----CONCENTRACIÓN DE SODIO EN LAS ALÍCUOTAS--------------------------------
library(cmna)
Na.Feed.8.X <- quadratic2(b2 = ModelNa.1_4$coefficients[[3]], b1 = ModelNa.1_4$coefficients[[2]],
                          b0 = ModelNa.1_4$coefficients[[1]] - Abs.Feed.8.X)[13:24] * Dil.Feed.8.X

Na.Fases <- data.frame(Ver = attr((PIM$coords[, 4] * 22990)[c(8, 7, 1, 6, 9, 9)], "names"),
                       Teo = (PIM$coords[, 4] * 22990)[c(8, 7, 1, 6, 9, 9)],
                       Ini = Na.Feed.8.X[c(1, 3, 5, 7, 9, 11)],
                       Fin = Na.Feed.8.X[c(2, 4, 6, 8, 10, 12)],
                       Dif = Na.Feed.8.X[c(1, 3, 5, 7, 9, 11)] - Na.Feed.8.X[c(2, 4, 6, 8, 10, 12)])

Na.Fases$Frac <- Na.Fases$Dif / Na.Fases$Ini * 100

print(Na.Fases)

par(mfrow = c(1, 3))

opar <- par(oma = par()$oma, mar = par()$mar)
par(oma = c(5, 4, 0, 0) + 0.1, mar = c(0, 0, 1, 1) + 0.1)
plot(1:10)
par(oma = opar$oma, mar = opar$mar)

par(new = TRUE)

opar <- par(oma = par()$oma, mar = par()$mar)
par(oma = c(5, 4, 0, 0) + 0.1, mar = c(0, 0, 1, 1) + 0.1)
plot(10:1)
par(oma = opar$oma, mar = opar$mar)

par(new = TRUE)

opar <- par(oma = par()$oma, mar = par()$mar)
par(oma = c(5, 4, 0, 0) + 0.1, mar = c(0, 0, 1, 1) + 0.1)
plot(rep(5, 10))
par(oma = opar$oma, mar = opar$mar)



par(mfrow = c(1, 3))
plot(1:10)
plot(10:1)
plot(rep(5, 10))
