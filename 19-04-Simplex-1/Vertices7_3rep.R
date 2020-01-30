library(ggplot2)
cStock  <- 129.5 * 0.187872 * 0.99 / 0.1200962
cStock5 <- cStock * 1.2484 / 50.2322
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CurvaS.1_1 <- c(c(0.0000, 0.0623, 0.1293, 0.3215, 0.6321, 1.2088, 2.4474, 3.0719) * cStock5 /
                c(6.0000, 6.1138, 6.1570, 6.1066, 6.2798, 6.0302, 6.2012, 6.1266))
AbsC.S.1_1 <- c(0.000, 0.007, 0.015, 0.037, 0.071, 0.143, 0.280, 0.349)

CurvaS.3_1 <- c(c(0.0000, 0.0652, 0.1340, 0.3044, 0.6083, 1.2322, 2.3922, 3.0645) * cStock5 /
                c(6.0000, 6.0075, 6.0268, 6.0103, 6.0341, 6.0633, 6.0103, 6.0626))
AbsC.S.3_1 <- c(0.000, 0.007, 0.016, 0.035, 0.071, 0.147, 0.284, 0.349)

CurvaF.1_1 <- c(c(0.0000, 0.0592, 0.1205, 0.2446, 0.5938, 1.2562, 2.4378, 3.0479) * cStock5 /
                c(6.0000, 6.0992, 6.2358, 6.1827, 6.0371, 7.1252, 6.0531, 6.4249))
AbsC.F.1_1 <- c(0.000, 0.008, 0.015, 0.040, 0.079, 0.142, 0.313, 0.362)

CurvaF.2_1 <- c(c(0.0000, 0.0638, 0.1185, 0.3047, 0.6060, 1.2444, 2.4116, 3.0405) * cStock5 /
                c(6.0000, 6.1404, 6.1014, 6.0261, 6.0292, 6.0421, 6.4073, 6.0009))
AbsC.F.2_1 <- c(0.000, 0.008, 0.015, 0.041, 0.081, 0.169, 0.295, 0.389)

#----------------------------------------------------------------------------
#-----MODELOS DE LAS CURVAS--------------------------------------------------
ModelS.1_1 <- lm(AbsC.S.1_1[1:7] ~ CurvaS.1_1[1:7])
plot(AbsC.S.1_1 ~ CurvaS.1_1, pch = c(rep(1, 7), 2))
abline(ModelS.1_1)

ModelS.3_1 <- lm(AbsC.S.3_1[1:7] ~ CurvaS.3_1[1:7])
plot(AbsC.S.3_1 ~ CurvaS.3_1, pch = c(rep(1, 7), 2))
abline(ModelS.3_1)

ModelF.1_1 <- lm(AbsC.F.1_1[1:7] ~ CurvaF.1_1[1:7])
plot(AbsC.F.1_1 ~ CurvaF.1_1, pch = c(rep(1, 7), 2))
abline(ModelF.1_1)

ModelF.4 <- lm(c(AbsC.F.1_1[1:7], AbsC.F.2_1[1:7], AbsC.F.2_1[1:7]) ~
                   c(CurvaF.1_1[1:7], CurvaF.2_1[1:7], CurvaF.2_1[1:7]))
plot(c(AbsC.F.1_1, AbsC.F.2_1) ~ c(CurvaF.1_1, CurvaF.2_1), pch = c(rep(1, 7), 2))
abline(ModelF.4)

#-----TIEMPOS DE LOS TRANSPORTES--------------------------------------------
Time.7 <- Time.3b <- c(0, 1, 2.75, 4, 4.5, 5.75, 6.5, 8, 11, 24.33, 25.33)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
Abs.Feed.8.7  <- c(0.306, 0.158, 0.045, 0.018, 0.013, 0.005, 0.004, 0.002, 0.002, 0.001, 0.001)#, 0.000)
Abs.Feed.8.3b <- c(0.301, 0.193, 0.086, 0.046, 0.037, 0.023, 0.018, 0.011, 0.010, 0.003, 0.002)#, 0.002)

Abs.Strip.8.7  <- c(0.001, 0.143, 0.250, 0.273, 0.280, 0.290, 0.292, 0.289, 0.288, 0.294, 0.296)#, 0.292)
Abs.Strip.8.3b <- c(0.001, 0.100, 0.203, 0.242, 0.252, 0.269, 0.269, 0.278, 0.279, 0.285, 0.289)#, 0.283)
#-----CONCENTRACIÓN DE LITIO EN LAS ALÍCUOTAS--------------------------------
Abs.to.conc <- function (Abs, model) return((Abs - model$coefficients[1]) / model$coefficients[2])

Feed.8.7   <- Abs.to.conc(Abs.Feed.8.7, ModelF.4)
Feed.8.3b  <- Abs.to.conc(Abs.Feed.8.3b, ModelF.1_1)
Strip.8.7  <- Abs.to.conc(Abs.Strip.8.7, ModelS.3_1)
Strip.8.3b <- Abs.to.conc(Abs.Strip.8.3b, ModelS.1_1)
#-----DIFERENCIA DE LA CONCENTRACIÓN INICIAL MEDIDA CON LA DETERMINADA-------
T_0Feed.8.x <- c(0.9048, 0.8966) * cStock / c(90.3719, 90.4774)
Deviation_0 <- c(Feed.8.7[1], Feed.8.3b[1]) / T_0Feed.8.x
#-----TRANSFORMACIÓN A FRACCIONES--------------------------------------------
Li.Strip.8.7  <- Strip.8.7  <- Strip.8.7 / Feed.8.7[1]
Li.Feed.8.7   <- Feed.8.7   <- Feed.8.7 / Feed.8.7[1]
Li.Strip.8.3b <- Strip.8.3b <- Strip.8.3b / Feed.8.3b[1]
Li.Feed.8.3b  <- Feed.8.3b  <- Feed.8.3b / Feed.8.3b[1]
#-----REGRESIONES NO LINEALES------------------------------------------------
NLS.Strip.7  <- nls(Strip.8.7 ~ (a * Time.7**2)/(1/b + Time.7**2), start = list(a = 1, b = 0.5))
NLS.Feed.7   <- nls(Feed.8.7 ~ 1 - (a * Time.7**2)/(1/b + Time.7**2), start = list(a = 1, b = 0.5))

NLS.Strip.3b <- nls(Strip.8.3b ~ (a * Time.3b**2)/(1/b + Time.3b**2), start = list(a = 1, b = 0.5))
NLS.Feed.3b  <- nls(Feed.8.3b ~ 1 - (a * Time.3b**2)/(1/b + Time.3b**2), start = list(a = 1, b = 0.5))
#-----GRÁFICOS---------------------------------------------------------------
#pdf("Trans-V_7-V_3bis.pdf", height = 5, width = 10)
Transporte.7 <- data.frame(Tiempo = c(Time.7, Time.7), Conc = c(Feed.8.7, Strip.8.7),
                           Fase = rep(c('Alimentación', 'Recuperación'), each = length(Time.7)))
print(ggplot(data = Transporte.7, aes(x = Tiempo, y = Conc, group = Fase)) +
        geom_point(size = 3, aes(color = Fase)) + labs(y = 'Fracción transportada de litio', x = 'Tiempo (horas)') +
        stat_function(fun = function(x) (coefficients(NLS.Strip.7)[1] * x**2)
                      / (1/coefficients(NLS.Strip.7)[2] + x**2), color = 'darkgrey') +
        stat_function(fun = function(x) (1 - (coefficients(NLS.Feed.7)[1] * x**2)
                      / (1/coefficients(NLS.Feed.7)[2] + x**2)), color = 'darkgrey') +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.1)) +
        scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)))

Transporte.3b <- data.frame(Tiempo = c(Time.3b, Time.3b), Conc = c(Feed.8.3b, Strip.8.3b),
                           Fase = rep(c('Alimentación', 'Recuperación'), each = length(Time.3b)))
print(ggplot(data = Transporte.3b, aes(x = Tiempo, y = Conc, group = Fase)) +
        geom_point(size = 3, aes(color = Fase)) + labs(y = 'Fracción transportada de litio', x = 'Tiempo (horas)') +
        stat_function(fun = function(x) (coefficients(NLS.Strip.3b)[1] * x**2)
                      / (1/coefficients(NLS.Strip.3b)[2] + x**2), color = 'darkgrey') +
        stat_function(fun = function(x) (1 - (coefficients(NLS.Feed.3b)[1] * x**2)
                      / (1/coefficients(NLS.Feed.3b)[2] + x**2)), color = 'darkgrey') +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.1)) +
        scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)))

#dev.off()
#-----FUCNIÓN DE CALIDAD-----------------------------------------------------
QF <- matrix(ncol = 8, nrow = 2)
  NLScoeff <- c(coefficients(NLS.Feed.7)[1], coefficients(NLS.Feed.7)[2],
                coefficients(NLS.Strip.7)[1], coefficients(NLS.Strip.7)[2])
  qf_f  <- NLScoeff[1] * NLScoeff[2]
  qf_s  <- NLScoeff[3] * NLScoeff[4]
  qf_m  <- mean(c(qf_f, qf_s))
  qf_sd <- sd(c(qf_f, qf_s))
  QF[1, ] <- c(NLScoeff, qf_f, qf_s, qf_m, qf_sd)

  NLScoeff <- c(coefficients(NLS.Feed.3b)[1], coefficients(NLS.Feed.3b)[2],
                coefficients(NLS.Strip.3b)[1], coefficients(NLS.Strip.3b)[2])
  qf_f  <- NLScoeff[1] * NLScoeff[2]
  qf_s  <- NLScoeff[3] * NLScoeff[4]
  qf_m  <- mean(c(qf_f, qf_s))
  qf_sd <- sd(c(qf_f, qf_s))
  QF[2, ] <- c(NLScoeff, qf_f, qf_s, qf_m, qf_sd)

QF <- cbind(QF, (QF[,1]+QF[,3])/2 * (QF[,2]+QF[,4])/2)
colnames(QF) <- c("a_f", "b_f", "a_s", "b_s", "qf_f", "qf_s", "qf_m", "qf_sd", "QF")

rowMeans(QF[, c(1, 3)])
rowMeans(QF[, c(2, 4)])
