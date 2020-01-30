library(ggplot2)
cStock    <- 129.5 * 0.187872 * 0.99 / 0.1200962
cStock5   <- cStock * 1.2484 / 50.2322

cStock5_1 <- cStock * 1.2377 / 50.1505
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CurvaS.3_4 <- c(c(0.0000, 0.0652, 0.1340, 0.3044, 0.6083, 1.2322, 2.3922, 3.0645) * cStock5 /
                c(6.0000, 6.0075, 6.0268, 6.0103, 6.0341, 6.0633, 6.0103, 6.0626))
AbsC.S.3_4 <- c(0.000, 0.010, 0.021, 0.046, 0.091, 0.186, 0.345, 0.428)

CurvaF.1_3 <- c(c(0.0000, 0.0592, 0.1205, 0.2446, 0.5938, 1.2562, 2.4378, 3.0479) * cStock5 /
                c(6.0000, 6.0992, 6.2358, 6.1827, 6.0371, 7.1252, 6.0531, 6.4249))
AbsC.F.1_3 <- c(0.000, 0.010, 0.019, 0.047, 0.097, 0.171, 0.378, 0.433)

#----------------------------------------------------------------------------
#-----MODELOS DE LAS CURVAS--------------------------------------------------
ModelS.3_4 <- lm(AbsC.S.3_4[1:7] ~ CurvaS.3_4[1:7])
plot(AbsC.S.3_4 ~ CurvaS.3_4, pch = c(rep(1, 7), 2))
abline(ModelS.3_4)

ModelF.1_3 <- lm(AbsC.F.1_3[1:7] ~ CurvaF.1_3[1:7])
plot(AbsC.F.1_3 ~ CurvaF.1_3, pch = c(rep(1, 7), 2))
abline(ModelF.1_3)

#-----TIEMPOS DE LOS TRANSPORTES--------------------------------------------
Time.9 <- Time.9b <- c(0, 1, 2, 3.25, 4, 4.5, 6.25, 7.25, 18, 22.25, 24)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
Abs.Feed.8.9  <- c(0.361, 0.237, 0.138, 0.070, 0.046, 0.033, 0.011, 0.006, 0.001, 0.000, 0.001)
Abs.Feed.8.9b <- c(0.357, 0.163, 0.078, 0.033, 0.020, 0.014, 0.005, 0.003, 0.000, 0.000, 0.000)

Abs.Strip.8.9  <- c(0.002, 0.118, 0.215, 0.281, 0.304, 0.315, 0.338, 0.341, 0.351, 0.355, 0.352)
Abs.Strip.8.9b <- c(0.003, 0.184, 0.270, 0.312, 0.330, 0.338, 0.352, 0.352, 0.352, 0.353, 0.356)
#-----CONCENTRACIÓN DE LITIO EN LAS ALÍCUOTAS--------------------------------
Abs.to.conc <- function (Abs, model) return((Abs - model$coefficients[1]) / model$coefficients[2])

Feed.8.9   <- Abs.to.conc(Abs.Feed.8.9, ModelF.1_3)
Feed.8.9b  <- Abs.to.conc(Abs.Feed.8.9b, ModelF.1_3)
Strip.8.9  <- Abs.to.conc(Abs.Strip.8.9, ModelS.3_4)
Strip.8.9b <- Abs.to.conc(Abs.Strip.8.9b, ModelS.3_4)
#-----DIFERENCIA DE LA CONCENTRACIÓN INICIAL MEDIDA CON LA DETERMINADA-------
T_0Feed.8.x <- c(0.8905, 0.9013) * cStock / c(90.3699, 90.2246)
Deviation_0 <- c(Feed.8.9[1], Feed.8.9b[1]) / T_0Feed.8.x
#-----TRANSFORMACIÓN A FRACCIONES--------------------------------------------
Li.Strip.8.9  <- Strip.8.9  <- Strip.8.9 / Feed.8.9[1]
Li.Feed.8.9   <- Feed.8.9   <- Feed.8.9 / Feed.8.9[1]
Li.Strip.8.9b <- Strip.8.9b <- Strip.8.9b / Feed.8.9b[1]
Li.Feed.8.9b  <- Feed.8.9b  <- Feed.8.9b / Feed.8.9b[1]
#-----REGRESIONES NO LINEALES------------------------------------------------
NLS.Strip.9  <- nls(Strip.8.9 ~ (a * Time.9**2)/(1/b + Time.9**2), start = list(a = 1, b = 0.5))
NLS.Feed.9   <- nls(Feed.8.9 ~ 1 - (a * Time.9**2)/(1/b + Time.9**2), start = list(a = 1, b = 0.5))

NLS.Strip.9b <- nls(Strip.8.9b ~ (a * Time.9b**2)/(1/b + Time.9b**2), start = list(a = 1, b = 0.5))
NLS.Feed.9b  <- nls(Feed.8.9b ~ 1 - (a * Time.9b**2)/(1/b + Time.9b**2), start = list(a = 1, b = 0.5))
#-----GRÁFICOS---------------------------------------------------------------
#pdf("Trans-V_8-V_6bis.pdf", height = 5, width = 10)
Transporte.9 <- data.frame(Tiempo = c(Time.9, Time.9), Conc = c(Feed.8.9, Strip.8.9),
                           Fase = rep(c('Alimentación', 'Recuperación'), each = length(Time.9)))
print(ggplot(data = Transporte.9, aes(x = Tiempo, y = Conc, group = Fase)) +
        geom_point(size = 3, aes(color = Fase)) + labs(y = 'Fracción transportada de litio', x = 'Tiempo (horas)') +
        stat_function(fun = function(x) (coefficients(NLS.Strip.9)[1] * x**2)
                      / (1/coefficients(NLS.Strip.9)[2] + x**2), color = 'darkgrey') +
        stat_function(fun = function(x) (1 - (coefficients(NLS.Feed.9)[1] * x**2)
                      / (1/coefficients(NLS.Feed.9)[2] + x**2)), color = 'darkgrey') +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.1)) +
        scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)))

Transporte.9b <- data.frame(Tiempo = c(Time.9b, Time.9b), Conc = c(Feed.8.9b, Strip.8.9b),
                           Fase = rep(c('Alimentación', 'Recuperación'), each = length(Time.9b)))
print(ggplot(data = Transporte.9b, aes(x = Tiempo, y = Conc, group = Fase)) +
        geom_point(size = 3, aes(color = Fase)) + labs(y = 'Fracción transportada de litio', x = 'Tiempo (horas)') +
        stat_function(fun = function(x) (coefficients(NLS.Strip.9b)[1] * x**2)
                      / (1/coefficients(NLS.Strip.9b)[2] + x**2), color = 'darkgrey') +
        stat_function(fun = function(x) (1 - (coefficients(NLS.Feed.9b)[1] * x**2)
                      / (1/coefficients(NLS.Feed.9b)[2] + x**2)), color = 'darkgrey') +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.1)) +
        scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)))

# dev.off()
#-----FUCNIÓN DE CALIDAD-----------------------------------------------------
QF <- matrix(ncol = 8, nrow = 2)
  NLScoeff <- c(coefficients(NLS.Feed.9)[1], coefficients(NLS.Feed.9)[2],
                coefficients(NLS.Strip.9)[1], coefficients(NLS.Strip.9)[2])
  qf_f  <- NLScoeff[1] * NLScoeff[2]
  qf_s  <- NLScoeff[3] * NLScoeff[4]
  qf_m  <- mean(c(qf_f, qf_s))
  qf_sd <- sd(c(qf_f, qf_s))
  QF[1, ] <- c(NLScoeff, qf_f, qf_s, qf_m, qf_sd)

  NLScoeff <- c(coefficients(NLS.Feed.9b)[1], coefficients(NLS.Feed.9b)[2],
                coefficients(NLS.Strip.9b)[1], coefficients(NLS.Strip.9b)[2])
  qf_f  <- NLScoeff[1] * NLScoeff[2]
  qf_s  <- NLScoeff[3] * NLScoeff[4]
  qf_m  <- mean(c(qf_f, qf_s))
  qf_sd <- sd(c(qf_f, qf_s))
  QF[2, ] <- c(NLScoeff, qf_f, qf_s, qf_m, qf_sd)

QF <- cbind(QF, (QF[,1]+QF[,3])/2 * (QF[,2]+QF[,4])/2)
colnames(QF) <- c("a_f", "b_f", "a_s", "b_s", "qf_f", "qf_s", "qf_m", "qf_sd", "QF")
rowMeans(QF[, c(1, 3)])
rowMeans(QF[, c(2, 4)])
