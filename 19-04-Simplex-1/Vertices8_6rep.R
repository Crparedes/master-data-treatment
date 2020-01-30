library(ggplot2)
cStock    <- 129.5 * 0.187872 * 0.99 / 0.1200962
cStock5   <- cStock * 1.2484 / 50.2322

cStock5_1 <- cStock * 1.2377 / 50.1505
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CurvaS.3_3 <- c(c(0.0000, 0.0652, 0.1340, 0.3044, 0.6083, 1.2322, 2.3922, 3.0645) * cStock5 /
                c(6.0000, 6.0075, 6.0268, 6.0103, 6.0341, 6.0633, 6.0103, 6.0626))
AbsC.S.3_3 <- c(0.000, 0.007, 0.016, 0.035, 0.068, 0.140, 0.267, 0.335)

CurvaS.8_1 <- c(c(0.0000, 0.0628, 0.1176, 0.2792, 0.5012, 1.0104, 2.0039, 2.3057) * cStock5_1 /
                  c(6.0000, 6.0136, 6.0092, 6.0270, 6.0632, 6.0429, 6.0496, 6.0633))
AbsC.S.8_1 <- c(0.000, 0.007, 0.013, 0.031, 0.056, 0.112, 0.228, 0.257)

CurvaF.3_2 <- c(c(0.0000, 0.0592, 0.1205, 0.2446, 0.5938, 1.2562, 2.4378, 3.0479) * cStock5_1 /
                c(6.0000, 6.0992, 6.2358, 6.1827, 6.0371, 7.1252, 6.0531, 6.4249))
AbsC.F.3_2 <- c(0.000, 0.007, 0.016, 0.040, 0.077, 0.148, 0.288, 0.364)

CurvaF.8_1 <- c(c(0.0000, 0.0598, 0.1440, 0.2527, 0.5052, 0.9977, 2.0093, 2.3069) * cStock5 /
                c(6.0000, 6.1482, 6.4671, 5.9901, 6.0197, 6.0150, 6.0242, 6.0530))
AbsC.F.8_1 <- c(0.000, 0.007, 0.016, 0.040, 0.077, 0.148, 0.288, 0.364)

#----------------------------------------------------------------------------
#-----MODELOS DE LAS CURVAS--------------------------------------------------
ModelS.8_1 <- lm(AbsC.S.8_1 ~ CurvaS.8_1)
plot(AbsC.S.8_1 ~ CurvaS.8_1, pch = c(rep(1, 7), 2))
abline(ModelS.8_1)

ModelS.3_3 <- lm(AbsC.S.3_3[1:7] ~ CurvaS.3_3[1:7])
plot(AbsC.S.3_3 ~ CurvaS.3_3, pch = c(rep(1, 7), 2))
abline(ModelS.3_3)

ModelF.8_1 <- lm(AbsC.F.8_1[1:7] ~ CurvaF.8_1[1:7])
plot(AbsC.F.8_1 ~ CurvaF.8_1, pch = c(rep(1, 7), 2))
abline(ModelF.8_1)

ModelF.3_2 <- lm(c(AbsC.F.3_2[1:7], AbsC.F.3_2[1:7], AbsC.F.3_2[1:7]) ~
                   c(CurvaF.3_2[1:7], CurvaF.3_2[1:7], CurvaF.3_2[1:7]))
plot(c(AbsC.F.3_2, AbsC.F.3_2) ~ c(CurvaF.3_2, CurvaF.3_2), pch = c(rep(1, 7), 2))
abline(ModelF.3_2)

#-----TIEMPOS DE LOS TRANSPORTES--------------------------------------------
Time.8 <- Time.6b <- c(0, 0.5, 1.5, 2.25, 3.25, 4.5, 6.5, 9.5, 11.75, 24.75, 26.75)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
Abs.Feed.8.8  <- c(0.291, 0.163, 0.151, 0.152, 0.150, 0.152, 0.154, 0.151, 0.151, 0.154, 0.154)
Abs.Feed.8.6b <- c(0.290, 0.222, 0.163, 0.125, 0.080, 0.037, 0.009, 0.001, 0.000, 0.000, 0.000)

Abs.Strip.8.8  <- c(0.014, 0.143, 0.151, 0.153, 0.155, 0.152, 0.153, 0.153, 0.154, 0.154, 0.152)
Abs.Strip.8.6b <- c(0.002, 0.061, 0.116, 0.148, 0.185, 0.225, 0.248, 0.257, 0.253, 0.260, 0.264)
#-----CONCENTRACIÓN DE LITIO EN LAS ALÍCUOTAS--------------------------------
Abs.to.conc <- function (Abs, model) return((Abs - model$coefficients[1]) / model$coefficients[2])

Feed.8.8   <- Abs.to.conc(Abs.Feed.8.8, ModelF.8_1)
Feed.8.6b  <- Abs.to.conc(Abs.Feed.8.6b, ModelF.3_2)
Strip.8.8  <- Abs.to.conc(Abs.Strip.8.8, ModelF.8_1)
Strip.8.6b <- Abs.to.conc(Abs.Strip.8.6b, ModelS.3_3)
#-----DIFERENCIA DE LA CONCENTRACIÓN INICIAL MEDIDA CON LA DETERMINADA-------
T_0Feed.8.x <- c(0.9017, 0.9017) * cStock / c(90.0746, 90.1658)
Deviation_0 <- c(Feed.8.8[1], Feed.8.6b[1]) / T_0Feed.8.x
#-----TRANSFORMACIÓN A FRACCIONES--------------------------------------------
Li.Strip.8.8  <- Strip.8.8  <- Strip.8.8 / Feed.8.8[1]
Li.Feed.8.8   <- Feed.8.8   <- Feed.8.8 / Feed.8.8[1]
Li.Strip.8.6b <- Strip.8.6b <- Strip.8.6b / Feed.8.6b[1]
Li.Feed.8.6b  <- Feed.8.6b  <- Feed.8.6b / Feed.8.6b[1]
#-----REGRESIONES NO LINEALES------------------------------------------------
NLS.Strip.8  <- nls(Strip.8.8 ~ (a * Time.8**2)/(1/b + Time.8**2), start = list(a = 1, b = 0.5))
NLS.Feed.8   <- nls(Feed.8.8 ~ 1 - (a * Time.8**2)/(1/b + Time.8**2), start = list(a = 1, b = 0.5))

NLS.Strip.6b <- nls(Strip.8.6b ~ (a * Time.6b**2)/(1/b + Time.6b**2), start = list(a = 1, b = 0.5))
NLS.Feed.6b  <- nls(Feed.8.6b ~ 1 - (a * Time.6b**2)/(1/b + Time.6b**2), start = list(a = 1, b = 0.5))
#-----GRÁFICOS---------------------------------------------------------------
#pdf("Trans-V_8-V_6bis.pdf", height = 5, width = 10)
Transporte.8 <- data.frame(Tiempo = c(Time.8, Time.8), Conc = c(Feed.8.8, Strip.8.8),
                           Fase = rep(c('Alimentación', 'Recuperación'), each = length(Time.8)))
print(ggplot(data = Transporte.8, aes(x = Tiempo, y = Conc, group = Fase)) +
        geom_point(size = 3, aes(color = Fase)) + labs(y = 'Fracción transportada de litio', x = 'Tiempo (horas)') +
        stat_function(fun = function(x) (coefficients(NLS.Strip.8)[1] * x**2)
                      / (1/coefficients(NLS.Strip.8)[2] + x**2), color = 'darkgrey') +
        stat_function(fun = function(x) (1 - (coefficients(NLS.Feed.8)[1] * x**2)
                      / (1/coefficients(NLS.Feed.8)[2] + x**2)), color = 'darkgrey') +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.1)) +
        scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)))

Transporte.6b <- data.frame(Tiempo = c(Time.6b, Time.6b), Conc = c(Feed.8.6b, Strip.8.6b),
                           Fase = rep(c('Alimentación', 'Recuperación'), each = length(Time.6b)))
print(ggplot(data = Transporte.6b, aes(x = Tiempo, y = Conc, group = Fase)) +
        geom_point(size = 3, aes(color = Fase)) + labs(y = 'Fracción transportada de litio', x = 'Tiempo (horas)') +
        stat_function(fun = function(x) (coefficients(NLS.Strip.6b)[1] * x**2)
                      / (1/coefficients(NLS.Strip.6b)[2] + x**2), color = 'darkgrey') +
        stat_function(fun = function(x) (1 - (coefficients(NLS.Feed.6b)[1] * x**2)
                      / (1/coefficients(NLS.Feed.6b)[2] + x**2)), color = 'darkgrey') +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.1)) +
        scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)))

# dev.off()
#-----FUCNIÓN DE CALIDAD-----------------------------------------------------
QF <- matrix(ncol = 8, nrow = 2)
  NLScoeff <- c(coefficients(NLS.Feed.8)[1], coefficients(NLS.Feed.8)[2],
                coefficients(NLS.Strip.8)[1], coefficients(NLS.Strip.8)[2])
  qf_f  <- NLScoeff[1] * NLScoeff[2]
  qf_s  <- NLScoeff[3] * NLScoeff[4]
  qf_m  <- mean(c(qf_f, qf_s))
  qf_sd <- sd(c(qf_f, qf_s))
  QF[1, ] <- c(NLScoeff, qf_f, qf_s, qf_m, qf_sd)

  NLScoeff <- c(coefficients(NLS.Feed.6b)[1], coefficients(NLS.Feed.6b)[2],
                coefficients(NLS.Strip.6b)[1], coefficients(NLS.Strip.6b)[2])
  qf_f  <- NLScoeff[1] * NLScoeff[2]
  qf_s  <- NLScoeff[3] * NLScoeff[4]
  qf_m  <- mean(c(qf_f, qf_s))
  qf_sd <- sd(c(qf_f, qf_s))
  QF[2, ] <- c(NLScoeff, qf_f, qf_s, qf_m, qf_sd)

QF <- cbind(QF, (QF[,1]+QF[,3])/2 * (QF[,2]+QF[,4])/2)
colnames(QF) <- c("a_f", "b_f", "a_s", "b_s", "qf_f", "qf_s", "qf_m", "qf_sd", "QF")
rowMeans(QF[, c(1, 3)])
rowMeans(QF[, c(2, 4)])
