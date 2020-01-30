library(ggplot2)
cStock  <- 129.5 * 0.187872 * 0.99 / 0.1200962
cStock5 <- cStock * 1.2484 / 50.2322
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CurvaS.1 <- c(c(0.0000, 0.0623, 0.1293, 0.3215, 0.6321, 1.2088, 2.4474, 3.0719) * cStock5 /
              c(6.0000, 6.1138, 6.1570, 6.1066, 6.2798, 6.0302, 6.2012, 6.1266))
AbsC.S.1 <- c(0.000, 0.007, 0.015, 0.037, 0.073, 0.145, 0.285, 0.355)

CurvaS.2 <- c(c(0.0000, 0.0667, 0.1235, 0.3042, 0.5950, 1.2661, 2.4213, 2.9851) * cStock5 /
              c(6.0000, 6.2459, 6.4873, 6.1130, 5.9841, 6.0776, 6.0439, 5.9978))
AbsC.S.2 <- c(0.000, 0.007, 0.013, 0.035, 0.070, 0.146, 0.277, 0.343)

CurvaS.3 <- c(c(0.0000, 0.0652, 0.1340, 0.3044, 0.6083, 1.2322, 2.3922, 3.0645) * cStock5 /
              c(6.0000, 6.0075, 6.0268, 6.0103, 6.0341, 6.0633, 6.0103, 6.0626))
AbsC.S.3 <- c(0.000, 0.007, 0.015, 0.036, 0.071, 0.142, 0.279, 0.353)

CurvaF.1 <- c(c(0.0000, 0.0592, 0.1205, 0.2446, 0.5938, 1.2562, 2.4378, 3.0479) * cStock5 /
              c(6.0000, 6.0992, 6.2358, 6.1827, 6.0371, 7.1252, 6.0531, 6.4249))
AbsC.F.1 <- c(0.000, 0.008, 0.016, 0.038, 0.079, 0.141, 0.313, 0.366)

CurvaF.2 <- c(c(0.0000, 0.0638, 0.1185, 0.3047, 0.6060, 1.2444, 2.4116, 3.0405) * cStock5 /
              c(6.0000, 6.1404, 6.1014, 6.0261, 6.0292, 6.0421, 6.4073, 6.0009))
AbsC.F.2 <- c(0.000, 0.008, 0.016, 0.042, 0.081, 0.165, 0.294, 0.390)

CurvaF.3 <- c(c(0.0000, 0.0558, 0.1298, 0.3105, 0.6049, 1.2016, 2.3780, 3.0182) * cStock5 /
              c(6.0000, 6.0254, 6.0153, 6.0184, 6.0251, 6.0545, 6.0011, 6.0204))
AbsC.F.3 <- c(0.000, 0.008, 0.018, 0.042, 0.080, 0.157, 0.308, 0.381)
#----------------------------------------------------------------------------
#-----MODELOS DE LAS CURVAS--------------------------------------------------
for (i in 1:3) {
  assign(x = paste0("ModelS.", i),
         value = eval(parse(text = paste0("lm(AbsC.S.", i, "[1:7] ~ CurvaS.", i, "[1:7])"))))
  eval(parse(text = paste0("plot(CurvaS.", i,", AbsC.S.", i,", pch = c(rep(1, 7), 2))")))
  eval(parse(text = paste0("abline(ModelS.", i,", col = 4)")))
}
for (i in 1:3) {
  assign(x = paste0("ModelF.", i),
         value = eval(parse(text = paste0("lm(AbsC.F.", i, "[1:7] ~ CurvaF.", i, "[1:7])"))))
  eval(parse(text = paste0("plot(CurvaF.", i,", AbsC.F.", i,", pch = c(rep(1, 7), 2))")))
  eval(parse(text = paste0("abline(ModelF.", i,", col = 4)")))
}
#-----TIEMPOS DE LOS TRANSPORTES--------------------------------------------
Time.1 <- Time.2 <- c(0, 1, 2, 3, 4, 10.25, 11.25, 13, 26, 26.5)
Time.3 <- Time.4 <- c(0, 1.5, 3, 4, 5, 7, 9, 10, 23, 24)
Time.5 <- c(0, 1, 3, 4, 5, 8.75, 10, 11, 24.5, 25.5)
Time.6 <- c(0, 0.75, 1.5, 2.5, 6.25, 7.25, 8, 10, 23, 24.5)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
Abs.Feed.8.1 <- c(0.313, 0.242, 0.203, 0.173, 0.138, 0.059, 0.057, 0.054, 0.049, 0.050)
Abs.Feed.8.2 <- c(0.311, 0.249, 0.215, 0.194, 0.170, 0.106, 0.099, 0.099, 0.099, 0.098)
Abs.Feed.8.3 <- c(0.305, 0.143, 0.080, 0.048, 0.028, 0.012, 0.007, 0.006, 0.009, 0.009)
Abs.Feed.8.4 <- c(0.302, 0.189, 0.122, 0.087, 0.060, 0.038, 0.026, 0.023, 0.011, 0.010)
Abs.Feed.8.5 <- c(0.311, 0.262, 0.199, 0.176, 0.150, 0.119, 0.119, 0.119, 0.117, 0.118)
Abs.Feed.8.6 <- c(0.304, 0.223, 0.192, 0.145, 0.033, 0.021, 0.015, 0.007, 0.001, 0.001)

Abs.Strip.8.1 <- c(0.000, 0.048, 0.091, 0.121, 0.154, 0.245, 0.244, 0.249, 0.259, 0.255)
Abs.Strip.8.2 <- c(0.000, 0.043, 0.074, 0.096, 0.116, 0.179, 0.182, 0.182, 0.187, 0.188)
Abs.Strip.8.3 <- c(0.002, 0.158, 0.212, 0.237, 0.249, 0.260, 0.265, 0.268, 0.272, 0.272)
Abs.Strip.8.4 <- c(0.000, 0.081, 0.154, 0.192, 0.219, 0.244, 0.265, 0.264, 0.281, 0.285)
Abs.Strip.8.5 <- c(0.001, 0.040, 0.101, 0.124, 0.149, 0.181, 0.183, 0.184, 0.186, 0.186)
Abs.Strip.8.6 <- c(0.002, 0.069, 0.106, 0.153, 0.264, 0.276, 0.280, 0.289, 0.299, 0.298)
#-----CONCENTRACIÓN DE LITIO EN LAS ALÍCUOTAS--------------------------------
Abs.to.conc <- function (Abs, model) return((Abs - model$coefficients[1]) / model$coefficients[2])

Feed.8.1 <- Abs.to.conc(Abs.Feed.8.1, ModelF.1)
Feed.8.2 <- Abs.to.conc(Abs.Feed.8.2, ModelF.1)
Feed.8.3 <- Abs.to.conc(Abs.Feed.8.3, ModelF.1)
Feed.8.4 <- Abs.to.conc(Abs.Feed.8.4, ModelF.2)
Feed.8.5 <- Abs.to.conc(Abs.Feed.8.5, ModelF.2)
Feed.8.6 <- Abs.to.conc(Abs.Feed.8.6, ModelF.3)
Strip.8.1 <- Abs.to.conc(Abs.Strip.8.1, ModelS.1)
Strip.8.2 <- Abs.to.conc(Abs.Strip.8.2, ModelS.1)
Strip.8.3 <- Abs.to.conc(Abs.Strip.8.3, ModelS.1)
Strip.8.4 <- Abs.to.conc(Abs.Strip.8.4, ModelS.1)
Strip.8.5 <- Abs.to.conc(Abs.Strip.8.5, ModelS.2)
Strip.8.6 <- Abs.to.conc(Abs.Strip.8.6, ModelS.3)
#-----DIFERENCIA DE LA CONCENTRACIÓN INICIAL MEDIDA CON LA DETERMINADA-------
T_0Feed.8.x <- c(0.9052, 0.8963, 0.8976, 0.8963, 0.9024, 0.8965) * cStock /
  c(90.2351, 90.1765, 90.8008, 91.3350, 90.5080, 91.4206)
Deviation_0 <- vector()
for (i in 1:6) Deviation_0 <- c(Deviation_0, eval(parse(text = paste0("Feed.8.", i)))[1] / T_0Feed.8.x[i])
#-----TRANSFORMACIÓN A FRACCIONES--------------------------------------------
for (i in 1:6) {
  eval(parse(text = paste0("Strip.8.", i, "<- Strip.8.", i,"/ Feed.8.", i, "[1]")))
  eval(parse(text = paste0("Feed.8.", i, "<- Feed.8.", i, "/ Feed.8.", i, "[1]")))
}
#-----REGRESIONES NO LINEALES------------------------------------------------
for (i in 1:6) {
  assign(x = paste0("NLS.Strip.", i), value = eval(parse(text = paste0("nls(Strip.8.", i,
         "~ (a * Time.", i, "**2)/(1 / b + (Time.", i, "**2)), start = list(a = 1, b = 0.5))"))))
  assign(x = paste0("NLS.Feed.", i), value = eval(parse(text = paste0("nls(Feed.8.", i,
         "~ 1 - (a * Time.", i, "**2)/(1 / b + (Time.", i, "**2)), start = list(a = 1, b = 0.5))"))))
}
#-----GRÁFICOS---------------------------------------------------------------
pdf("Trans-V_1-V_6.pdf", height = 5, width = 10)
for (i in 1:6) {
  assign(x = paste0("Transporte.", i),
         value = eval(parse(text = paste0("data.frame(Tiempo = c(Time.", i, ", Time.", i, "),
                                          Conc = c(Feed.8.", i, ", Strip.8.", i, "),
                                          Fase = rep(c('Alimentación', 'Recuperación'), each = length(Time.", i, ")))"))))

  eval(parse(text = paste0("print(ggplot(data = Transporte.", i, ", aes(x = Tiempo, y = Conc, group = Fase)) +
    geom_point(size = 3, aes(color = Fase)) + labs(y = 'Fracción transportada de litio', x = 'Tiempo (horas)') +
    stat_function(fun = function(x) (coefficients(NLS.Strip.", i, ")[1] * x**2)
                           / (1/coefficients(NLS.Strip.", i, ")[2] + x**2), color = 'darkgrey') +
    stat_function(fun = function(x) (1 - (coefficients(NLS.Feed.", i, ")[1] * x**2)
                           / (1/coefficients(NLS.Feed.", i, ")[2] + x**2)), color = 'darkgrey') +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.1)) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)))")))
}
dev.off()
#-----FUCNIÓN DE CALIDAD-----------------------------------------------------
QF <- matrix(ncol = 8, nrow = 6)
for (i in 1:6) {
  NLScoeff <- eval(parse(text = paste0("c(coefficients(NLS.Feed.", i, ")[1], 
                                          coefficients(NLS.Feed.", i, ")[2],
                                          coefficients(NLS.Strip.", i, ")[1], 
                                          coefficients(NLS.Strip.", i, ")[2])")))
  qf_f  <- NLScoeff[1] * NLScoeff[2]
  qf_s  <- NLScoeff[3] * NLScoeff[4]
  qf_m  <- mean(c(qf_f, qf_s))
  qf_sd <- sd(c(qf_f, qf_s))
  QF[i, ] <- c(NLScoeff, qf_f, qf_s, qf_m, qf_sd)
}
QF <- cbind(QF, (QF[,1]+QF[,3])/2 * (QF[,2]+QF[,4])/2)
colnames(QF) <- c("a_f", "b_f", "a_s", "b_s", "qf_f", "qf_s", "qf_m", "qf_sd", "QF")
