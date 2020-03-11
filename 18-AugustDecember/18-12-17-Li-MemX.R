library(ggplot2)
library(data.table)
#-------------------------------------------------------
cStock <- 109.0 * 0.187872 * 0.99 / 0.1004535
#-------------------------------------------------------
A_7 <- cStock * 0.3021 / 20.0382
B_7 <- cStock * 0.3055 / 20.0082
C_7 <- cStock * 0.2916 / 19.9763
D_7 <- cStock * 0.2955 / 20.2787
E_7 <- cStock * 0.3159 / 20.0570
F_7 <- cStock * 0.3230 / 19.9384

G_7  <- cStock * 0.3207 / 20.0251
B2_7 <- cStock * 0.3055 / 20.0082
E2_7 <- cStock * 0.3248 / 20.1809
#-------------------------------------------------------
ConcA  <- c(0, c(0.1015, 0.2062, 0.5063, 0.9918, 2.0116, 4.0131) * A_7 / c(6.0092, 6.1104, 6.0490, 6.0101, 6.0084, 6.1631), A_7)
ConcB  <- c(0, c(0.1019, 0.2068, 0.5043, 1.0027, 2.0083, 4.0086) * B_7 / c(6.0221, 6.0073, 6.0274, 6.0159, 6.0091, 6.0275), B_7)
ConcC  <- c(0, c(0.1071, 0.1951, 0.5054, 0.9982, 1.9930, 4.0172) * C_7 / c(6.0076, 6.0411, 6.4149, 6.2060, 6.2019, 6.0155), C_7)
ConcD  <- c(0, c(0.1034, 0.2102, 0.5041, 1.0206, 2.0522, 4.1199) * D_7 / c(6.1058, 6.0045, 6.0662, 6.1237, 6.1408, 5.9962), D_7)
ConcE  <- c(0, c(0.1091, 0.2159, 0.5202, 0.3225, 1.0568, 2.0887) * E_7 / c(6.1465, 5.9976, 5.2180, 2.4255, 3.1233, 3.1248), E_7)
ConcF  <- c(0, c(0.1025, 0.2084, 0.4845, 0.3466, 0.5052, 0.9207) * F_7 / c(6.0592, 6.1558, 5.8998, 2.3292, 2.1033, 1.5454), F_7)

ConcG  <- c(0, c(0.1230, 0.2140, 0.5185, 1.0441, 2.0835, 4.1032) * G_7 / c(6.8493, 6.4510, 6.0324, 6.0963, 6.0284, 6.1571), G_7)
ConcB2 <- c(0, c(0.1019, 0.2068, 0.5043, 1.0027, 2.0083, 4.0086) * B2_7 / c(6.0221, 6.0073, 6.0274, 6.0159, 6.0091, 6.0275), B2_7)
ConcE2 <- c(0, c(0.1116, 0.2073, 0.4822, 1.0174, 2.0379, 4.0940) * E2_7 / c(6.1666, 6.0110, 5.9941, 6.3287, 6.0144, 5.9783), E2_7)
#-------------------------------------------------------
AbsoA  <- c(0, 0.007, 0.015, 0.037, 0.072, 0.144, 0.274, 0.409)
AbsoB  <- c(0, 0.007, 0.014, 0.033, 0.066, 0.133, 0.257, 0.375)
AbsoC  <- c(0, 0.007, 0.014, 0.033, 0.068, 0.136, 0.271, 0.395)
AbsoD  <- c(0.023, 0.026, 0.064, 0.056, 0.095, 0.166, 0.307, 0.436)
AbsoE  <- c(0, 0.009, 0.018, 0.043, 0.067, 0.156, 0.314, 0.449)
AbsoF  <- c(0, 0.009, 0.017, 0.041, 0.075, 0.118, 0.288, 0.460)

AbsoG  <- c(0, 0.007, 0.013, 0.032, 0.064, 0.126, 0.238, 0.354)
AbsoB2 <- c(0, 0.007, 0.013, 0.032, 0.062, 0.126, 0.243, 0.360)
AbsoE2 <- c(0, 0.008, 0.016, 0.038, 0.077, 0.158, 0.310, 0.438)

AbsoB3 <- c(0, 0.007, 0.013, 0.030, 0.062, 0.123, 0.244, 0.361)
AbsoE3 <- c(0, 0.009, 0.017, 0.039, 0.077, 0.159, 0.314, 0.441)
#-------------------------------------------------------
A       <- "HNO3 2% - 17/Dic"
Model.A <- lm(AbsoA ~ ConcA)
B       <- "HCl 1.8% - 17/Dic"
Model.B <- lm(AbsoB ~ ConcB)
C       <- "D.H2O - 17/Dic"
Model.C <- lm(AbsoC ~ ConcC)
E       <- "NaOH 0.001 M + HNO3 2% - 17/Dic"
Model.E <- lm(AbsoE ~ ConcE)
F       <- "NaOH 0.001 M - 17/Dic"
Model.F <- lm(AbsoF ~ ConcF)
G       <- "HCl 3.6% - 23/Dic"
Model.G <- lm(AbsoG ~ ConcG)
B2      <- "HCl 1.8% - 23/Dic"
Model.B2<- lm(AbsoB2 ~ ConcB2)
E2      <- "NaOH 0.001 M + HNO3 2% - 23/Dic"
Model.E2<- lm(AbsoE2 ~ ConcE2)
B3      <- "HCl 1.8% - 24/Dic"
Model.B3<- lm(AbsoB3 ~ ConcB2)
E3      <- "NaOH 0.001 M + HNO3 2% - 24/Dic"
Model.E3<- lm(AbsoE3 ~ ConcE2)

Data <- data.frame(Conc = c(ConcA, ConcB, ConcC, ConcD, ConcE, ConcF, ConcG, ConcB2, ConcE2), 
                   Abs = c(AbsoA, AbsoB, AbsoC, AbsoD, AbsoE, AbsoF, AbsoG, AbsoB2, AbsoE2),
                   Iden = rep(c(LETTERS[1:7], "B2", "E2"), each = length(ConcA)))

ggplot(data = Data[-which(Data$Iden == "D"), ], aes(x = Conc, y = Abs)) + geom_point(aes(color = Iden)) + 
  stat_smooth(method = lm, aes(group = Iden, color = Iden))

ggplot(data = Data[which(Data$Iden %in% c("B", "B2")), ], aes(x = Conc, y = Abs)) + geom_point(aes(color = Iden)) + 
  stat_smooth(method = lm, aes(group = Iden, color = Iden))

ggplot(data = Data[which(Data$Iden %in% c("E", "E2")), ], aes(x = Conc, y = Abs)) + geom_point(aes(color = Iden)) + 
  stat_smooth(method = lm, aes(group = Iden, color = Iden))

Time1 <- c(0, 1, 2, 3, 4, 5, 7, 9)
Time2 <- c(0, 1, 2, 3, 3.5, 4, 6, 8)
Time3 <- c(0, 1, 2, 3, 4, 6, 8, 10, 24, 26, 30)

AbsStr4.1 <- c(0, 0.004, 0.010, 0.017, 0.023, 0.030, 0.040, 0.048)
AbsStr4.2 <- c(0, 0.004, 0.006, 0.006, 0.007, 0.009, 0.009, 0.009)
AbsStr4.3 <- c(0, 0.004, 0.009, 0.011, 0.013, 0.018, 0.028, 0.029)
AbsStr4.4 <- c(0, 0.012, 0.024, 0.036, 0.047, 0.050, 0.052, 0.053)
AbsStr4.5 <- c(0, 0.014, 0.029, 0.032, 0.034, 0.039, 0.044, 0.046)
AbsStr4.6 <- c(0, 0.004, 0.011, 0.015, 0.018, 0.021, 0.030, 0.032)
AbsStr5.1 <- c(0, 0.009, 0.022, 0.038, 0.055, 0.089, 0.117, 0.146, 0.250, 0.252, 0.252)
AbsStr5.2 <- c(0, 0.013, 0.021, 0.030, 0.038, 0.053, 0.063, 0.070, 0.079, 0.077, 0.079)
AbsStr5.3 <- c(0, 0.021, 0.042, 0.060, 0.077, 0.109, 0.127, 0.139, 0.155, 0.154, 0.153)

AbsFeeT.0 <- c(0.233, 0.242, 0.252)
DilFeeT.0 <- c(0.2159/2.3130, 0.2069/2.1233, 0.2107/2.0564)
ConFeeT.0 <- (AbsFeeT.0 - Model.E2$coefficients[1]) / Model.E2$coefficients[2] / DilFeeT.0
(paste(mean(ConFeeT.0), "+-", sd(ConFeeT.0)))

AbsFee4.1 <- c(0.266, 0.271, 0.260, 0.264, 0.264, 0.245, 0.250)
DilFee4.1 <- c(0.2209/2.0369, 0.2208/1.9785, 0.2214/2.0496, 0.2219/2.0014, 0.2223/2.0255, 0.2112/2.0825, 0.2219/2.1321)
AbsFee4.2 <- c(0.269, 0.257, 0.264, 0.248, 0.241, 0.280, 0.269)
DilFee4.2 <- c(0.2219/2.0054, 0.2218/2.1164, 0.2204/2.0503, 0.2234/2.2068, 0.2214/2.3039, 0.2232/1.9744, 0.2219/2.0285)
AbsFee4.3 <- c(0.265, 0.269, 0.230, 0.248, 0.233, 0.247, 0.208)
DilFee4.3 <- c(0.2209/2.0169, 0.2204/1.9509, 0.2266/2.3235, 0.2233/2.1367, 0.2225/2.2619, 0.2225/2.0856, 0.2240/2.4797)
AbsFee4.4 <- c(0.262, 0.247, 0.231, 0.267, 0.241, 0.240, 0.249)
DilFee4.4 <- c(0.2219/2.0001, 0.2239/2.1601, 0.2230/2.2828, 0.2237/1.9808, 0.2242/2.2104, 0.2213/2.2230, 0.2231/2.1706)
AbsFee4.5 <- c(0.263, 0.245, 0.242, 0.226, 0.218, 0.226, 0.256)
DilFee4.5 <- c(0.2236/2.0511, 0.2227/2.1825, 0.2224/2.2083, 0.2220/2.3600, 0.2227/2.4282, 0.2233/2.3638, 0.2222/2.0933)
AbsFee4.6 <- c(0.247, 0.258, 0.262, 0.251, 0.248, 0.243, 0.243)
DilFee4.6 <- c(0.2221/2.1979, 0.2223/2.0962, 0.2224/2.0708, 0.2229/2.1865, 0.2226/2.1364, 0.2291/2.3104, 0.2291/2.3104)
AbsFee5.1 <- c(0.228, 0.240, 0.210, 0.222, 0.247, 0.224, 0.213, 0.216, 0.196, 0.230)
DilFee5.1 <- c(0.2223/2.3537, 0.2155/2.1574, 0.2233/2.5278, 0.2246/2.3691, 0.2234/2.1031,
               0.2218/2.2935, 0.2229/2.5253, 0.2196/2.2785, 0.2224/2.5236, 0.2233/2.1458)
AbsFee5.2 <- c(0.202, 0.201, 0.194, 0.229, 0.239, 0.257, 0.229, 0.247, 0.268, 0.249)
DilFee5.2 <- c(0.2065/2.4048, 0.2010/2.3862, 0.2018/2.4653, 0.2203/2.2904, 0.2210/2.1576,
               0.2202/2.0155, 0.2233/2.4226, 0.2208/2.1767, 0.2217/2.0013, 0.2211/2.1895)
AbsFee5.3 <- c(0.255, 0.235, 0.227, 0.213, 0.234, 0.219, 0.210, 0.232, 0.246, 0.231)
DilFee5.3 <- c(0.2198/2.0364, 0.2201/2.1883, 0.2213/2.2977, 0.2189/2.4318, 0.2220/2.1806,
               0.2210/2.2973, 0.2236/2.1796, 0.2199/2.2025, 0.2205/2.1017, 0.2226/2.2630)

#------------------------------------------------
#Calculo de concentraciones
for (ii in 1:6){
  assign(paste0("ConStr4.", ii), eval(parse(text = paste0("(AbsStr4.", ii, "- Model.G$coefficients[1]) / Model.G$coefficients[2]"))))
}
for (ii in 1:3){
  assign(paste0("ConStr5.", ii), eval(parse(text = paste0("(AbsStr5.", ii, "- Model.B2$coefficients[1]) / Model.B2$coefficients[2]"))))
}
for (ii in 1:6){
  assign(paste0("ConFee4.", ii), eval(parse(
    text = paste0("c(mean(ConFeeT.0), (AbsFee4.", ii, "- Model.E2$coefficients[1]) / Model.E2$coefficients[2] / DilFee4.", ii, ")"))))
}
for (ii in 1:3){
  assign(paste0("ConFee5.", ii), eval(parse(
    text = paste0("c(mean(ConFeeT.0), (AbsFee5.", ii, "- Model.E2$coefficients[1]) / Model.E2$coefficients[2] / DilFee5.", ii, ")"))))
}
#-----------------------------------------------
#Perfiles de transporte
#pdf("Trans17-12-18.pdf", width=10, height=5)
for (ii in 1:4) {
  transporte <- eval(parse(text = paste0("data.frame(Tiempo = c(Time1, Time1), Conc = c(ConFee4.", ii, ", ConStr4.", ii, 
                                         "), Fase=rep(c('Alimentación', 'Recuperación'), each=length(Time1)))")))
  
  q <- ggplot(data=transporte, aes(x=Tiempo,y=Conc, group=Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
         geom_point(size = 3, aes(color=Fase)) + labs(y=expression(Concentración~Li~(mg~kg[Dis]^-1)), x="Tiempo (horas)")# +
         #geom_smooth(method = 'loess', aes(color=Fase))
  print(q)

  transporte <- data.table(transporte)
  transporte[Fase == "Alimentación", y_min := 15]
  transporte[Fase == "Alimentación", y_max := 18.5]
  transporte[Fase == "Recuperación", y_min := 0]
  transporte[Fase == "Recuperación", y_max := 2.5]
  q <- ggplot(data = transporte, aes(x = Tiempo, y = Conc, group = Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
         geom_point(size = 3, aes(color=Fase)) + labs(y=expression(Concentración~Li~(mg~kg[Dis]^-1)), x="Tiempo (horas)") + 
         facet_wrap(. ~ Fase, nrow = 2, scales = "free_y") + 
         theme(strip.background = element_blank(), strip.text = element_blank()) + 
         geom_blank(aes(y = y_min)) + geom_blank(aes(y = y_max))
  print(q)
}
for (ii in 5:6) {
  transporte <- eval(parse(text = paste0("data.frame(Tiempo = c(Time2, Time2), Conc = c(ConFee4.", ii, ", ConStr4.", ii, 
                                         "), Fase=rep(c('Alimentación', 'Recuperación'), each=length(Time2)))")))
  
  q <- ggplot(data=transporte, aes(x=Tiempo,y=Conc, group=Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
    geom_point(size = 3, aes(color=Fase)) + labs(y=expression(Concentración~Li~(mg~kg[Dis]^-1)), x="Tiempo (horas)")# +
  #geom_smooth(method = 'loess', aes(color=Fase))
  print(q)
  
  transporte <- data.table(transporte)
  transporte[Fase == "Alimentación", y_min := 15]
  transporte[Fase == "Alimentación", y_max := 18.5]
  transporte[Fase == "Recuperación", y_min := 0]
  transporte[Fase == "Recuperación", y_max := 2.5]
  q <- ggplot(data = transporte, aes(x = Tiempo, y = Conc, group = Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
    geom_point(size = 3, aes(color=Fase)) + labs(y=expression(Concentración~Li~(mg~kg[Dis]^-1)), x="Tiempo (horas)") + 
    facet_wrap(. ~ Fase, nrow = 2, scales = "free_y") + 
    theme(strip.background = element_blank(), strip.text = element_blank()) + 
    geom_blank(aes(y = y_min)) + geom_blank(aes(y = y_max))
  print(q)
}
#dev.off()

#pdf("Trans24-12-18.pdf", width=10, height=5)
for (ii in 1:3) {
  transporte <- eval(parse(text = paste0("data.frame(Tiempo = c(Time3, Time3), Conc = c(ConFee5.", ii, ", ConStr5.", ii, 
                                         "), Fase=rep(c('Alimentación', 'Recuperación'), each=length(Time3)))")))
  
  q <- ggplot(data=transporte, aes(x=Tiempo,y=Conc, group=Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
    geom_point(size = 3, aes(color=Fase)) + labs(y=expression(Concentración~Li~(mg~kg[Dis]^-1)), x="Tiempo (horas)")# +
  #geom_smooth(method = 'loess', aes(color=Fase))
  print(q)
  
  transporte <- data.table(transporte)
  transporte[Fase == "Alimentación", y_min := 15]
  transporte[Fase == "Alimentación", y_max := 18.5]
  transporte[Fase == "Recuperación", y_min := 0]
  transporte[Fase == "Recuperación", y_max := 2.5]
  q <- ggplot(data = transporte, aes(x = Tiempo, y = Conc, group = Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
    geom_point(size = 3, aes(color=Fase)) + labs(y=expression(Concentración~Li~(mg~kg[Dis]^-1)), x="Tiempo (horas)") + 
    facet_wrap(. ~ Fase, nrow = 2, scales = "free_y") + 
    theme(strip.background = element_blank(), strip.text = element_blank()) + 
    geom_blank(aes(y = y_min)) + geom_blank(aes(y = y_max))
  print(q)
  
}
#dev.off()


transporte <- data.table(transporte)
transporte[Fase == "Alimentación", y_min := 15]
transporte[Fase == "Alimentación", y_max := 18.5]
transporte[Fase == "Recuperación", y_min := 0]
transporte[Fase == "Recuperación", y_max := 2.5]
ggplot(data = transporte, aes(x = Tiempo, y = Conc, group = Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
  geom_point(size = 2, aes(color=Fase)) + labs(y=expression(Concentración~Li~(mg~kg[Dis]^-1)), x="Tiempo (horas)") + 
  facet_wrap(. ~ Fase, nrow = 2, scales = "free_y") + 
  theme(strip.background = element_blank(), strip.text = element_blank()) + 
  geom_blank(aes(y = y_min)) +
  geom_blank(aes(y = y_max))

