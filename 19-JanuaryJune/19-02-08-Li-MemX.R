library(ggplot2)
cStock <- 129.5 * 0.187872 * 0.99 / 0.1200962
Std7   <- cStock * 0.7495 / 60.1995
CurvaA <- c(c(0.0000, 0.2565, 0.4725, 1.2025, 2.4371, 4.8029, 9.6060) * Std7 /
            c(12.0000, 12.0554, 11.8008, 12.0137, 11.9934, 11.9848, 12.0406), Std7)
Std7   <- cStock * 0.7489 / 60.0825
CurvaB <- c(c(0.0000, 0.2375, 0.4852, 1.2425, 2.4417, 4.8009, 9.5967) * Std7 /
            c(12.0000, 11.9824, 11.9053, 12.0372, 11.9600, 12.0133, 11.9895), Std7)
Std7   <- cStock * 0.7540 / 60.2633
CurvaC <- c(c(0.0000, 0.2514, 0.4898, 1.2110, 2.3951, 4.8137, 9.6054) * Std7 /
            c(12.0000, 11.9823, 12.0021, 11.9666, 12.0722, 11.9614, 12.1221), Std7)
Std7   <- cStock * 0.7495 / 60.1995
CurvaD <- c(c(0.0000, 0.2536, 0.4866, 1.2120, 2.4127, 4.8050, 9.6065) * Std7 /
            c(12.0000, 11.9890, 12.0140, 12.0138, 11.9879, 12.0224, 12.1904), Std7)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
AbsCurvaA <- c(0.000, 0.009, 0.017, 0.041, 0.082, 0.160, 0.316, 0.382)
plot(CurvaA, AbsCurvaA)
abline(lm(AbsCurvaA ~ CurvaA), col = 2)
ModelA <- lm(AbsCurvaA[1:7] ~ CurvaA[1:7]) 
abline(ModelA, col = 4)
#----------------------------------------------------------------------------
AbsCurvaD <- c(0.000, 0.007, 0.014, 0.034, 0.068, 0.132, 0.258, 0.324)
plot(CurvaD, AbsCurvaD)
abline(lm(AbsCurvaD ~ CurvaD), col = 2)
ModelD <- lm(AbsCurvaD[1:7] ~ CurvaD[1:7]) 
abline(ModelD, col = 4)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
Time <- c(0, 1, 2, 3.5, 5, 7, 21.66, 23)
#----------------------------------------------------------------------------
T_o_Feed_6.7 <- 4.4730 * cStock / 90.1478
Feed_6.7  <- (c(0.151 * 2.19165 / 0.22062, 0.148 * 2.20925 / 0.21093) - 
                ModelA$coefficients[1]) / ModelA$coefficients[2]
P_o_Feed_6.7 <- mean(Feed_6.7)
Feed_6.7  <- Feed_6.7 / P_o_Feed_6.7
Strip_6.7 <- (c(0.000, 0.000, 0.001, 0.001, 0.001, 0.002, 0.003, 0.004) - 
                ModelD$coefficients[1]) / ModelD$coefficients[2] / P_o_Feed_6.7
plot(Time, Strip_6.7, type = "p", col = c(2), pch = 1, ylim = c(0, 1.1))      


#----------------------------------------------------------------------------
T_o_Feed_6.8 <- 4.4834 * cStock / 90.0252
DilF_6.8  <- c(2.23250 / 0.20545, 2.27705 / 0.21674, 2.10987 / 0.22194, 2.09980 / 0.22334,
               2.29770 / 0.22360, 2.05000 / 0.22611, 2.12239 / 0.22209, 2.03763 / 0.22609)
Feed_6.8  <- (c(0.143, 0.133, 0.139, 0.132, 0.118, 0.137, 0.135, 0.140) - 
                ModelA$coefficients[1]) / ModelA$coefficients[2] * DilF_6.8
P_o_Feed_6.8 <- Feed_6.8[1]
Feed_6.8  <- Feed_6.8 / P_o_Feed_6.8
Strip_6.8 <- (c(0.000, 0.070, 0.141, 0.223, 0.264, 0.279, 0.282, 0.281) - 
                ModelD$coefficients[1]) / ModelD$coefficients[2] / P_o_Feed_6.8
matplot(Time, cbind(Feed_6.8, Strip_6.8), type = "p", col = c(1, 2), pch = 1, ylim = c(0, 1.1))

transporte <- data.frame(Tiempo = c(Time, Time), Conc = c(Feed_6.8, Strip_6.8), 
                         Fase=rep(c('Alimentación', 'Recuperación'), each=length(Time)))

ggplot(data=transporte, aes(x=Tiempo,y=Conc, group=Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
  geom_point(size = 3, aes(color=Fase)) + labs(y="Fracción transportada de litio", x="Tiempo (horas)")# +
#geom_smooth(method = 'loess', aes(color=Fase))

#plot(Curva)
#ggplot(data=Curva, aes(x=Conc,y=Abs)) + geom_smooth(method='lm',alpha=0.999,size=0.5) + 
#  geom_point() + 
#  labs(x=expression(Concentración~Li~(mg~kg[Disolución]^-1)), y="Absorbancia (UA)")

