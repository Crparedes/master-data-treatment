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
AbsCurvaB <- c(0.000, 0.008, 0.017, 0.044, 0.085, 0.164, 0.320, 0.379)
plot(CurvaB, AbsCurvaB)
abline(lm(AbsCurvaB ~ CurvaB), col = 2)
ModelB <- lm(AbsCurvaB[1:7] ~ CurvaB[1:7]) 
abline(ModelB, col = 4)
#----------------------------------------------------------------------------
AbsCurvaC <- c(0.000, 0.008, 0.016, 0.038, 0.075, 0.152, 0.280, 0.360)
plot(CurvaC, AbsCurvaC)
abline(lm(AbsCurvaC ~ CurvaC), col = 2)
ModelC <- lm(AbsCurvaC ~ CurvaC)  #No se descarta el último valor en este caso
abline(ModelC, col = 4)
#----------------------------------------------------------------------------
AbsCurvaD <- c(0.000, 0.007, 0.013, 0.034, 0.068, 0.134, 0.264, 0.327)
plot(CurvaD, AbsCurvaD)
abline(lm(AbsCurvaD ~ CurvaD), col = 2)
ModelD <- lm(AbsCurvaD[1:7] ~ CurvaD[1:7]) 
abline(ModelD, col = 4)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
Time1 <- c(0, 1, 2, 3, 5, 7, 20, 21)
#----------------------------------------------------------------------------
T_o_Feed_6.3 <- 0.9039 * cStock / 93.1411
Feed_6.3  <- (c(0.312, 0.253, 0.229, 0.205, 0.166, 0.137, 0.052, 0.049) - 
                ModelB$coefficients[1]) / ModelB$coefficients[2]
P_o_Feed_6.3 <- Feed_6.3[1]
Feed_6.3  <- Feed_6.3 / P_o_Feed_6.3
Strip_6.3 <- (c(0.000, 0.062, 0.086, 0.108, 0.144, 0.177, 0.247, 0.243) - 
                ModelC$coefficients[1]) / ModelC$coefficients[2] / P_o_Feed_6.3
matplot(Time1, cbind(Feed_6.3, Strip_6.3), type = "p", col = c(1, 2), pch = 1, ylim = c(0, 1.1))        

transporte <- data.frame(Tiempo = c(Time1, Time1), Conc = c(Feed_6.3, Strip_6.3), 
                         Fase=rep(c('Alimentación', 'Recuperación'), each=length(Time1)))

ggplot(data=transporte, aes(x=Tiempo,y=Conc, group=Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
  geom_point(size = 3, aes(color=Fase)) + labs(y="Fracción transportada de litio", x="Tiempo (horas)")# +
#geom_smooth(method = 'loess', aes(color=Fase))

#----------------------------------------------------------------------------
T_o_Feed_6.4 <- 4.4855 * cStock / 90.4758
Strip_6.4 <- (c(0.000, 0.000, 0.001, 0.001, 0.002, 0.003, 0.007, 0.007) - 
                ModelC$coefficients[1]) / ModelC$coefficients[2] / T_o_Feed_6.4
plot(Time1, Strip_6.4, type = "p", col = c(2), pch = 1, ylim = c(0, 1.1))                

transporte <- data.frame(Tiempo = Time1, Conc = Strip_6.4, 
                         Fase=rep(c('Recuperación'), each=length(Time1)))

ggplot(data=transporte, aes(x=Tiempo,y=Conc, group=Fase)) + #geom_smooth(alpha=0.999,size=0.5)
  geom_point(size = 3, aes(color=Fase)) + ylim(-0.01, 1) + labs(y="Fracción transportada de litio", x="Tiempo (horas)")# +
#geom_smooth(method = 'loess', aes(color=Fase))

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
Time2 <- c(0, 1.5, 3.25, 4, 6, 8, 21, 24)
#----------------------------------------------------------------------------
T_o_Feed_6.5 <- 4.4925 * cStock / 90.0389
Strip_6.5 <- (c(0.001, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.001) - 
                ModelC$coefficients[1]) / ModelC$coefficients[2] / T_o_Feed_6.5
plot(Time2, Strip_6.5, type = "p", col = c(2), pch = 1, ylim = c(0, 1.1))                
#----------------------------------------------------------------------------
T_o_Feed_6.6 <- 0.9056 * cStock / 90.0657
Strip_6.6 <- (c(0.000, 0.000, 0.000, 0.002, 0.003, 0.003, 0.006, 0.006) - 
                ModelC$coefficients[1]) / ModelC$coefficients[2] / T_o_Feed_6.6
plot(Time2, Strip_6.6, type = "p", col = c(2), pch = 1, ylim = c(0, 1.1))                

transporte <- data.frame(Tiempo = Time1, Conc = Strip_6.5, 
                         Fase=rep(c('Recuperación'), each=length(Time1)))

ggplot(data=transporte, aes(x=Tiempo,y=Conc, group=Fase)) + #geom_smooth(alpha=0.999,size=0.5)
  geom_point(size = 3, aes(color=Fase)) + ylim(-0.01, 1) + labs(y="Fracción transportada de litio", x="Tiempo (horas)")# +
#geom_smooth(method = 'loess', aes(color=Fase))

#plot(Curva)
#ggplot(data=Curva, aes(x=Conc,y=Abs)) + geom_smooth(method='lm',alpha=0.999,size=0.5) + 
#  geom_point() + 
#  labs(x=expression(Concentración~Li~(mg~kg[Disolución]^-1)), y="Absorbancia (UA)")

