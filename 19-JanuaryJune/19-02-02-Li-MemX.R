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
AbsCurvaA <- c(0.000, 0.009, 0.019, 0.046, 0.095, 0.188, 0.364, 0.450)
plot(CurvaA, AbsCurvaA)
abline(lm(AbsCurvaA ~ CurvaA), col = 2)
ModelA <- lm(AbsCurvaA[1:7] ~ CurvaA[1:7]) 
abline(ModelA, col = 4)
#----------------------------------------------------------------------------
AbsCurvaC <- c(0.000, 0.009, 0.019, 0.046, 0.091, 0.172, 0.336, 0.416)
plot(CurvaC, AbsCurvaC)
abline(lm(AbsCurvaC ~ CurvaC), col = 2)
ModelC <- lm(AbsCurvaC[1:7] ~ CurvaC[1:7]) 
abline(ModelC, col = 4)
#----------------------------------------------------------------------------
AbsCurvaD <- c(0.000, 0.008, 0.016, 0.034, 0.079, 0.156, 0.305, 0.374)
plot(CurvaD, AbsCurvaD)
abline(lm(AbsCurvaD ~ CurvaD), col = 2)
ModelD <- lm(AbsCurvaD[1:7] ~ CurvaD[1:7]) 
abline(ModelD, col = 4)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
Time <- c(0, 1, 2, 3, 4, 6, 8, 22, 24)
#----------------------------------------------------------------------------
T_o_Feed_6.1 <- 0.8992 * cStock / 90.1283
Feed_6.1  <- (c(0.355, 0.346, 0.355, 0.352, 0.354, 0.359, 0.357, 0.364, 0.361) - 
                ModelA$coefficients[1]) / ModelA$coefficients[2]
P_o_Feed_6.1 <- mean(Feed_6.1)
Feed_6.1  <- Feed_6.1 / P_o_Feed_6.1
Strip_6.1 <- (c(0.000, 0.006, 0.009, 0.010, 0.010, 0.009, 0.009, 0.010, 0.010) - 
                ModelC$coefficients[1]) / ModelC$coefficients[2] / P_o_Feed_6.1
matplot(Time, cbind(Feed_6.1, Strip_6.1), type = "p", col = c(1, 2), pch = 1, ylim = c(0, 1.1))   

transporte <- data.frame(Tiempo = c(Time, Time), Conc = c(Feed_6.1, Strip_6.1), 
              Fase=rep(c('Alimentación', 'Recuperación'), each=length(Time)))

ggplot(data=transporte, aes(x=Tiempo,y=Conc, group=Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
       geom_point(size = 3, aes(color=Fase)) + labs(y="Fracción transportada de litio", x="Tiempo (horas)")# +
#geom_smooth(method = 'loess', aes(color=Fase))


#----------------------------------------------------------------------------
T_o_Feed_6.2 <- 0.8992 * cStock / 91.9126
Feed_6.2  <- (c(0.344, 0.342, 0.335, 0.337, 0.340, 0.344, 0.348, 0.351, 0.352) - 
                ModelA$coefficients[1]) / ModelA$coefficients[2]
P_o_Feed_6.2 <- mean(Feed_6.2)
Feed_6.2  <- Feed_6.2 / P_o_Feed_6.2
Strip_6.2 <- (c(0.000, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.006, 0.007) - 
                ModelD$coefficients[1]) / ModelD$coefficients[2] / P_o_Feed_6.2
matplot(Time, cbind(Feed_6.2, Strip_6.2), type = "p", col = c(1, 2), pch = 1, ylim = c(0, 1.1))

transporte <- data.frame(Tiempo = c(Time, Time), Conc = c(Feed_6.2, Strip_6.2), 
                         Fase=rep(c('Alimentación', 'Recuperación'), each=length(Time)))

ggplot(data=transporte, aes(x=Tiempo,y=Conc, group=Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
  geom_point(size = 3, aes(color=Fase)) + labs(y="Fracción transportada de litio", x="Tiempo (horas)")# +
#geom_smooth(method = 'loess', aes(color=Fase))
