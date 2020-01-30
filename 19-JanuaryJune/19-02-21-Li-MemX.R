library(ggplot2)
cStock <- 129.5 * 0.187872 * 0.99 / 0.1200962
Std7   <- cStock * 0.7489 / 60.0825
CurvaB <- c(c(0.0000, 0.2375, 0.4852, 1.2425, 2.4417, 4.8009, 9.5967) * Std7 /
            c(12.0000, 11.9824, 11.9053, 12.0372, 11.9600, 12.0133, 11.9895), Std7)
Std7   <- cStock * 0.7540 / 60.2633
CurvaC <- c(c(0.0000, 0.2514, 0.4898, 1.2110, 2.3951, 4.8137, 9.6054) * Std7 /
            c(12.0000, 11.9823, 12.0021, 11.9666, 12.0722, 11.9614, 12.1221), Std7)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
AbsCurvaB <- c(0.000, 0.009, 0.017, 0.043, 0.086, 0.164, 0.315, 0.380)
plot(CurvaB, AbsCurvaB)
abline(lm(AbsCurvaB ~ CurvaB), col = 2)
ModelB <- lm(AbsCurvaB[1:7] ~ CurvaB[1:7]) 
abline(ModelB, col = 4)
#----------------------------------------------------------------------------
AbsCurvaC <- c(0.000, 0.008, 0.016, 0.038, 0.075, 0.157, 0.294, 0.355)
plot(CurvaC, AbsCurvaC)
abline(lm(AbsCurvaC ~ CurvaC), col = 2)
ModelC <- lm(AbsCurvaC[1:7] ~ CurvaC[1:7]) 
abline(ModelC, col = 4)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
Time1 <- c(0, 1, 4.25, 5.25, 9, 20.5, 21.5, 22)
Time2 <- c(0, 1, 2, 2.66, 4, 6, 7.66, 21, 22)
#----------------------------------------------------------------------------
pdf("Trans18-02-19.pdf", height = 5, width = 10)
#----------------------------------------------------------------------------
T_o_Feed_7.1 <- 0.9160 * cStock / 90.6956
Feed_7.1  <- (c(0.301, 0.230, 0.106, 0.083, 0.052, 0.033, 0.034, 0.033) - 
                ModelB$coefficients[1]) / ModelB$coefficients[2]
P_o_Feed_7.1 <- Feed_7.1[1]
Feed_7.1  <- Feed_7.1 / P_o_Feed_7.1
Strip_7.1 <- (c(0.001, 0.051, 0.181, 0.208, 0.244, 0.262, 0.263, 0.265) - 
                ModelC$coefficients[1]) / ModelC$coefficients[2] / P_o_Feed_7.1
#matplot(Time1, cbind(Feed_7.1, Strip_7.1), type = "p", col = c(1, 2), pch = 1, ylim = c(0, 1.1))
#curve((1.049 * x)/(2.78 + x), add=TRUE)
#curve((2.088 * x)/(3.000 + x), add=TRUE, col = 2)
#curve((1.088 * x)/(6.000 + x), add=TRUE, col = 4)


transporte <- data.frame(Tiempo = c(Time1, Time1), Conc = c(Feed_7.1, Strip_7.1), 
                         Fase=rep(c('Alimentación', 'Recuperación'), each=length(Time1)))

ggplot(data=transporte, aes(x=Tiempo,y=Conc, group=Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
  geom_point(size = 3, aes(color=Fase)) + labs(y="Fracción transportada de litio", x="Tiempo (horas)") +
  stat_function(fun = function(x) (0.9399 * x**2)/(1/0.1359 + x**2), color="darkgrey")
#geom_smooth(method = 'loess', aes(color=Fase))
#----------------------------------------------------------------------------
T_o_Feed_7.2 <- 0.9055 * cStock / 90.3865
Feed_7.2  <- (c(0.301, 0.235, 0.157, 0.143, 0.106, 0.081, 0.081, 0.080) - 
                ModelB$coefficients[1]) / ModelB$coefficients[2]
P_o_Feed_7.2 <- Feed_7.2[1]
Feed_7.2  <- Feed_7.2 / P_o_Feed_7.2
Strip_7.2 <- (c(0.001, 0.038, 0.120, 0.136, 0.179, 0.223, 0.218, 0.221) - 
                ModelC$coefficients[1]) / ModelC$coefficients[2] / P_o_Feed_7.2
#matplot(Time1, cbind(Feed_7.2, Strip_7.2), type = "p", col = c(1, 2), pch = 1, ylim = c(0, 1.1))
#curve((0.9888* x)/(5.5944+ x), add=TRUE)


transporte <- data.frame(Tiempo = c(Time1, Time1), Conc = c(Feed_7.2, Strip_7.2), 
                         Fase=rep(c('Alimentación', 'Recuperación'), each=length(Time1)))

ggplot(data=transporte, aes(x=Tiempo,y=Conc, group=Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
  geom_point(size = 3, aes(color=Fase)) + labs(y="Fracción transportada de litio", x="Tiempo (horas)") +
  stat_function(fun = function(x) (0.80003 * x**2)/(1/0.05885 + x**2), color="darkgrey")# +
#geom_smooth(method = 'loess', aes(color=Fase))
#----------------------------------------------------------------------------
T_o_Feed_7.3 <- 0.9074 * cStock / 90.0597
Feed_7.3  <- (c(0.303, 0.148, 0.152, 0.147, 0.150, 0.153, 0.148, 0.149, 0.148) - 
                ModelB$coefficients[1]) / ModelB$coefficients[2]
P_o_Feed_7.3 <- Feed_7.3[1]
Feed_7.3  <- Feed_7.3 / P_o_Feed_7.3

Strip_7.3 <- (c(0.001, 0.151, 0.147, 0.150, 0.151, 0.149, 0.150, 0.149, 0.149) - 
                ModelC$coefficients[1]) / ModelC$coefficients[2] / P_o_Feed_7.3
#matplot(Time2, cbind(Feed_7.3, Strip_7.3), type = "p", col = c(1, 2), pch = 1, ylim = c(0, 1.1))

transporte <- data.frame(Tiempo = c(Time2, Time2), Conc = c(Feed_7.3, Strip_7.3), 
                         Fase=rep(c('Alimentación', 'Recuperación'), each=length(Time2)))

ggplot(data=transporte, aes(x=Tiempo,y=Conc, group=Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
  geom_point(size = 3, aes(color=Fase)) + labs(y="Fracción transportada de litio", x="Tiempo (horas)")# +
#geom_smooth(method = 'loess', aes(color=Fase))
#----------------------------------------------------------------------------
T_o_Feed_7.4 <- 0.9074 * cStock / 90.0597
Feed_7.4  <- (c(0.303, 0.211, 0.166, 0.134, 0.082, 0.046, 0.035, 0.025, 0.025) - 
                ModelB$coefficients[1]) / ModelB$coefficients[2]
P_o_Feed_7.4 <- Feed_7.4[1]
Feed_7.4  <- Feed_7.4 / P_o_Feed_7.4
Strip_7.4 <- (c(0.001, 0.046, 0.095, 0.131, 0.187, 0.230, 0.245, 0.266, 0.268) - 
                ModelC$coefficients[1]) / ModelC$coefficients[2] / P_o_Feed_7.4
#matplot(Time2, cbind(Feed_7.4, Strip_7.4), type = "p", col = c(1, 2), pch = 1, ylim = c(0, 1.1))

#curve((1.1488 * x)/(1/0.2767 + x), add=TRUE)
#curve((0.9567 * x**2)/(1/0.1384 + x**2), add=TRUE, col = 2)
#curve(1 - (0.9275 * x**2)/(1/0.2710 + x**2), add=TRUE, col = 1)

#curve(1.032 * exp(- 1.958 / x), add = TRUE, col = 2)

nls(Strip_7.4 ~ (a * Time2)/(1/b + (Time2)), start = list(a = 1.2, b = 0.5))
nls(Strip_7.4 ~ (a * Time2**2)/(1/b + (Time2**2)), start = list(a = 1.2, b = 0.5))
nls(Feed_7.4 ~ 1 - (a * Time2**2)/(1/b + (Time2**2)), start = list(a = 1.2, b = 0.5))

nls(Strip_7.4 ~ a*exp(-b / Time2), start = list(a = 1, b = 1))
#nls(Strip_7.4 ~ log(a * Time2), start = list(a = 2))

transporte <- data.frame(Tiempo = c(Time2, Time2), Conc = c(Feed_7.4, Strip_7.4), 
                         Fase=rep(c('Alimentación', 'Recuperación'), each=length(Time2)))

ggplot(data=transporte, aes(x=Tiempo,y=Conc, group=Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
  geom_point(size = 3, aes(color=Fase)) + labs(y="Fracción transportada de litio", x="Tiempo (horas)") +
  stat_function(fun = function(x) (0.9567 * x**2)/(1/0.1384 + x**2), color="darkgrey")# +
#geom_smooth(method = 'loess', aes(color=Fase))

MaxStrips <- vector()
for (i in 1:4) {
  MaxStrips[i] <- max(get(paste0("Strip_7.", i)))
}



nls(Strip_7.1 ~ (a * Time1**2)/(1/b + (Time1**2)), start = list(a = 1, b = 0.5))
nls(Strip_7.2 ~ (a * Time1**2)/(1/b + (Time1**2)), start = list(a = 1, b = 0.5))
#nls(Strip_7.3 ~ (a * Time2**2)/(1/b + (Time2**2)), start = list(a = 0.5, b = 5000))
nls(Strip_7.4 ~ (a * Time2**2)/(1/b + (Time2**2)), start = list(a = 1, b = 0.5))


dev.off()


red<-(0.9399 * Time1**2)/(1/0.1359 + (Time1**2))
