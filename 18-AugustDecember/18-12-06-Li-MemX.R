library(ggplot2)
cStock<-109.0*0.187872*0.99/0.1004535
cStock1_20 <- cStock * 5.0167/100.0724
mStock<-c(0,0.1481,0.3082,0.7588,1.4906,3.0976,8.9740, 18.0127, 24.9839)
mFinal<-c(30, 29.9188, 30.0320, 29.7629, 29.6242, 30.0918, 30.0026, 30.0732, 30.1446)

ConcCurva <- mStock *cStock1_20 / mFinal
AbsCurva <- c(0.001,0.007,0.014,0.029,0.056,0.116,0.331,0.627,0.790)

#sdAbsCurva <-c()
Curva<-data.frame(Conc=ConcCurva[1:7], Abs=AbsCurva[1:7])

summary(lm(Abs~Conc, data=data.frame(Conc=ConcCurva, Abs=AbsCurva)))
summary(lm(Abs~Conc, data=data.frame(Conc=ConcCurva[1:8], Abs=AbsCurva[1:8])))
summary(lm(Abs~Conc, data=data.frame(Conc=ConcCurva[1:7], Abs=AbsCurva[1:7])))



plot(Curva)
ggplot(data=Curva, aes(x=Conc,y=Abs)) + geom_smooth(method='lm',alpha=0.999,size=0.5) + 
  geom_point() + 
  labs(x=expression(Concentración~Li~(mg~kg[Disolución]^-1)), y="Absorbancia (UA)")

CurvaEq <- (lm(Abs~Conc, data=Curva))
summary(CurvaEq)


dilFaseAlim_1<-c(0.4911/4.9953,0.4879/4.9566,0.4840/4.9512,0.4900/5.3730,0.4803/4.7938,
                   0.4887/4.8881,0.4886/4.7573,0.4717/5.0378,0.4907/4.9490,0.4870/4.9549,
                   0.4761/5.3853,0.4878/4.8437,0.4881/5.0773,0.4989/4.9126)#,0.4911/5.1591,
                   #0.4982/5.1750)
                   
AbsFaseAlim_1<-c(0.148,0.147,0.144,0.134,0.147,0.145,0.151,0.133,0.141,0.139,0.124,0.141,0.134,0.144)
ConcFaseAlim_1<-(AbsFaseAlim_1-CurvaEq$coefficients[1])/CurvaEq$coefficients[2]/dilFaseAlim_1

AbsFaseRec_1<-c(0.000,0.003,0.006,0.013,0.027,0.036,0.047,0.053,0.062,0.068,0.078,0.092,0.099,0.101)
ConcFaseRec_1<-(AbsFaseRec_1-CurvaEq$coefficients[1])/CurvaEq$coefficients[2]

Time <- c(0,20,40,60,120,160,200,240,280,320,380,440,500,520)/60
plot(Time, ConcFaseRec_1)

matplot(Time, cbind(ConcFaseRec_1,ConcFaseAlim_1), type="o", pch = c(1,2))

transporte<-data.frame(Tiempo=c(Time,Time), Conc=c(ConcFaseAlim_1,ConcFaseRec_1)/max(ConcFaseAlim_1), 
                       Fase=rep(c("Alimentación", "Recuperación"), each=length(Time)))
ggplot(data=transporte, aes(x=Tiempo,y=Conc, group=Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
  geom_point(size = 3, aes(color=Fase)) + labs(y=expression(Concentración~Li~(mg~kg[Dis]^-1)), x="Tiempo (horas)") +
  geom_smooth(aes(color=Fase))

ggplot(data=transporte, aes(x=Tiempo,y=Conc, group=Fase)) + #geom_smooth(alpha=0.999,size=0.5) + 
  geom_point(aes(color=Fase)) + labs(y=expression(Concentración~Li~(mg~kg[Dis]^-1)), x="Tiempo (horas)")+ 
  facet_wrap(. ~ Fase, scales = "free")
