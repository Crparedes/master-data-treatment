library(ggplot2)
cStock<-109.0*0.187872*0.99/0.1004535
mStock<-c(0,0.1233,0.2527,0.5276,0.7501,1.0061,1.2242,1.7201,2.4747)
mFinal<-c(50.0000,50.0987,50.3381,50.0258,50.0044,50.0676,50.3772,50.0084,50.0971)

ConcCurva<-mStock*cStock/mFinal
AbsCurva <-c(0.000,0.032,0.061,0.133,0.186,0.247,0.300,0.419,0.593)
sdAbsCurva<-c(0.0005,0.0004,0.0011,0.0019,0.0011,0.0049,0.0052,0.0095,0.0090)
Curva<-data.frame(Conc=ConcCurva, Abs=AbsCurva, sd=sdAbsCurva)

plot(ConcCurva,AbsCurva)
summary(lm(AbsCurva ~ ConcCurva))
arrows(ConcCurva,AbsCurva-sdAbsCurva,ConcCurva,AbsCurva+sdAbsCurva)
ggplot(data=Curva, aes(x=Conc,y=Abs)) + geom_smooth(method='lm',alpha=0.999,size=0.5) + geom_point() + 
  labs(x=expression(Concentración~Li~(mg~kg[Disolución]^-1)), y="Absorbancia (UA)")



AbsMues <- c(0.121,0.121,0.149)
ConMues <- (AbsMues-lm(AbsCurva ~ ConcCurva)$coefficients[[1]])/lm(AbsCurva ~ ConcCurva)$coefficients[[2]]
DilMues <- c(24.5847/0.4849,25.0566/0.4972,25.4889/0.5807)

MasMues <- c(71.2439-71.1425,71.8366-71.7365,71.7546-71.6544)
MasFinal<- c(172.6075-71.1425,172.0750-71.7365,172.2290-71.6544)
Litio <- ConMues*DilMues*(MasFinal/1000)/(MasMues*1000)*100
