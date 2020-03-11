library(ggplot2)
cStock<-109.0*0.187872*0.99/0.1004535
mStock<-c(0,0.1233,0.2527,0.5276,0.7501,1.0061,1.2242,1.7201,2.4747)
mFinal<-c(50.0000,50.0987,50.3381,50.0258,50.0044,50.0676,50.3772,50.0084,50.0971)

ConcCurva<-mStock*cStock/mFinal
Std1_2<-ConcCurva[2]*2.1049/4.1824
Std1_4<-ConcCurva[2]*1.0461/4.0821
Std1_8<-ConcCurva[2]*0.5312/4.3369
Std1_16<-ConcCurva[2]*0.2941/4.3005
ConcCurva<-c(ConcCurva[1],Std1_16,Std1_8,Std1_4,Std1_2,ConcCurva[2:length(ConcCurva)])

AbsCurva <-c(0,0.006,0.010,0.021,0.042,0.084,0.171,0.358,0.483,0.632,0.745,0.982,1.285)
#sdAbsCurva <-c()
Curva<-data.frame(Conc=ConcCurva, Abs=AbsCurva)
CurvaBaja<-data.frame(Conc=ConcCurva[1:8], Abs=AbsCurva[1:8])
CurvaAlta<-data.frame(Conc=ConcCurva[8:13], Abs=AbsCurva[8:13])

plot(CurvaBaja)
plot(CurvaAlta)
summary(lm(AbsCurva ~ ConcCurva))
#arrows(ConcCurva,AbsCurva-sdAbsCurva,ConcCurva,AbsCurva+sdAbsCurva)
ggplot(data=CurvaBaja, aes(x=Conc,y=Abs)) + geom_smooth(method='lm',alpha=0.999,size=0.5) + geom_point() +
  labs(x=expression(Concentración~Li~(mg~kg[Disolución]^-1)), y="Absorbancia (UA)")
ggplot(data=CurvaAlta, aes(x=Conc,y=Abs)) + geom_smooth(method='lm',alpha=0.95,size=0.5,data=subset(CurvaAlta,Conc<8)) + geom_point() +
  labs(x=expression(Concentración~Li~(mg~kg[Disolución]^-1)), y="Absorbancia (UA)")

Cbaja<-(lm(Abs~Conc,data=CurvaBaja))
Calta<-(lm(Abs~Conc,data=CurvaAlta,subset = 1:5))
summary(Cbaja)

LixiB1<-(0.391-Calta$coefficients[1])/Calta$coefficients[2] *5.6613/0.1182
LixiB2<-(0.678-Calta$coefficients[1])/Calta$coefficients[2] *6.7793/0.2605
LixiB<-mean(c(LixiB1,LixiB2))

ConcFaseExtrac_0<-c(10.0834/25.5817,10.0908/25.5619,10.1279/25.5537,10.1073/25.6479,10.0876/25.5549,10.1063/25.5820,10.0890/25.5612)*LixiB
dilFaseExtrac_1<-c(0.5542/5.5318,0.5470/5.4461,0.5370/5.8439,0.5561/5.4137,0.5465/5.4240,0.5454/5.7109,0.5422/5.4627)
AbsFaseExtrac_1<-c(0.686,0.888,0.654,0.721,0.711,0.679,0.690)
ConcFaseExtrac_1<-(AbsFaseExtrac_1-Calta$coefficients[1])/Calta$coefficients[2]*(1/dilFaseExtrac_1)

ConcFaseRec_1<-(c(0.030,0.056,0.005,0.008,0.010,0.004,0.000)-Cbaja$coefficients[1])/Cbaja$coefficients[2]
masaFaseRec<-c(54.5503-14.7740,54.4400-14.5355,54.7931-14.7907,54.4747-14.5651,54.5368-14.5959,54.8379-14.4990,54.2937-14.5161)
MasLitRec<-ConcFaseRec_1*masaFaseRec # En microgramos
44*1000*0.025
