library(ggplot2)
cStock<-109.0*0.187872*0.99/0.1004535
cStock1_20 <- cStock * 5.0167/100.0724
mStock<-c(0,0.1481,0.3082,0.7588,1.4906,3.0976,8.9740, 18.0127, 24.9839)
mFinal<-c(30, 29.9188, 30.0320, 29.7629, 29.6242, 30.0918, 30.0026, 30.0732, 30.1446)

ConcCurva <- mStock *cStock1_20 / mFinal
AbsCurva <- c(0.001,0.006,0.014,0.034,0.067,0.136,0.381,0.695,0.875)

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

L_1_2_1<-(0.426-CurvaEq$coefficients[[1]])/CurvaEq$coefficients[[2]] * 5.2454 / 0.9887
L_1_2_2<-(0.230-CurvaEq$coefficients[[1]])/CurvaEq$coefficients[[2]] * 4.9782 / 0.4947
L_1_2_3<-(0.220-CurvaEq$coefficients[[1]])/CurvaEq$coefficients[[2]] * 5.2368 / 0.4935
mean(c(L_1_2_1, L_1_2_2, L_1_2_3))

dilFaseExtrac_1<-c(0.4921/5.6475, 0.4962/5.1061, 0.4950/5.0402, 0.4959/5.1000, 0.5140/5.1138)
AbsFaseExtrac_1<-c(0.199, 0.221, 0.225, 0.221, 0.209)
ConcFaseExtrac_1<-(AbsFaseExtrac_1-CurvaEq$coefficients[1])/CurvaEq$coefficients[2]/dilFaseExtrac_1
C_0 <- mean(c(L_1_2_1, L_1_2_2, L_1_2_3))
masasdisextrac <- c(20.0062, 20.0350, 20.0167, 20.0377, 20.0110)
AmountLiext <- masasdisextrac*(ConcFaseExtrac_1-C_0)

ConcFaseRec_1<-(c(0.003, 0.002, 0.003, 0.009, 0.173)-CurvaEq$coefficients[1])/CurvaEq$coefficients[2]
masaFaseRec<-c(29.4476-14.5089, 29.7457-14.7796, 29.4241-14.4962, 29.6591-14.7917, 29.5410-14.6778)
ConcFaseRec_1*masaFaseRec # En microgramos
