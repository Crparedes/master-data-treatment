Na2CO3.18.10.19 <- 1.1966  / 105.9888 / 0.0302849
HCl.18.10.19 <- c(5.2526, 5.1579, 5.1238) * Na2CO3.18.10.19 * 2 / c(1.8442, 1.8029, 1.8061)
mean(HCl.18.10.19);sd(HCl.18.10.19)

Strip.18_0 <- c(5.0001, 5.0118, 4.9867) *Na2CO3.18.10.19*5.2723/52.7882*2 / c(3.9628, 3.8309, 3.8249)
mean(Strip.18_0[-1]);sd(Strip.18_0[-1])
StipF.18.1A <- c(4.9254, 4.9579) *Na2CO3.18.10.19*5.2723/52.7882*2 / c(5.9149, 5.9244)
mean(StipF.18.1A);sd(StipF.18.1A)
StipF.18.1B <- c(4.9293, 4.9273, 4.9423) *Na2CO3.18.10.19*5.2723/52.7882*2 / c(4.8964, 4.8702, 4.8746)
mean(StipF.18.1B);sd(StipF.18.1B)

NH4OH.18.10.19 <- c(1.2895, 1.8318) * mean(HCl.18.10.19) / c(3.3362, 4.8244)
mean(NH4OH.18.10.19); sd(NH4OH.18.10.19)

Feed.18_0 <- c(0.7096, 2.1501, 3.2938) * mean(Strip.18_0) / c(5.0028, 14.9634, 22.7678)
mean(Feed.18_0);sd(Feed.18_0)
FeedF.18.1A <- c(0.1501, 0.3021) * mean(Strip.18_0) / c(10.0034, 22.1596)
mean(FeedF.18.1A); sd(FeedF.18.1A)
FeedF.18.1B <- c(0.7125, 0.7250) * mean(Strip.18_0) / c(15.6395, 16.7448)
mean(FeedF.18.1B); sd(FeedF.18.1B)

(mean(Feed.18_0) - c(mean(FeedF.18.1A), mean(FeedF.18.1B)))/mean(Feed.18_0)

(mean(Strip.18_0) - c(mean(StipF.18.1A), mean(StipF.18.1B)))/mean(Strip.18_0)


#source(file = "19-08-17-LiNa-Mem14.R")
#AliConc$Strip.14.3.Li[6] / 6.94 / 1000 + AliConc$Strip.14.3.Na[3] / 23 / 1000
#AliConc$Strip.14.4.Li[6] / 6.94 / 1000 + AliConc$Strip.14.4.Na[3] / 23 / 1000
#AliConc$Strip.14.5.Li[6] / 6.94 / 1000 + AliConc$Strip.14.5.Na[3] / 23 / 1000
#AliConc$Strip.14.7.Li[6] / 6.94 / 1000 + AliConc$Strip.14.7.Na[3] / 23 / 1000

#0.077*0.085

s <- c(0.16, 0.25, 0.40, 0.70, 1.00) * 10^{-2}
r <- c(2.23, 2.82, 3.48, 4.17, 4.45) * 10^{-5}
is <- 1/s
ir <- 1/r
plot(ir~is)
abline(lm(ir~is))
summary(lm(ir~is))
library(propagate)
propagate(expr = expression(1 / int), data = cbind(int = c(1.803e+04, 1.642e+02)))
propagate(expr = expression(b / int), data = cbind(int = c(1.803e+04, 1.642e+02), b = c(4.30e+01, 5e-01)))

Vmax1 <- c(5.55e-5, 5e-7)
Km1 <- c(2.38e-3, 4e-5)

summary(nls(r~(a*s)/(b+s), data = data.frame(s = s, r = r), start = list(a = 1/1.803e4, b = 4.304e1*1/1.803e4)))

plot(r~s)

Cl  <- 0.54922; SO4 <- 0.02824
Na  <- 0.46911; Mg  <- 0.05283; Ca  <- 0.01036; K   <- 0.01021 
StockLi.200_2 <- 130.3 * 0.187872 * 0.99 / 0.1205105

Na+K+2*(Mg-SO4)+2*Ca - Cl
2*SO4*246.4746
2*Na*58.44
2*K * 74.5513
2* Ca * 147.0146
(Mg-SO4) * 84.3139 * 2
(Mg-SO4) * 2 * 2 / 6 * 1000
Li_m<-0.18e-3    
Li_m/6.941
(LiCl<- 0.18e-3 / 0.16372 * 2)
0.18*2000/StockLi.200_2
