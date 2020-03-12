Na2CO3.20.08.19 <- 0.2192 / 105.9888 / 0.0275208
HCl.20.08.19 <- c(2.0760, 3.1323) * Na2CO3.20.08.19 * 2 / c(4.6350, 6.8687)
mean(HCl.20.08.19);sd(HCl.20.08.19)
HCl.20.08.19 <- c(2.0572, 2.0876, 1.1161) * Na2CO3.20.08.19 * 2 / c(4.0032, 4.1047, 2.1690)
mean(HCl.20.08.19);sd(HCl.20.08.19)
StipF.14.3 <- c(2.0854, 3.1031) * Na2CO3.20.08.19 * 2 / c(3.9295, 5.8272)
mean(StipF.14.3);sd(StipF.14.3)
StipF.14.4 <- c(2.0820, 3.1248) * Na2CO3.20.08.19 * 2 / c(4.3983, 6.5300)
mean(StipF.14.4);sd(StipF.14.4)
StipF.14.5 <- c(2.0942, 3.1297) * Na2CO3.20.08.19 * 2 / c(4.9796, 7.3030)
mean(StipF.14.5);sd(StipF.14.5)
StipF.14.7 <- c(2.0746, 1.0357) * Na2CO3.20.08.19 * 2 / c(9.8273, 4.9247)
mean(StipF.14.7);sd(StipF.14.7)

NH4OH.20.08.19 <- c(1.4389, 0.7633, 1.6725) * mean(HCl.20.08.19) / c(10.4195, 5.4177, 12.2071)
mean(NH4OH.20.08.19); sd(NH4OH.20.08.19)
HCl_1_10 <- mean(HCl.20.08.19) * 5.1190 / 50.0143
FeedF.14.3 <- c(1.5617) * c(HCl_1_10) / c(4.5251)
mean(FeedF.14.3)
FeedF.14.4 <- c(1.8089, 2.5476, 0.2304) * c(HCl_1_10, HCl_1_10, mean(HCl.20.08.19)) / c(5.7929, 7.3496, 6.3922)
mean(FeedF.14.4); sd(FeedF.14.4)
FeedF.14.5 <- c(0.3812, 3.5907) * c(mean(HCl.20.08.19), HCl_1_10) / c(10.7240, 9.6965)
mean(FeedF.14.5); sd(FeedF.14.5)

(mean(NH4OH.20.08.19) - c(mean(FeedF.14.3), mean(FeedF.14.4), mean(FeedF.14.5)))/mean(NH4OH.20.08.19)


source(file = "19-08-17-LiNa-Mem14.R")
AliConc$Strip.14.3.Li[6] / 6.94 / 1000 + AliConc$Strip.14.3.Na[3] / 23 / 1000
AliConc$Strip.14.4.Li[6] / 6.94 / 1000 + AliConc$Strip.14.4.Na[3] / 23 / 1000
AliConc$Strip.14.5.Li[6] / 6.94 / 1000 + AliConc$Strip.14.5.Na[3] / 23 / 1000
AliConc$Strip.14.7.Li[6] / 6.94 / 1000 + AliConc$Strip.14.7.Na[3] / 23 / 1000

0.077*0.085
