library(FrF2)
#variables <- c('CTA', 'NPOE', 'LIX-54', 'Cyanex', 'NaOH', 'HCl', 'Li')

variables <- list(X1_CTA = c(30, 80), X2_NPOE = c(20, 40), X3_LIX = c(12, 50), X4_Cyanex = c(12, 50), 
                  X5_NaOH = c(10, 1), X7_HCl = c(0.5, 0.1), X6_Li = c(2, 10))
variables1 <- list(CTA = c(30, 80), NPOE = c(20, 40), LIX = c(12, 50), Cyanex = c(12, 50), 
                   NaOH = c(10, 1), HCl = c(0.5, 0.1), Li = c(2, 10))

FrF2(8, factor.names = variables, seed = 12, randomize = FALSE)
(Dessign <- FrF2(8, factor.names = variables, seed = 12))
(Dessign1 <- FrF2(8, factor.names = variables1, seed = 12))

# Run scripts associated with the lithium cuantification corresponding to these days...
source("19-JanuaryJune/19-02-02-Li-MemX.R")
source("19-JanuaryJune/19-02-06-Li-MemX.R")
source("19-JanuaryJune/19-02-08-Li-MemX.R")

# Order got messed up in frf2 package D:
order <- c(4, 5, 7, 8, 1, 3, 2, 6)
MaxStrips <- IniFeed <- vector()
for (i in 1:8) {
  MaxStrips[i] <- max(get(paste0("Strip_6.", i)))
  IniFeed[i]   <- max(get(paste0("T_o_Feed_6.", i)))
}
Eficiencia <- MaxStrips[order]

Des.Resp <- add.response(Dessign, 100*Eficiencia)
Des.Resp1 <- add.response(Dessign1, 100*Eficiencia)
#IAPlot(Des.Resp)
MEPlot(Des.Resp)
halfnormal(Des.Resp, code = TRUE, alpha = 0.10)
anova(lm(X100...Eficiencia ~ X1_CTA + X2_NPOE + X3_LIX + X4_Cyanex + X5_NaOH + X7_HCl + X6_Li, data = Des.Resp))
anova(lm(X100...Eficiencia ~ X3_LIX, data = Des.Resp))
anova(lm(X100...Eficiencia ~ X1_CTA, data = Des.Resp))
anova(lm(X100...Eficiencia ~ X5_NaOH, data = Des.Resp))
anova(lm(X100...Eficiencia ~ X1_CTA + X3_LIX + X5_NaOH, data = Des.Resp))
anova(lm(X100...Eficiencia ~ X1_CTA + X3_LIX, data = Des.Resp))
anova(lm(X100...Eficiencia ~ X1_CTA + X5_NaOH, data = Des.Resp))
anova(lm(X100...Eficiencia ~ X3_LIX + X5_NaOH, data = Des.Resp))

anova(lm(X100...Eficiencia ~ X1_CTA + X2_NPOE + X3_LIX + X4_Cyanex + X5_NaOH + X7_HCl + X6_Li, data = Des.Resp))


source("19-JanuaryJune/customMEPlot.R")
cMEPlot(Des.Resp)

source("custom-DanielPlot.R")
DanielPlot(Des.Resp1, main = '', code = FALSE, alpha = 0.1, autolab = FALSE, 
           pch = c(5, 5, 5, 5, 5, 5, 5), cex.pch = 1.2, half = TRUE)
abline(a = -2, b = 0.12, lty = 2)

plot(Des.Resp)
#0.01*0.25*39997
#stdNa <- c(150, 100, 50, 20, 10, 5, 2)
#stockNA <- 0.0861 * 39997
#10*200/stockNA
#sum(5*stdNa/200)
