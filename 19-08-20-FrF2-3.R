library(FrF2)
library(transMem)
#variables <- c('CTA', 'NPOE', 'LIX-54', 'Cyanex', 'NaOH', 'HCl', 'Li')

variables <- list(Dish = c("Pyrex", "NN"), Sup.feed = c("Air", "Glass"),
                  Solvent = c("DCM", "DCM.EtOH"))
(Dessign <- FrF2(nfactors = 3, seed = 666, resolution = 3, factor.names = variables))

##########################
##### CONTINUE HERE ######

# Run scripts associated with the lithium cuantification corresponding to these days...
#source("19-06-12-LiNa-Mem12.R")

Des.Resp1 <- add.response(Dessign, Parameters$alpha)
#IAPlot(Des.Resp1)
#MEPlot(Des.Resp1)
plot(Des.Resp1)
#halfnormal(Des.Resp1)
anova(lm(Parameters.alpha ~ CTA + Extr + molRat + NH4OH_f + HCl_s, data = Des.Resp1))


Des.Resp2 <- add.response(Dessign, Parameters$beta)
#IAPlot(Des.Resp2)
#MEPlot(Des.Resp2)
plot(Des.Resp2)
#halfnormal(Des.Resp2)
anova(lm(Parameters.beta ~ CTA + Extr + molRat + NH4OH_f + HCl_s, data = Des.Resp2))

Des.Resp3 <- add.response(Dessign, sF)
#IAPlot(Des.Resp3)
#MEPlot(Des.Resp3)
plot(Des.Resp3)
#halfnormal(Des.Resp3)
anova(lm(sF ~ CTA + Extr + molRat + NH4OH_f + HCl_s, data = Des.Resp3))

#Deseabilidad
d1 <- (Parameters$alpha - min(Parameters$alpha)) / (max(Parameters$alpha) - min(Parameters$alpha))
d2 <- (Parameters$beta - min(Parameters$beta)) / (max(Parameters$beta) - min(Parameters$beta))
d3 <- (sF - min(sF)) / (max(sF) - min(sF))

gm_mean <- function(a){prod(a)^(1/length(a))}
Des <- apply(cbind(d1, d2, d3), 1, gm_mean)

Des.RespD <- add.response(Dessign, Des)
#IAPlot(Des.RespD)
#MEPlot(Des.RespD)
plot(Des.RespD)

anova(lm(Des ~ CTA + Extr + molRat + NH4OH_f + HCl_s, data = Des.RespD))

pdf("Halfnormal19-06-12.pdf", height = 7/2, width = 7/2)
halfnormal(Des.RespD, main = '', code = TRUE)
halfnormal(Des.Resp1, main = '', code = TRUE)
halfnormal(Des.Resp2, main = '', code = TRUE)
halfnormal(Des.Resp3, main = '', code = TRUE)
dev.off()
