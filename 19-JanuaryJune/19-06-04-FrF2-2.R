library(FrF2)library(transMem)
#variables <- c('CTA', 'NPOE', 'LIX-54', 'Cyanex', 'NaOH', 'HCl', 'Li')

variables <- list(CTA = c(30, 45), Extr = c(70, 110), molRat = c(2, 3.5),
                  NH4OH_f = c(0.01, 0.02), HCl_s = c(0.07, 0.04))

(Dessign <- FrF2(8, factor.names = variables, seed = 12))

round(extrMolRat(mass = as.numeric(as.character(Dessign$Extr)), ratio = as.numeric(as.character(Dessign$molRat))),1)
extrMolRat(ex1 = c(40.6, 49.8, 78.0, 41.1, 78.1, 64.5, 64.5, 50.3),
           ex2 = c(29.5, 20.0, 31.8, 28.5, 32.3, 45.3, 45.7, 20.3))
# Run scripts associated with the lithium cuantification corresponding to these days...
source("19-06-12-LiNa-Mem12.R")

Des.Resp1 <- add.response(Dessign, Parameters$alpha)
#IAPlot(Des.Resp1)
MEPlot(Des.Resp1)
plot(Des.Resp1)
halfnormal(Des.Resp1)
anova(lm(Parameters.alpha ~ CTA + Extr + molRat + NH4OH_f + HCl_s, data = Des.Resp1))


Des.Resp2 <- add.response(Dessign, Parameters$beta)
#IAPlot(Des.Resp2)
MEPlot(Des.Resp2)
plot(Des.Resp2)
halfnormal(Des.Resp2)
anova(lm(Parameters.beta ~ CTA + Extr + molRat + NH4OH_f + HCl_s, data = Des.Resp2))

Des.Resp3 <- add.response(Dessign, sF)
#IAPlot(Des.Resp3)
MEPlot(Des.Resp3)
plot(Des.Resp3)
halfnormal(Des.Resp3)
anova(lm(sF ~ CTA + Extr + molRat + NH4OH_f + HCl_s, data = Des.Resp3))

Phi_5 <- vector()
for (i in 1:8) {
  eval(parse(text = paste0("Phi_5 <- c(Phi_5, 
                           TransFrac$M.12.", i, ".Li[(which(TransFrac$M.12.", i, ".Li$Phase=='Strip')), ][6, 3])")))
}

Des.Resp4 <- add.response(Dessign, Phi_5)
#IAPlot(Des.Resp4)
MEPlot(Des.Resp4)
plot(Des.Resp4)
halfnormal(Des.Resp4)
anova(lm(Phi_5 ~ CTA + Extr + molRat + NH4OH_f + HCl_s, data = Des.Resp4))

#Deseabilidad
d1 <- (Parameters$alpha - min(Parameters$alpha)) / (max(Parameters$alpha) - min(Parameters$alpha))
d2 <- (Parameters$beta - min(Parameters$beta)) / (max(Parameters$beta) - min(Parameters$beta))
d3 <- (sF - min(sF)) / (max(sF) - min(sF))

gm_mean <- function(a){prod(a)^(1/length(a))}
Des <- apply(cbind(d1, d2, d3), 1, gm_mean)

Des.RespD <- add.response(Dessign, Des)
#IAPlot(Des.RespD)
MEPlot(Des.RespD)
plot(Des.RespD)

anova(lm(Des ~ CTA + Extr + molRat + NH4OH_f + HCl_s, data = Des.RespD))

pdf("Halfnormal19-06-12.pdf", height = 7/2, width = 7/2)
halfnormal(Des.RespD, main = '', code = TRUE)
halfnormal(Des.Resp1, main = '', code = TRUE)
halfnormal(Des.Resp2, main = '', code = TRUE)
halfnormal(Des.Resp3, main = '', code = TRUE)
dev.off()
