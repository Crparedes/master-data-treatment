library(FrF2)
library(transmem)
#variables <- c('CTA', 'NPOE', 'LIX-54', 'Cyanex', 'NaOH', 'HCl', 'Li')

variables <- list(CTA = c(30, 45), Extr = c(70, 110), molRat = c(2, 3.5),
                  NH4OH = c(0.01, 0.02)*1000, HCl = c(0.07, 0.04)*1000)

(Dessign <- FrF2(8, factor.names = variables, seed = 12))

#round(extrMolRat(mass = as.numeric(as.character(Dessign$Extr)), ratio = as.numeric(as.character(Dessign$molRat))),1)
#extrMolRat(ex1 = c(40.6, 49.8, 78.0, 41.1, 78.1, 64.5, 64.5, 50.3),
#           ex2 = c(29.5, 20.0, 31.8, 28.5, 32.3, 45.3, 45.7, 20.3))
# Run scripts associated with the lithium cuantification corresponding to these days...
source("19-JanuaryJune/19-06-12-LiNa-Mem12.R")
order <- c(4, 5, 7, 8, 1, 3, 2, 6)

Des.Resp1 <- add.response(Dessign, Parameters$alpha[order])
#IAPlot(Des.Resp1)
MEPlot(Des.Resp1)
plot(Des.Resp1)
halfnormal(Des.Resp1, code = TRUE, alpha = 0.05)
anova(lm(Parameters.alpha.order. ~ CTA + Extr + molRat + NH4OH + HCl, data = Des.Resp1))


Des.Resp2 <- add.response(Dessign, Parameters$beta[order])
#IAPlot(Des.Resp2)
MEPlot(Des.Resp2)
plot(Des.Resp2)
halfnormal(Des.Resp2, code = TRUE, alpha = 0.10)
anova(lm(Parameters.beta.order. ~ CTA + Extr + molRat + NH4OH + HCl, data = Des.Resp2))



#Des.Resp3 <- add.response(Dessign, sF[order])
#IAPlot(Des.Resp3)
#MEPlot(Des.Resp3)
#plot(Des.Resp3)
#halfnormal(Des.Resp3, code = TRUE, alpha = 0.10)
#anova(lm(sF.order. ~ CTA + Extr + molRat + NH4OH + HCl, data = Des.Resp3))

Des.Resp4 <- add.response(Dessign, sF1[order])
#IAPlot(Des.Resp3)
MEPlot(Des.Resp4)
plot(Des.Resp4)
halfnormal(Des.Resp4, code = TRUE, alpha = 0.10)
anova(lm(sF1.order. ~ CTA + Extr + molRat + NH4OH + HCl, data = Des.Resp4))


#Des.Resp4 <- add.response(Dessign, sF2)
#IAPlot(Des.Resp3)
#MEPlot(Des.Resp4)
#plot(Des.Resp4)
#halfnormal(Des.Resp4)
#anova(lm(sF ~ CTA + Extr + molRat + NH4OH + HCl, data = Des.Resp3))


d1 <- (Parameters$alpha - min(Parameters$alpha)) / (max(Parameters$alpha) - min(Parameters$alpha))
d2 <- (Parameters$beta - min(Parameters$beta)) / (max(Parameters$beta) - min(Parameters$beta))
d3 <- (sF - min(sF)) / (max(sF) - min(sF))

gm_mean <- function(a){prod(a)^(1/length(a))}
Des <- apply(cbind(d1, d2, d3), 1, gm_mean)

Des.RespD <- add.response(Dessign, Des[order])
IAPlot(Des.RespD)
MEPlot(Des.RespD)
plot(Des.RespD)

anova(lm(Des.order. ~ CTA + Extr + molRat + NH4OH + HCl, data = Des.RespD))

#pdf("Halfnormal19-06-12.pdf", height = 7/2, width = 7/2)
halfnormal(Des.Resp1, code = TRUE, alpha = 0.1, keep.colons = TRUE)
halfnormal(Des.Resp2, main = '', code = TRUE, alpha = 0.1, keep.colons = TRUE)
halfnormal(Des.Resp4, main = '', code = TRUE, alpha = 0.1, keep.colons = TRUE)
halfnormal(Des.RespD, main = '', code = TRUE, alpha = 0.1, keep.colons = TRUE)

DanielPlot(Des.Resp1, main = '', code = FALSE, alpha = 0.1, autolab = FALSE, 
           pch = c(5, 5, 15, 15, 5, 5, 15), cex.pch = 1.2, half = TRUE)
abline(a = -0, b = 44, lty = 2)
DanielPlot(Des.Resp2, main = '', code = FALSE, alpha = 0.1, autolab = FALSE, 
           pch = c(15, 5, 5, 5, 5, 5, 5), cex.pch = 1.2, half = TRUE)
abline(a = -1.6, b = 22, lty = 2)
DanielPlot(Des.Resp4, main = '', code = FALSE, alpha = 0.1, autolab = FALSE, 
           pch = c(5, 5, 5, 15, 5, 5, 5), cex.pch = 1.2, half = TRUE)
abline(a = -0.1, b = 0.09, lty = 2)
DanielPlot(Des.RespD, main = '', code = FALSE, alpha = 0.1, autolab = FALSE, 
           pch = c(5, 5, 15, 15, 5, 5, 15), cex.pch = 1.2, half = TRUE)
abline(a = 0, b = 12, lty = 2)
#dev.off()

var <- list(X1 = c(30, 45), X2 = c(70, 110), X3 = c(2, 3.5), X4 = c(0.01, 0.02), X5 = c(0.07, 0.04))
(Dess <- FrF2(8, factor.names = var, seed = 12))

R1 <- add.response(Dess, Parameters$alpha[order])
R2 <- add.response(Dess, Parameters$beta[order])
R3 <- add.response(Dess, sF1[order])
R4 <- add.response(Dess, Des[order])

for (i in 1:5) {
  eval(parse(text = paste0('print(anova(lm(Parameters.alpha.order. ~ X', i, ', data = R1)))')))
}
for (i in 1:5) {
  eval(parse(text = paste0('print(anova(lm(Parameters.beta.order. ~ X', i, ', data = R2)))')))
}
for (i in 1:5) {
  eval(parse(text = paste0('print(anova(lm(sF1.order. ~ X', i, ', data = R3)))')))
}
for (i in 1:5) {
  eval(parse(text = paste0('print(anova(lm(Des.order. ~ X', i, ', data = R4)))')))
}

for (i in 1:4) {
  for (j in (i+1):5) {
    eval(parse(text = paste0('print(anova(lm(Parameters.alpha.order. ~ X', i, ' + X', j, ', data = R1)))')))
  }
}
for (i in 1:4) {
  for (j in (i+1):5) {
    eval(parse(text = paste0('print(anova(lm(Parameters.beta.order. ~ X', i, ' + X', j, ', data = R2)))'))) 
  }
}
for (i in 1:4) {
  for (j in (i+1):5) {
    eval(parse(text = paste0('print(anova(lm(sF1.order. ~ X', i, ' + X', j, ', data = R3)))')))
  }
}
for (i in 1:4) {
  for (j in (i+1):5) {
    eval(parse(text = paste0('print(anova(lm(Des.order. ~ X', i, ' + X', j, ', data = R4)))'))) 
  }
}


for (i in 1:3) {
  for (j in (i+1):4) {
    for (k in (j+1):5) {
    eval(parse(text = paste0('print(anova(lm(Parameters.alpha.order. ~ X', i, ' + X', j, ' + X', k,
                             ', data = R1)))')))
}}}

for (i in 1:3) {
  for (j in (i+1):4) {
    for (k in (j+1):5) {
      eval(parse(text = paste0('print(anova(lm(Parameters.beta.order. ~ X', i, ' + X', j, ' + X', k,
                               ', data = R2)))'))) 
    }}}

for (i in 1:3) {
  for (j in (i+1):4) {
    for (k in (j+1):5) {
      eval(parse(text = paste0('print(anova(lm(sF1.order. ~ X', i, ' + X', j, ' + X', k,
                               ', data = R3)))')))
    }}}

for (i in 1:3) {
  for (j in (i+1):4) {
    for (k in (j+1):5) {
      eval(parse(text = paste0('print(anova(lm(Des.order. ~ X', i, ' + X', j, ' + X', k,
                               ', data = R4)))'))) 
}}}



for (i in 1:2) {
  for (j in (i+1):3) {
    for (k in (j+1):4) {
      for (l in (k+1):5) {
      eval(parse(text = paste0('print(anova(lm(Parameters.alpha.order. ~ X', i, ' + X', j, ' + X', k,
                               ', data = R1)))')))
    }}}}

for (i in 1:2) {
  for (j in (i+1):3) {
    for (k in (j+1):4) {
      for (l in (k+1):5) {
        eval(parse(text = paste0('print(anova(lm(Parameters.beta.order. ~ X', i, ' + X', j, ' + X', k,
                               ', data = R2)))'))) 
      }}}}

for (i in 1:2) {
  for (j in (i+1):3) {
    for (k in (j+1):4) {
      for (l in (k+1):5) {
        eval(parse(text = paste0('print(anova(lm(sF1.order. ~ X', i, ' + X', j, ' + X', k,
                               ', data = R3)))')))
      }}}}

for (i in 1:2) {
  for (j in (i+1):3) {
    for (k in (j+1):4) {
      for (l in (k+1):5) {
        eval(parse(text = paste0('print(anova(lm(Des.order. ~ X', i, ' + X', j, ' + X', k,
                               ', data = R4)))'))) 
    }}}}
