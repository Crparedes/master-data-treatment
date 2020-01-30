library(FrF2)
#variables <- c('CTA', 'NPOE', 'LIX-54', 'Cyanex', 'NaOH', 'HCl', 'Li')

variables <- list(CTA = c(30, 80), NPOE = c(20, 40), LIX = c(12, 50), Cyanex = c(12, 50), 
                  NaOH_f = c(0.01, 0.001), HCl_s = c(0.5, 0.1), Li_0 = c(2, 10))

FrF2(8, factor.names = variables, seed = 12)
(Dessign <- FrF2(8, factor.names = variables, seed = 12))

# Run scripts associated with the lithium cuantification corresponding to these days...
source("CurvaLi02Feb19.R"); source("CurvaLi06Feb19.R"); source("CurvaLi08Feb19.R")
MaxStrips <- IniFeed <- vector()
for (i in 1:8) {
  MaxStrips[i] <- max(get(paste0("Strip_6.", i)))
  IniFeed[i]   <- max(get(paste0("T_o_Feed_6.", i)))
}


Des.Resp <- add.response(Dessign, MaxStrips)
IAPlot(Des.Resp)
MEPlot(Des.Resp)
plot(Des.Resp)
0.01*0.25*39997
stdNa <- c(150, 100, 50, 20, 10, 5, 2)
stockNA <- 0.0861 * 39997
10*200/stockNA
sum(5*stdNa/200)
