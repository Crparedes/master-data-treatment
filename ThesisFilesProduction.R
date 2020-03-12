savepdf <- function(p, name, h = 7/1.5, w = 9/1.5) {
  pdf(paste0(name, '.pdf'), height = h, width = w)
  print(p)
  dev.off()
}
library(transmem)

source("18-AugustDecember/18-12-06-Li-MemX.R")
  C.1 <- conc2frac(feed = ConcFaseAlim_1, strip = ConcFaseRec_1, time = Time)
  n <- transTrend(C.1)
  p <- transPlot(C.1, bw = TRUE, xlab = 'Tiempo (h)', srs = 0.5, trend = n)
  (p <- p + geom_smooth(se = FALSE, size = 0.5, color = 'black') +
    geom_point(data = C.1[which(C.1$Phase == 'Strip'), ],
               col = 'white', size = 2.8*0.5,
               aes(x = Time, y = Fraction), shape = 15))
  #savepdf(p, 'C1-profile')
  permcoef(trans = C.1, conc0 = ConcFaseAlim_1[1], vol = 90, area = 4.91, units = c('cm^3', 'cm^2', 'h'))

source("18-AugustDecember/18-12-17-Li-MemX.R")
  C.2 <- conc2frac(feed = ConFee5.1, strip = ConStr5.1, time = Time3)
  n <- transTrend(C.2)
  p <- transPlot(C.2, bw = TRUE, xlab = 'Tiempo (h)', srs = 0.5, trend = n)
  #savepdf(p, 'C2-profile')
  permcoef(trans = C.2, conc0 = ConFee5.1[1], vol = 90, area = 4.91, units = c('cm^3', 'cm^2', 'h'))
  
source("19-JanuaryJune/19-01-28-FrF2-1.R")
  D.3 <- conc2frac(feed = Feed_6.3, strip = Strip_6.3, time = c(0, 1, 2, 3, 5, 7, 20, 21))
  n <- transTrend(D.3)
  p <- transPlot(D.3, bw = TRUE, xlab = 'Tiempo (h)', srs = 0.5, trend = n)
  #savepdf(p, 'D3-profile')
  permcoef(trans = D.3, conc0 = 2, vol = 90, area = 4.91, units = c('cm^3', 'cm^2', 'h'))
  permcoef(trans = D.3[-c(1, 9), ], conc0 = 2, vol = 90, area = 4.91, units = c('cm^3', 'cm^2', 'h'))
  
  D.8 <- conc2frac(feed = Feed_6.8, strip = Strip_6.8, time = c(0, 1, 2, 3.5, 5, 7, 21.66, 23))
  n <- transTrend(D.8, eccen = 2)
  p <- transPlot(D.8, bw = TRUE, xlab = 'Tiempo (h)', srs = 0.5, trend = n)
  #savepdf(p, 'D8-profile')
  permcoef(trans = D.8, conc0 = 10, vol = 90, area = 4.91, units = c('cm^3', 'cm^2', 'h'))
  
  pdf('MEP1.pdf', height = 7/1.5, width = 18/1.5)
  cMEPlot(Des.Resp)
  dev.off()  
  
source("19-JanuaryJune/19-02-21-Li-MemX.R")
  E.1 <- conc2frac(feed = Feed_7.1, strip = Strip_7.1, time = Time1)
  E.2 <- conc2frac(feed = Feed_7.2, strip = Strip_7.2, time = Time1)
  E.3 <- conc2frac(feed = Feed_7.3, strip = Strip_7.3, time = Time2)
  E.4 <- conc2frac(feed = Feed_7.4, strip = Strip_7.4, time = Time2)
  p <- list()
  p[[1]] <- transPlot(trans = E.1, trend = transTrend(trans = E.1, eccen = 1.5), bw = TRUE, srs = 0.5)  
  p[[2]] <- transPlot(trans = E.2, trend = transTrend(trans = E.2, eccen = 1.5), bw = TRUE, srs = 0.5)  
  (p[[3]] <- transPlot(trans = E.3, bw = TRUE, srs = 0.5) + 
      geom_smooth(se = FALSE, col = 'black', size = 0.5, span = 0.6))
  p[[4]] <- transPlot(trans = E.4, trend = transTrend(trans = E.4, eccen = 1.5), bw = TRUE, srs = 0.5)  
  for (i in 1:4) savepdf(p[[i]], name = paste0('E.', i, '-profile')) 

source("19-JanuaryJune/19-06-01-Li-Mem11.R")
  transPlotWR(trans = list(TransFrac$M.11.1.Li, TransFrac$M.11.2.Li, TransFrac$M.11.3.Li),
              trend = list(TransNLS$M.11.1, TransNLS$M.11.2, TransNLS$M.11.3), bw = TRUE, srs = 0.5)
  transPlot(TransFrac[[1]], trend = TransNLS[[1]], bw = TRUE, srs = 0.5)
  p <- transPlot(TransFrac[[2]], trend = TransNLS[[2]], bw = TRUE, srs = 0.5)
  transPlot(TransFrac[[3]], trend = TransNLS[[3]], bw = TRUE, srs = 0.5)
  
  savepdf(p, 'g1profile')
  