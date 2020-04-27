setwd("~/Dropbox/0-UNAM/0-Polymeric Inclusion Membranes/master-data-treatment")
savepdf <- function(p, name, h = 7/1.5, w = 9/1.5) {
  pdf(paste0(name, '.pdf'), height = h, width = w)
  print(p)
  dev.off()
}
library(transmem)

# Liquid-Solid extractions using PIMS
source("18-AugustDecember/18-10-05-Li-MemX.R")
# LIX-54-100, D2EHP, TEHP, Cyanex 923, TBP
MW <- c(246, 322.43, 434.6, 348, 266.318)
#Moles used in first stage
molused1 <- 15e-3/MW
mean(molused1); sd(molused1)
#lithium moles present:
molRec<- 40e-3/6.941*0.025
#Lithium moles extracted:
molextracted1 <- c(0.18, 0.33, 0.03, 0.05, 0.06)*1e-3/6.941*0.030
#Lithium extraction efficiency:
molRec/(44*1000*0.025)*100
#Corrected efficiency
molextracted1/molused1[c(3, 4, 5, 3, 4)]*100

#Moles used in seccond stage
# LIX-54-100, TEHP, Cyanex 923
MW <- c(246, 434.6, 348)
molused1 <- 16e-3/MW
mean(molused1); sd(molused1)
#lithium moles present:
molRec<-25e-3/6.941*0.018
#Lithium moles extracted:
molextracted1 <- c(0.06, 1.36)*1e-3/6.941*0.015
#Lithium extraction efficiency:
molextracted1/molRec
#Corrected efficiency
molextracted1/mean(molused1)*100
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
  p[[1]] <- transPlot(trans = E.1, trend = transTrend(trans = E.1, eccen = 1.5), bw = TRUE, srs = 0.5, xlab = 'Tiempo (h)')  
  p[[2]] <- transPlot(trans = E.2, trend = transTrend(trans = E.2, eccen = 1.5), bw = TRUE, srs = 0.5, xlab = 'Tiempo (h)')  
  (p[[3]] <- transPlot(trans = E.3, bw = TRUE, srs = 0.5, xlab = 'Tiempo (h)') + 
      geom_smooth(se = FALSE, col = 'black', size = 0.5, span = 0.6))
  p[[4]] <- transPlot(trans = E.4, trend = transTrend(trans = E.4, eccen = 1.5), bw = TRUE, srs = 0.5, xlab = 'Tiempo (h)')  
  for (i in 1:4) savepdf(p[[i]], name = paste0('E.', i, '-profile')) 

source("19-JanuaryJune/19-06-01-Li-Mem11.R")
  transPlotWR(trans = list(TransFrac$M.11.1.Li, TransFrac$M.11.2.Li, TransFrac$M.11.3.Li),
              trend = list(TransNLS$M.11.1, TransNLS$M.11.2, TransNLS$M.11.3), bw = TRUE, srs = 0.5)
  transPlot(TransFrac[[1]], trend = TransNLS[[1]], bw = TRUE, srs = 0.5)
  p <- transPlot(TransFrac[[2]], trend = TransNLS[[2]], bw = TRUE, srs = 0.5, xlab = 'Tiempo (h)')
  transPlot(TransFrac[[3]], trend = TransNLS[[3]], bw = TRUE, srs = 0.5)
  
  savepdf(p, 'g1profile')
  
source("19-JanuaryJune/19-06-04-FrF2-2.R")
  pdf('DanielPlotsFrF2-2.pdf', height = 7/1.35, width = 9/1.35)
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
  dev.off()
  
  pdf('IAPlotFrF2-2.pdf', height = 7/1.35, width = 10/1.35)
  IAPlot(Des.RespD)
  dev.off()

  #Hidrodinámica y reproducibilidad
source("19-JuneDecember/19-07-01-Li-Mem13.R")
    savepdf(p, 'Thetha1_profiles')
    savepdf(q, 'RPM_1')
    
source("19-JuneDecember/19-08-17-LiNa-Mem14.R")
    savepdf(p_sf, 'SepFactor_Thetha2')
    savepdf(p, 'Theta2_profiles')
    t.test(x = velData[c(6, 30, 36), 2],
           y = velData[c(12, 18, 24), 2])
    t.test(x = ssepFactor[c(6, 30, 36), 2],
           y = ssepFactor[c(12, 18, 24), 2])    
    
    b <- vector()
    for (i in 1:6) b <- c(b, TransNLS[[i]]$Result[3])
    mean(b[c(1,5,6)]); sd(b[c(1,5,6)]); b[c(1,5,6)];sd(b[c(1,5,6)])/mean(b[c(1,5,6)])*100
    mean(b[c(2,3,4)]); sd(b[c(2,3,4)]); b[c(2,3,4)];sd(b[c(2,3,4)])/mean(b[c(2,3,4)])*100
    a <- vector()
    for (i in 1:6) a <- c(a, TransNLS[[i]]$Result[1])
    mean(a[c(1,5,6)]); sd(a[c(1,5,6)]); a[c(1,5,6)];sd(a[c(1,5,6)])/mean(a[c(1,5,6)])*100
    mean(a[c(2,3,4)]); sd(a[c(2,3,4)]); a[c(2,3,4)];sd(a[c(2,3,4)])/mean(a[c(2,3,4)])*100
    
  #Simplex
  source("19-09-Simplex-2/19-08-30-Simplex_2.R")
  source("19-09-Simplex-2/19-09-10-LiNa-Mem16CLEAN.R")
    savepdf(p, 'K1_pro') 
  source("19-09-Simplex-2/19-09-23-LiNa-Mem16_9.R")
    savepdf(p, 'K9_pro')   
  #source(file = "19-09-Simplex-2/19-10-03-Li-Mem17_1.R")
  #  savepdf(p, 'K10_pro')   
  
  # REUSE CYCLES
  source("19-JuneDecember/19-10-25-Li-Reuse_Mem19.R")  
    savepdf(p1, 'K10_pro')
    savepdf(perm, 'K10_perm')
    savepdf(p3, 'reuseprofiles')
    savepdf(q, 'reusesummary')
  
  source("19-JuneDecember/19-10-18-Li-Preconcentration_Mem18.R")
    savepdf(p, 'liconc_0')

    #Sythetic Sinplified Seawater    
    source("19-JuneDecember/19-11-14-LiNaK-SSS_Mem22.R")
    savepdf(p, 'sssprof')
    savepdf(p_sf, 'sssSep')
    
    #Actual Natural Seawater
    source("20-JanuaryToTheEnd/20-01-29-Li-Concentration_SWM_Mem25.R")
    savepdf(p1, 'LiConcAS', h = 5/1.5, w = 9/1.5)
    savepdf(p2, 'NaConcAS', h = 5/1.5, w = 9/1.5)
    savepdf(p3, 'KConcAS', h = 5/1.5, w = 9/1.5)
    savepdf(p_sf, 'SW_Sep')
    
    source("20-JanuaryToTheEnd/20-01-24-Li-Concentration_SWS_Mem23.R")
    savepdf(p1, 'LiConcAS-FAIL', h = 5/1.5, w = 9/1.5)
    
    #Simulations of proposed equations
    source("ModelsEqEmpiric.R")
    
    #Simplexmovements
    library(labsimplex); library(ggplot2)
    simplex <- labsimplex(N = 2, var.name = c('Variable 1', 'Variable 2'), centroid = c(3, 5))
    
    plot(simplex$coords, xlim = c(2, 6), ylim = c(4, 10))
    #Reflection:
    simplex1 <- generateVertex(simplex, qflv = c(2, 1, 0))
    #Cw:
    generateVertex(simplex1, qflv = -1, algor = 'variable')
    #Cr:
    generateVertex(simplex1, qflv = 0.8, algor = 'variable')
    #Expansion:
    generateVertex(simplex1, qflv = 2.2, algor = 'variable')
    
    R  <- c(4, 6.73)
    Cw <- c(2.875, 4.783)
    Cr <- c(3.625, 6.0825)
    E  <- c(4.75, 8.031) 
    ID <- as.factor(c(rep('S', 3), 'R', 'Cw', 'Cr', 'E'))
    data <- data.frame(cbind(rbind(simplex$coords, R, Cw, Cr, E), ID))
    
    p <- ggplot(data = data, aes(x = Variable.1, y = Variable.2, group = ID)) + 
      labs(y = 'Variable 2', x = 'Variable 1') + 
      scale_x_continuous(expand = c(0, 1.04)) + scale_y_continuous(expand = c(0, 1.04)) +
      geom_point(colour = 'black', shape = c(22, 22, 22, 23, 25, 24, 21), size = 5, fill = 'black') +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(color = "black", size=8),
            axis.text.y = element_text(color = "black", size=8)) + geom_line(size = 0.8) +
      geom_segment(x = data[1, 1], y = data[1, 2], xend = data[2, 1], yend = data[2, 2], size = 0.5) +
      geom_segment(x = data[1, 1], y = data[1, 2], xend = data[4, 1], yend = data[4, 2], size = 0.5, linetype = 2) +
      geom_segment(x = data[1, 1], y = data[1, 2], xend = data[5, 1], yend = data[5, 2], size = 0.5, linetype = 3) +
      geom_segment(x = data[1, 1], y = data[1, 2], xend = data[6, 1], yend = data[6, 2], size = 0.5, linetype = 3) +
      geom_segment(x = data[1, 1], y = data[1, 2], xend = data[7, 1], yend = data[7, 2], size = 0.5, linetype = 10) +
      geom_segment(x = data[2, 1], y = data[2, 2], xend = data[4, 1], yend = data[4, 2], size = 0.5, linetype = 2) +
      geom_segment(x = data[2, 1], y = data[2, 2], xend = data[5, 1], yend = data[5, 2], size = 0.5, linetype = 3) +
      geom_segment(x = data[2, 1], y = data[2, 2], xend = data[6, 1], yend = data[6, 2], size = 0.5, linetype = 3) +
      geom_segment(x = data[2, 1], y = data[2, 2], xend = data[7, 1], yend = data[7, 2], size = 0.5, linetype = 10)
    p
    msavepdf(p, 'simplexmov')
