library(ggplot2)
library(ggformula)
library(transMem)
PDF <- TRUE
#-----STOCK SOLUTIONS--------------------------------------------------------
StockLi.200 <- 129.5 * 0.187872 * 0.99 / 0.1200962
StockLi.5_5   <- StockLi.200 * 1.2621 / 50.0062
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CalCurves <- list(
  Lithium.1 = data.frame(Conc = c(0.0000, 0.0592, 0.1194, 0.3021, 0.5929, 1.1905, 1.8952, 2.4922) *
    StockLi.5_5 / c(6.0000, 6.0524, 6.0561, 6.0393, 6.0230, 6.0389, 6.0886, 6.0428),
  Signal = c(0.000, 0.007, 0.015, 0.041, 0.080, 0.159, 0.248, 0.328))
)
## for a cleaner workspace
#rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
CalModels <- list(
  Lithium.1 = calibCurve(curve = CalCurves$Lithium.1)
)
anova(CalModels$Lithium.1)
summary(CalModels$Lithium.1)
#-----MUESTRAS CIEGAS--------------------------------------------------------
BlindeP <- data.frame(LiRe = c(0.0794, 0.6180, 1.5084, 0.0793, 0.6328, 1.4877) * StockLi.5_5 /
    c(6.0672, 6.0764, 6.0190, 6.0308, 6.2410, 6.5392),
  LiSg = c(0.010, 0.080, 0.196, 0.010, 0.080, 0.179)
)
BlindeP$LiIn <- signal2conc(signal = BlindeP$LiSg, model = CalModels$Lithium.1)
plot(x = BlindeP$LiRe, y = BlindeP$LiIn)
abline(a = 0, b = 1, col = 2, lty = 3)
abline(lm(BlindeP$LiIn ~ BlindeP$LiRe))
summary(lm(BlindeP$LiIn ~ BlindeP$LiRe))
## NO HAY EFECTO MATRIZ POR EL ÁCIDO CLORHÍDRICO O EL HIDRÓXIDO DE AMONIO:
t.test(x = BlindeP$LiIn[1:3], y = BlindeP$LiRe[1:3], paired = TRUE)
t.test(x = BlindeP$LiIn[4:6], y = BlindeP$LiRe[4:6], paired = TRUE)

#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- list (
  T.13.1 = c(0, 1, 2, 3, 4, 5),
  T.13.2 = c(0, 1, 2, 3, 4, 5),
  T.13.3 = c(0, 1, 2, 3, 4, 5),
  T.13.4 = c(0, 1, 2, 3, 4, 5),
  T.13.5 = c(0, 1, 2, 3, 4, 5),
  T.13.6 = c(0, 1, 2, 3, 4, 4.75),
  T.13.7 = c(0, 1, 2, 3, 4, 5)
)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(
  Feed.13.1.Li   = c(0.310, 0.247, 0.200, 0.163, 0.135, 0.106),
  Strip.13.1.Li  = c(0.002, 0.065, 0.114, 0.156, 0.185, 0.207),
  Feed.13.2.Li   = c(0.304, 0.244, 0.203, 0.165, 0.135, 0.115),
  Strip.13.2.Li  = c(0.000, 0.065, 0.102, 0.139, 0.165, 0.191),
  Feed.13.3.Li   = c(0.307, 0.256, 0.223, 0.195, 0.168, 0.148),
  Strip.13.3.Li  = c(0.000, 0.032, 0.071, 0.104, 0.135, 0.150),
  Feed.13.4.Li   = c(0.301, 0.237, 0.187, 0.150, 0.122, 0.100),
  Strip.13.4.Li  = c(0.000, 0.055, 0.106, 0.145, 0.174, 0.196),
  Feed.13.5.Li   = c(0.306, 0.233, 0.177, 0.137, 0.108, 0.087),
  Strip.13.5.Li  = c(0.000, 0.067, 0.123, 0.165, 0.193, 0.216),
  Feed.13.7.Li   = c(0.301, 0.203, 0.136, 0.093, 0.067, 0.054),
  Strip.13.7.Li  = c(0.000, 0.093, 0.162, 0.205, 0.232, 0.253),
  Feed.13.6.Li   = c(0.295, 0.251, 0.219, 0.207, 0.205, 0.199),
  Strip.13.6.Li  = c(0.000, 0.046, 0.072, 0.087, 0.094, 0.100)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- list()
for (i in 1:7) {
  eval(parse(text = paste0("AliConc$Feed.13.", i, ".Li <- signal2conc(signal = AliAbs$Feed.13.", i, ".Li,
        model = CalModels$Lithium.1)")))
  eval(parse(text = paste0("AliConc$Strip.13.", i, ".Li <- signal2conc(signal = AliAbs$Strip.13.", i, ".Li,
        model = CalModels$Lithium.1)")))
}
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
TransFrac <- list()
for (i in 1:7) {
  eval(parse(text = paste0("TransFrac$M.13.", i, ".Li = conc2frac(feed = AliConc$Feed.13.", i, ".Li,
                            strip = AliConc$Strip.13.", i, ".Li, time = AliTimes$T.13.", i, ")")))
}
#rm(sub.Na)
#-----MODELOS DE REGRESIÓN NO LINEAL-----------------------------------------
TransNLS <- list()
SS_par <- vector()
for (i in 1:7) {
  eval(parse(text = paste0("X <- transTrend(TransFrac$M.13.", i, ".Li, model = 'paredes', eccen = 1)")))
  SS_par <- c(SS_par, sum(resid(X$feed)^2), sum(resid(X$strip)^2))
  eval(parse(text = paste0("TransNLS$M.13.", i, " <- X")))
}

TransNLSXot <- list()
SS_xot <- vector()
for (i in 1:7) {
  eval(parse(text = paste0("X <- transTrend(TransFrac$M.13.", i, ".Li, model = 'rodriguez')")))
  SS_xot <- c(SS_xot, sum(resid(X$feed)^2), sum(resid(X$strip)^2))
    eval(parse(text = paste0("TransNLSXot$M.13.", i, " <- X")))
}
t.test(x = SS_par, y = SS_xot, paired = TRUE)
#-----PERFILES DE TRANSPORTE ------------------------------------------------
if (PDF) pdf("Perfiles19-07-01.pdf", height = 7/1.8, width = 9/1.8)

Parameters <- data.frame()
for (i in 1:7) {
# invisible(readline(prompt="Press [enter] to continue"))
  eval(parse(text = paste0("transPlot(trans = TransFrac$M.13.", i, ".Li, trend = TransNLS$M.13.", i, ",
        ylim = c(-0.05, 1.08), ybreaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), xbreaks = 1:5)")))
  eval(parse(text = paste0("Parameters <- rbind(Parameters, TransNLS$M.13.", i, "$Result)")))
}
colnames(Parameters) <- names(TransNLS$M.13.1$Result)
round(Parameters, 3)

#-----CONSTANTES BETAS DE LOS TRANSPORTES------------------------------------
vel <- c("a) 360", "b) 440", "c) 530", "d) 591", "e) 680", "f) 740", "g) 920")
frac <- vector()
for (i in 1:7) frac <- c(frac, TransFrac[[i]]$Fraction[7:12])
velData <- data.frame(Agit. = as.factor(rep(vel, each = 6)), Phi_strip = frac, Time = rep(AliTimes[[1]], 7))

p <- ggplot(data = velData, aes(x = Time, y = Phi_strip, color = Agit., group = Agit.)) +
       theme_bw() + geom_point(size = 3, shape = 15)  + labs(y = expression(Phi), x = 'Time (h)') +
       theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
             axis.text.x = ggplot2::element_text(color = "black"),
             axis.text.y = ggplot2::element_text(color = "black"))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(7)
  e <- TransNLS[[1]]$eccen

  (p <- p + stat_function(fun = function(x) ((coefficients(TransNLS[[1]]$strip)[1] * x^e)
                               / (1 / coefficients(TransNLS[[1]]$strip)[2] + x^e)),
                         color = cols[1], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                         xlim = c(0, 5), size = 0.4) +
      stat_function(fun = function(x) ((coefficients(TransNLS[[2]]$strip)[1] * x^e)
                                       / (1 / coefficients(TransNLS[[2]]$strip)[2] + x^e)),
                    color = cols[2], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                    xlim = c(0, 5), size = 0.4) +
      stat_function(fun = function(x) ((coefficients(TransNLS[[3]]$strip)[1] * x^e)
                                       / (1 / coefficients(TransNLS[[3]]$strip)[2] + x^e)),
                    color = cols[3], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                    xlim = c(0, 5), size = 0.4) +
      stat_function(fun = function(x) ((coefficients(TransNLS[[4]]$strip)[1] * x^e)
                                       / (1 / coefficients(TransNLS[[4]]$strip)[2] + x^e)),
                    color = cols[4], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                    xlim = c(0, 5), size = 0.4) +
      stat_function(fun = function(x) ((coefficients(TransNLS[[5]]$strip)[1] * x^e)
                                       / (1 / coefficients(TransNLS[[5]]$strip)[2] + x^e)),
                    color = cols[5], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                    xlim = c(0, 5), size = 0.4) +
      stat_function(fun = function(x) ((coefficients(TransNLS[[6]]$strip)[1] * x^e)
                                       / (1 / coefficients(TransNLS[[6]]$strip)[2] + x^e)),
                    color = cols[6], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                    xlim = c(0, 5), size = 0.4) +
      stat_function(fun = function(x) ((coefficients(TransNLS[[7]]$strip)[1] * x^e)
                                       / (1 / coefficients(TransNLS[[7]]$strip)[2] + x^e)),
                    color = cols[7], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                    xlim = c(0, 5), size = 0.4))

#################################################
  #######################################################################
  frac <- vector()
  for (i in c(2, 4, 5, 7)) frac <- c(frac, TransFrac[[i]]$Fraction[7:12])
  velData <- data.frame(Agit. = as.factor(rep(vel[c(2, 4, 5, 7)], each = 6)),
                        Phi_strip = frac, Time = rep(AliTimes[[1]], 4))

  p <- ggplot(data = velData, aes(x = Time, y = Phi_strip, color = Agit., group = Agit.)) +
    theme_bw() + geom_point(size = 3, shape = 15)  + labs(y = expression(Phi), x = 'Time (h)') +
    theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(color = "black"),
          axis.text.y = ggplot2::element_text(color = "black"))

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  cols = gg_color_hue(4)
  e <- TransNLS[[1]]$eccen

  (p <- p + stat_function(fun = function(x) ((coefficients(TransNLS[[2]]$strip)[1] * x^e)
                                       / (1 / coefficients(TransNLS[[2]]$strip)[2] + x^e)),
                    color = cols[1], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                    xlim = c(0, 5), size = 0.4) +
      stat_function(fun = function(x) ((coefficients(TransNLS[[4]]$strip)[1] * x^e)
                                       / (1 / coefficients(TransNLS[[4]]$strip)[2] + x^e)),
                    color = cols[2], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                    xlim = c(0, 5), size = 0.4) +
      stat_function(fun = function(x) ((coefficients(TransNLS[[5]]$strip)[1] * x^e)
                                       / (1 / coefficients(TransNLS[[5]]$strip)[2] + x^e)),
                    color = cols[3], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                    xlim = c(0, 5), size = 0.4) +
      stat_function(fun = function(x) ((coefficients(TransNLS[[7]]$strip)[1] * x^e)
                                       / (1 / coefficients(TransNLS[[7]]$strip)[2] + x^e)),
                    color = cols[4], # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                    xlim = c(0, 5), size = 0.4))
  ####################################################3
consolid <- data.frame(Agit = c(360, 440, 530, 591, 680, 740, 920),
                       phiMax = velData[which(velData$Time == 5), 2])
  (q <- ggplot(data = consolid,
              aes(x = Agit, y = phiMax)) + geom_point(size = 3) +
      geom_errorbar(aes(ymin = phiMax - 0.02, ymax = 0.02 + phiMax), width = 8) +
      geom_errorbarh(xmin = consolid$Agit - 30, xmax = 30 + consolid$Agit) +
      labs(y = expression(Phi[5*h]), x = 'Rapidez de agitación (RPM)') +
      theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(color = "black"),
            axis.text.y = ggplot2::element_text(color = "black")) + theme_bw())



if (PDF) dev.off()
###################################################
###################################################
if (F) {
    Gr_f <- rowMeans(rbind(c(19, 30, 34, 33, 23),
                    c(16, 26, 31, 17, 16),
                    c(31, 36, 30, 29, 32),
                    c(24, 13, 33, 23, 30),
                    c(18, 36, 36, 22, 48),
                    c(42, 45, 32, 50, 55),
                    c(20, 25, 21, 37, 26)))
    order(Gr_f)[]
    velData <- data.frame(Grosor = as.factor(rep(Gr_f, each = 6)),
                          Phi_strip = frac, Time = rep(AliTimes[[1]], 7))

    p <- ggplot(data = velData, aes(x = Time, y = Phi_strip, color = Grosor, group = Grosor)) +
        theme_bw() + geom_point(size = 3, shape = 15)  + labs(y = expression(Phi), x = 'Time (h)') +
        theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
              axis.text.x = ggplot2::element_text(color = "black"),
              axis.text.y = ggplot2::element_text(color = "black"))

    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }

    cols = gg_color_hue(7)
    e <- TransNLS[[1]]$eccen

    (p <- p + stat_function(fun = function(x) ((coefficients(TransNLS[[1]]$strip)[1] * x^e)
                                               / (1 / coefficients(TransNLS[[1]]$strip)[2] + x^e)),
                            color = "black", # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                            xlim = c(0, 5), size = 0.4) +
        stat_function(fun = function(x) ((coefficients(TransNLS[[2]]$strip)[1] * x^e)
                                         / (1 / coefficients(TransNLS[[2]]$strip)[2] + x^e)),
                      color = "black", # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                      xlim = c(0, 5), size = 0.4) +
        stat_function(fun = function(x) ((coefficients(TransNLS[[3]]$strip)[1] * x^e)
                                         / (1 / coefficients(TransNLS[[3]]$strip)[2] + x^e)),
                      color = "black", # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                      xlim = c(0, 5), size = 0.4) +
        stat_function(fun = function(x) ((coefficients(TransNLS[[4]]$strip)[1] * x^e)
                                         / (1 / coefficients(TransNLS[[4]]$strip)[2] + x^e)),
                      color = "black", # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                      xlim = c(0, 5), size = 0.4) +
        stat_function(fun = function(x) ((coefficients(TransNLS[[5]]$strip)[1] * x^e)
                                         / (1 / coefficients(TransNLS[[5]]$strip)[2] + x^e)),
                      color = "black", # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                      xlim = c(0, 5), size = 0.4) +
        stat_function(fun = function(x) ((coefficients(TransNLS[[6]]$strip)[1] * x^e)
                                         / (1 / coefficients(TransNLS[[6]]$strip)[2] + x^e)),
                      color = "black", # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                      xlim = c(0, 5), size = 0.4) +
        stat_function(fun = function(x) ((coefficients(TransNLS[[7]]$strip)[1] * x^e)
                                         / (1 / coefficients(TransNLS[[7]]$strip)[2] + x^e)),
                      color = "black", # ggsci::pal_npg("nrc", alpha = 0.7)(2)[1],
                      xlim = c(0, 5), size = 0.4))
}
