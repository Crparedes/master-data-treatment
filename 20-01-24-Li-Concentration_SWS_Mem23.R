library(transmem)
library(ggplot2)
PDF <- TRUE
if (PDF) pdf("SWS-23-01-19.pdf", height = 7/1.3, width = 9/1.3)

#-----STOCK SOLUTIONS--Prepared January 18th, 2020---------------------------
StockLi.622  <- 100.4 * 0.18787 * 0.990 / 0.0300314
StockNa.6627 <- 508.4 * 0.39337 * 0.995 / 0.0300249
StockK.1768  <- 101.8 * 0.52445 * 0.995 / 0.0300397
StockMg.979  <- 304.7 * 0.09479 * 1.014 / 0.0300828
StockCa.1349 <- 102.3 * 0.40043 * 0.990 / 0.0300715

StockLi.500u <- StockLi.622 * 0.6197 / 6.1725 * 0.4114 / 50.0378 * 1000
StockNa.20   <- StockNa.6627 * 0.1546 / 50.3288
StockK.10    <- StockK.1768 * 0.3235 / 50.0196
StockMg.2    <- StockMg.979 * 0.1182 / 51.2115
StockCa.10   <- StockCa.1349 * 0.3882 / 51.5038
#-----CALIBRATION CURVES AND MODELS------------------------------------------
CalCurves <- list(
  Lithium   = data.frame(Conc = c(0, 0.0572, 0.2463, 1.2093, 2.4374, 3.1012) * StockLi.500u / 
                           c(6, 6.1763, 6.1816, 6.1501, 6.2182, 6.1090),
                         Signal = c(0, 0.010, 0.044, 0.216, 0.427, 0.550)),
  LithiumSW = data.frame(Conc = c(0, 0.2340, 1.2107, 2.3974, 3.2936) * StockLi.500u / 
                           c(6, 6.0480, 6.0575, 6.0114, 6.0348),
                         Signal = c(0, 0.026, 0.211, 0.395, 0.607))
)
## for a cleaner workspace
#rm(list = ls()[grep("Stock", ls())])
#-----MODELOS DE LAS CURVAS--------------------------------------------------
CalModels <- list()
for (i in 1:2) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = 1, plot = TRUE)
names(CalModels) <- names(CalCurves)
summary(CalModels$Lithium)
summary(CalModels$LithiumSW)
#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
AliTimes <- c(0, 1, 3, 4.5, 4.51, 5.5, 7.5, 9, 9.01, 10, 12, 13.5, 13.51,
              14.5, 16.5, 18, 18.01, 19, 21, 22.5)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
AliAbs <- list(
  Strip.23.1.Li = c(0.000, 0.232, 0.372, 0.404,
                    0.406, 0.449, 0.466, 0.468,
                    0.462, 0.473, 0.466, 0.458,
                    0.450, 0.450, 0.438, 0.437,
                    0.434, 0.428, 0.418, 0.421),
  Feed.23.1.Li  = c(0.369, 0.173, 0.054, 0.031,
                    0.357, 0.338, 0.326, 0.330,
                    0.363, 0.357, 0.360, 0.363,
                    0.368, 0.363, 0.364, 0.367,
                    0.363, 0.368, 0.378, 0.374)
)
#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
AliConc <- vector(mode = "list", length = length(AliAbs))
names(AliConc) <- names(AliAbs)
for (i in 1:2) {
  AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels[[i]])
}
#-----PERFILES DE TRANSPORTE ------------------------------------------------
A <- data.frame(Phase = rep(c('Rec.', 'Alim.'), each = 20), Conc = c(AliConc[[1]], AliConc[[2]]), Tiempo = AliTimes)
ggplot(data = A, aes(x = Tiempo, y = Conc, color = Phase)) + geom_point() + theme_bw()

ggplot(data = A, aes(x = Tiempo, y = Conc, shape = Phase)) +
  geom_vline(xintercept = c(0, 4.5, 9, 13.5, 18), linetype = 'dashed', color = 'gray') +
  geom_point(size = 2) + theme_bw() +
#  geom_errorbar(aes(ymin = Conc - 5, ymax = Conc + 5), width = 0.4) +
  scale_x_continuous(breaks = seq(0, 24, 2), limits = c(-0.2, 22.7)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")) +
  scale_color_manual(values = c("black", "red")) +
  labs(y = expression(paste('Conc. (mg k', g^{-1}, ')')), x = 'Time (h)') +
  theme(text = element_text(size = 9), legend.position = "none")
if (PDF) dev.off()
