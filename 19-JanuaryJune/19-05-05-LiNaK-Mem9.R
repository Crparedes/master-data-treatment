#library(ggplot2)
#library(ggformula)
library(transMem)
#-----STOCK SOLUTIONS--------------------------------------------------------
StockLi.1 <- 129.5 * 0.187872 * 0.99 / 0.1200962
StockNa.1 <- 0.0861 * 22990
StockK.1  <- 0.8264 * 39098

StockLi.1.5_1  <- StockLi.1 * 1.2484 / 50.2322
StockLi.1.5_2  <- StockLi.1 * 1.2377 / 50.1505
StockNa.1.20_1 <- StockNa.1  * 0.5136 / 5.1747 * 1.0230 / 10.0315
StockK.1.12_1  <- StockK.1 * 0.2006 / 10.0910 * 0.2227 / 11.9819
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
LithiumCC.1 <- data.frame(Conc = c(0.0000, 0.0652, 0.1340, 0.3044, 0.6083, 1.2322, 2.3922, 3.0645) *
                            StockLi.1.5_1 / c(6.0000, 6.0075, 6.0268, 6.0103, 6.0341, 6.0633, 6.0103, 6.0626),
                          Signal = c(0.000, 0.009, 0.018, 0.041, 0.081, 0.171, 0.310, 0.383))
Lith.NaCC.1 <- data.frame(Conc = c(0.0000, 0.0592, 0.1205, 0.2446, 0.5938, 1.2562, 2.4378, 3.0479) *
                            StockLi.1.5_1 / c(6.0000, 6.0992, 6.2358, 6.1827, 6.0371, 7.1252, 6.0531, 6.4249),
                          Signal = c(0.000, 0.009, 0.016, 0.042, 0.084, 0.149, 0.342, 0.378))
Lith.KCC.1  <- data.frame(Conc = c(0.0000, 0.0646, 0.1187, 0.2681, 0.4645, 1.0323, 2.0837, 3.2906) *
                            StockLi.1.5_2 / c(6.0000, 6.1436, 6.0954, 6.0805, 6.2874, 6.1209, 6.0611, 6.1164),
                          Signal = c(0.000, 0.008, 0.016, 0.036, 0.059, 0.135, 0.266, 0.292))

SodiumCC.1 <- data.frame(Conc = c(0.0000, 0.0548, 0.1433, 0.3205,        0.9372, 1.4929, 2.0799, 3.0523) *
                           StockNa.1.20_1 / c(6.0000, 6.1328, 6.0089, 6.1254,        6.1531, 6.1615, 6.1384, 6.1277),
                         Signal  = c(0.000, 0.021, 0.096, 0.153,        0.423, 0.679, 0.914, 1.229))
PotasiCC.1 <- data.frame(Conc = c(0.0000, 0.0500, 0.2295, 0.5153, 1.0036, 1.5286, 2.5255, 3.5000, 3.2500) *
                           StockK.1.12_1 / c(6.0000, 6.0302, 6.0539, 6.0109, 6.1441, 6.0810, 6.1211, 6.1925, 4.3117),
                         Signal = c(0.000, 0.001, 0.004, 0.008, 0.014, 0.018, 0.025, 0.029, 0.033))
#-----MODELOS DE LAS CURVAS--------------------------------------------------
calibCurve(curve = LithiumCC.1, badpoint = 8)
calibCurve(curve = Lith.NaCC.1, badpoint = 8)
calibCurve(curve = Lith.KCC.1, badpoint = 8)
calibCurve(curve = SodiumCC.1, order = 2)
calibCurve(curve = PotasiCC.1, order = 2, badpoint = 9)
#-----TIEMPOS DE LA TOMA DE ALÍCUOTAS----------------------------------------
Time.9.1 <- Time.9.2 <- c(0, 0.5, 1, 2, 3, 4.5, 6, 7, 8)
Time.9.3 <- Time.9.4 <- c(0, 1, 2, 2.5, 3, 4, 5.5, 7, 8)
Time.9.5 <- c(0, 0.5, 1, 1.75, 2.5, 3.5, 4.5, 6.5, 7)
#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------

dilFeed.9.1  <- c(6.0316/0.0630, 6.1093/0.0580, 6.1674/0.0594, 6.3258/0.0666, 6.0613/0.0632)
dilStrip.9.1 <- c(2.0694/0.2211, 2.0467/0.2017, 2.7017/0.1991, 2.2480/0.1968, 2.0072/0.2111)

dilFeed.9.2  <- c(6.2507/0.0622, 6.2177/0.0626, 6.1075/0.0633, 6.0882/0.0619, 6.2626/0.0628)
dilStrip.9.2 <- c(2.1284/0.2104, 2.1016/0.2071, 2.0175/0.2113, 2.0508/0.2128, 2.3938/0.2123)

dilFeed.9.3  <- c(5.9975/0.0584, 6.2463/0.0635, 6.2366/0.0613, 6.2103/0.0622, 6.4919/0.0619)
dilStrip.9.3 <- c(2.0400/0.2097, 2.0716/0.2136, 2.0115/0.2140, 2.0680/0.2135, 2.0168/0.2135)

dilFeed.9.4  <- c(6.1062/0.0624, 6.2082/0.0624, 6.0671/0.0646, 6.3980/0.0633, 6.2946/0.0556)
dilStrip.9.4 <- c(2.0213/0.2160, 2.0213/0.2177, 2.0616/0.2176, 2.4565/0.2144, 2.0173/0.2166)

dilFeed.9.5  <- c(6.1840/0.0625, 6.0822/0.0633, 6.2735/0.0623, 6.0341/0.0626, 6.1625/0.0613)
dilStrip.9.5 <- c(2.0086/0.2112, 2.0809/0.2120, 2.2068/0.2136, 2.0597/0.2128, 2.1579/0.2136)
#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
SignalFeed.9.1.Li   <- c(0.305, 0.263, 0.221, 0.155, 0.115, 0.071, 0.043, 0.029, 0.021)
SignalStrip.9.1.Li  <- c(0.002, 0.046, 0.089, 0.193, 0.192, 0.234, 0.263, 0.275, 0.297)
SignalFeed.9.1.Na   <- c(0.376, 0.332, 0.336, 0.352, 0.338)
SignalStrip.9.1.Na  <- c(0.000, 0.043, 0.101, 0.234, 0.366)

SignalFeed.9.2.Li   <- c(0.306, 0.246, 0.192, 0.152, 0.156, 0.157, 0.158, 0.163, 0.167)
SignalStrip.9.2.Li  <- c(0.004, 0.074, 0.131, 0.176, 0.177, 0.174, 0.172, 0.170, 0.170)
SignalFeed.9.2.K    <- c(0.025, 0.024, 0.022, 0.020, 0.018)
SignalStrip.9.2.K   <- c(0.001, 0.025, 0.025, 0.025, 0.025)

SignalFeed.9.3.Li   <- c(0.310, 0.242, 0.200, 0.179, 0.162, 0.131, 0.093, 0.066, 0.055)
SignalStrip.9.3.Li  <- c(0.001, 0.061, 0.107, 0.125, 0.143, 0.175, 0.213, 0.242, 0.256)
SignalFeed.9.3.K    <- c(0.025, 0.024, 0.024, 0.023, 0.023)
SignalStrip.9.3.K   <- c(0.000, 0.002, 0.004, 0.006, 0.008)

SignalFeed.9.4.Li   <- c(0.307, 0.125, 0.052, 0.034, 0.022, 0.010, 0.003, 0.001, 0.000)
SignalStrip.9.4.Li  <- c(0.001, 0.176, 0.246, 0.265, 0.279, 0.287, 0.299, 0.300, 0.300)
SignalFeed.9.4.Na   <- c(0.366, 0.355, 0.367, 0.317, 0.276)
SignalStrip.9.4.Na  <- c(0.000, 0.254, 0.270, 0.357, 0.560)

SignalFeed.9.5.Li   <- c(0.311, 0.255, 0.211, 0.162, 0.127, 0.093, 0.066, 0.032, 0.028)
SignalStrip.9.5.Li  <- c(0.000, 0.060, 0.105, 0.151, 0.180, 0.205, 0.226, 0.250, 0.252)
SignalFeed.9.5.K    <- c(0.024, 0.024, 0.023, 0.024, 0.023)
SignalStrip.9.5.K   <- c(0.001, 0.016, 0.026, 0.026, 0.026)

#-----CONCENTRACIÓN DE ESPECIES EN LAS ALÍCUOTAS-----------------------------
signal2conc(signal = SignalFeed.9.1.Li, model = model.Lith.NaCC.1)
signal2conc(signal = SignalStrip.9.1.Li, model = model.LithiumCC.1)
signal2conc(signal = SignalFeed.9.1.Na, model = model.SodiumCC.1, dilution = dilFeed.9.1)
signal2conc(signal = SignalStrip.9.1.Na, model = model.SodiumCC.1, dilution = dilStrip.9.1)

signal2conc(signal = SignalFeed.9.2.Li, model = model.Lith.KCC.1)
signal2conc(signal = SignalStrip.9.2.Li, model = model.LithiumCC.1)
signal2conc(signal = SignalFeed.9.2.K, model = model.PotasiCC.1, dilution = dilFeed.9.2)
signal2conc(signal = SignalStrip.9.2.K, model = model.PotasiCC.1, dilution = dilStrip.9.2)

signal2conc(signal = SignalFeed.9.3.Li, model = model.Lith.KCC.1)
signal2conc(signal = SignalStrip.9.3.Li, model = model.LithiumCC.1)
signal2conc(signal = SignalFeed.9.3.K, model = model.PotasiCC.1, dilution = dilFeed.9.3)
signal2conc(signal = SignalStrip.9.3.K, model = model.PotasiCC.1, dilution = dilStrip.9.3)

signal2conc(signal = SignalFeed.9.4.Li, model = model.Lith.NaCC.1)
signal2conc(signal = SignalStrip.9.4.Li, model = model.LithiumCC.1)
signal2conc(signal = SignalFeed.9.4.Na, model = model.SodiumCC.1, dilution = dilFeed.9.4)
signal2conc(signal = SignalStrip.9.4.Na, model = model.SodiumCC.1, dilution = dilStrip.9.4)

signal2conc(signal = SignalFeed.9.5.Li, model = model.Lith.KCC.1)
signal2conc(signal = SignalStrip.9.5.Li, model = model.LithiumCC.1)
signal2conc(signal = SignalFeed.9.5.K, model = model.PotasiCC.1, dilution = dilFeed.9.5)
signal2conc(signal = SignalStrip.9.5.K, model = model.PotasiCC.1, dilution = dilStrip.9.5)

#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
conc2frac(feed = ConcFeed.9.1.Li, strip = ConcStrip.9.1.Li, time = Time.9.1)
conc2frac(feed = ConcFeed.9.1.Na, strip = ConcStrip.9.1.Na, time = Time.9.1[c(1, 3, 5, 7, 9)])
conc2frac(feed = ConcFeed.9.2.Li, strip = ConcStrip.9.2.Li, time = Time.9.2)
conc2frac(feed = ConcFeed.9.2.K, strip = ConcStrip.9.2.K, time = Time.9.2[c(1, 3, 5, 7, 9)])
conc2frac(feed = ConcFeed.9.3.Li, strip = ConcStrip.9.3.Li, time = Time.9.3)
conc2frac(feed = ConcFeed.9.3.K, strip = ConcStrip.9.3.K, time = Time.9.3[c(1, 3, 5, 7, 9)])
conc2frac(feed = ConcFeed.9.4.Li, strip = ConcStrip.9.4.Li, time = Time.9.4)
conc2frac(feed = ConcFeed.9.4.Na, strip = ConcStrip.9.4.Na, time = Time.9.4[c(1, 3, 5, 7, 9)])
conc2frac(feed = ConcFeed.9.5.Li, strip = ConcStrip.9.5.Li, time = Time.9.5)
conc2frac(feed = ConcFeed.9.5.K, strip = ConcStrip.9.5.K, time = Time.9.5[c(1, 3, 5, 7, 9)])
#-----MODELOS DE REGRESIÓN NO LINEAL-----------------------------------------
transTrend(Transport.9.1.Li)
transTrend(Transport.9.2.Li)
transTrend(Transport.9.3.Li)
transTrend(Transport.9.4.Li)
transTrend(Transport.9.5.Li)
#-----PERFILES DE TRANSPORTE ------------------------------------------------
pdf("Perfiles010519.pdf", height = 5, width = 10)
transPlot(trans = Transport.9.1.Li, trend = nls.9.1.Li, secondary = Transport.9.1.Na)
transPlot(trans = Transport.9.2.Li, trend = nls.9.2.Li, secondary = Transport.9.2.K)
transPlot(trans = Transport.9.3.Li, trend = nls.9.3.Li, secondary = Transport.9.3.K)
transPlot(trans = Transport.9.4.Li, trend = nls.9.4.Li, secondary = Transport.9.4.Na)
transPlot(trans = Transport.9.5.Li, trend = nls.9.5.Li, secondary = Transport.9.5.K)
dev.off()
#-----PRUEBA T DE IGUALDAD DE MEDIAS-----------------------------------------
Feed.Na <- c(1.0624, 1.0176, Transport.9.1.Li[18, 3], Transport.9.4.Li[18, 3])
Feed.K  <- c(Transport.9.2.Li[18, 3], Transport.9.3.Li[18, 3], Transport.9.5.Li[18, 3])
t.test(x = Feed.Na, y = Feed.K, var.equal = FALSE, alternative = 'greater')
var.test(x = Feed.Na, y = Feed.K)
