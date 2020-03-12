library(transmem)
####################################################
#### curvelithium Data.
#### concentrationcycles Data. Original file: 19-10-18_Li-Preconcentration_Mem18.R
StockLi.5_7   <- 130.3 * 0.187872 * 0.99 / 0.1205105 * 1.2673 / 50.0164
curvelithium <- data.frame(Conc = c(0.0000, 0.0597, 0.2989, 0.9115, 1.2053, 1.6816, 2.1670, 2.6474) *
                         StockLi.5_7 / c(6.0000, 6.1144, 6.0524, 6.0355, 6.1449, 6.0859, 6.0838, 6.0795),
                       Signal = c(0.000, 0.033, 0.161, 0.483, 0.619, 0.848, 1.050, 1.251))

#save(curvelithium, file = "/home/cris/Dropbox/transmem/data/curvelithium.RData")
CalModel <- calibCurve(curve = curvelithium, order = 2, plot = TRUE)
AliTimes <- list(c(0, 1.5, 3, 4.75, 6),
                 c(6.01, 7.5, 9, 10.5, 12),
                 c(12.01, 13.5, 15.25, 16.5, 18),
                 c(18.01, 19.5, 21, 22.5, 24),
                 c(24.01, 25.5, 27, 28.5, 30))
dilutions <- list(Strip.1 = rep(1, 5),
                  Strip.2 = c(8517/3701, 8296/3734, 7863/3439, 8278/3662, 7967/3554),
                  Strip.3 = c(12305/3293, 11113/3265, 11160/3267, 11150/3277, 11142/3281),
                  Strip.4 = c(12838/2797, 12759/2807, 12747/2775, 12740/2780, 12666/2760),
                  Strip.5 = c(12582/2758, 13710/2775, 13681/2753, 13662/2769, 13718/2786))
AliAbs <- list(Feed.1 = c(1.170, 0.601, 0.358, 0.208, 0.155),
               Feed.2 = c(1.140, 0.750, 0.554, 0.427, 0.341),
               Feed.3 = c(1.160, 0.871, 0.672, 0.581, 0.499),
               Feed.4 = c(1.157, 0.940, 0.788, 0.688, 0.610),
               Feed.5 = c(1.156, 0.983, 0.868, 0.782, 0.727),
               Strip.1 = c(0.005, 0.574, 0.798, 0.909, 0.930),
               Strip.2 = c(0.483, 0.687, 0.760, 0.829, 0.850),
               Strip.3 = c(0.552, 0.690, 0.745, 0.773, 0.802),
               Strip.4 = c(0.614, 0.687, 0.714, 0.735, 0.751),
               Strip.5 = c(0.752, 0.749, 0.771, 0.790, 0.805))
concentrationcycles <- list()
for (i in 1:5) {
  feed  <- signal2conc(signal = AliAbs[[i]], model = CalModel)
  strip <- signal2conc(signal = AliAbs[[5+i]], model = CalModel, dilution = dilutions[[i]])
  concentrationcycles[[i]] <- conc2frac(feed = feed, strip = strip, time = AliTimes[[i]], normalize = FALSE)
}
cyclesPlot(trans = concentrationcycles, ylab = expression(paste('Conc. (mg k', g^{-1}, ')')))
save(concentrationcycles, file = "/home/cris/Dropbox/transmem/data/concentrationcycles.RData")


####################################################
#### reusecycles Data. Original file: 19-10-25-Li-Reuse_Mem19.R
StockLi.5_8   <- 130.3 * 0.187872 * 0.99 / 0.1205105 * 0.2646 / 10.3841
cyclesaliquots <- list(feed.1 = c(1.115, 0.622, 0.342, 0.194, 0.116),
                       strip.1= c(0.000, 0.636, 0.898, 1.021, 1.078),
                       feed.2 = c(1.138, 0.751, 0.482, 0.321, 0.217),
                       strip.2 = c(0.006, 0.578, 0.852, 0.983, 1.059),
                       feed.3 = c(1.142, 0.713, 0.518, 0.361, 0.264),
                       strip.3 = c(0.006, 0.602, 0.802, 0.938, 1.015),
                       feed.4 = c(1.167, 0.809, 0.573, 0.402, 0.300),
                       strip.4 = c(0.006, 0.499, 0.757, 0.908, 0.990),
                       feed.5 = c(1.136, 0.850, 0.609, 0.400, 0.335),
                       strip.5 = c(0.008, 0.541, 0.793, 0.900, 1.046),
                       feed.6 = c(1.215, 0.921, 0.693, 0.535, 0.417),
                       strip.6 = c(0.003, 0.451, 0.706, 0.856, 0.963),
                       feed.7 = c(1.209, 0.923, 0.726, 0.582, 0.470),
                       strip.7 = c(0.004, 0.440, 0.687, 0.830, 0.935),
                       feed.8 = c(1.234, 0.979, 0.789, 0.658, 0.553),
                       strip.8 = c(0.005, 0.402, 0.630, 0.764, 0.863),
                       feed.9 = c(1.249, 1.014, 0.814, 0.682, 0.587),
                       strip.9 = c(0.008, 0.377, 0.621, 0.758, 0.846),
                       feed.10 = c(1.248, 1.028, 0.874, 0.738, 0.656),
                       strip.10 = c(0.007, 0.355, 0.565, 0.714, 0.792))
CalCurves <- list(
  Lithium.1 = data.frame(Conc = c(0.0000, 0.0685, 0.2891, 0.9453, 1.1944, 1.6853, 2.2121, 2.7284) *
                                StockLi.5_8 / c(6.0000, 6.6725, 6.3378, 6.6590, 6.6845, 6.6557, 6.8208, 6.6317),
                         Signal = c(0.000, 0.036, 0.162, 0.477, 0.586, 0.797, 0.971, 1.169)),
  Lithium.2 = data.frame(Conc = c(0.0000, 0.0597, 0.2989, 0.9115, 1.2053, 1.6816, 2.1670, 2.6474) *
                           StockLi.5_8 / c(6.0000, 6.1144, 6.0524, 6.0355, 6.1449, 6.0859, 6.0838, 6.0795),
                         Signal = c(0.000, 0.037, 0.168, 0.497, 0.618, 0.839, 1.032, 1.241))
)
CalModels <- list()
for (i in 1:2) CalModels[[i]] <- calibCurve(curve = CalCurves[[i]], order = 2, plot = TRUE)
names(CalModels) <- names(CalCurves)
AliTimes <- c(0, 1.5, 3, 4.75, 6, 6.01, 7.5, 9, 10.5, 12, 12.01, 13.5, 15.25,
              16.5, 18, 18.01, 19.5, 21, 22.5, 24, 24.01, 25.5, 27, 28.5, 30,
              30.01, 31.5, 33, 34.5, 36, 36.01, 37.5, 39, 40.5, 42, 42.01,
              43.5, 45, 46.5, 48, 48.01, 49.5, 51, 52.5, 54, 54.01, 55.5, 57, 60)
AliAbs <- list(Cycle.1.F.b = c(1.115, 0.622, 0.342, 0.194, 0.116),
               Cycle.1.S.b = c(0.000, 0.636, 0.898, 1.021, 1.078),
               Cycle.2.F.b = c(1.138, 0.751, 0.482, 0.321, 0.217),
               Cycle.2.S.b = c(0.006, 0.578, 0.852, 0.983, 1.059),
               Cycle.3.F.b = c(1.142, 0.713, 0.518, 0.361, 0.264),
               Cycle.3.S.b = c(0.006, 0.602, 0.802, 0.938, 1.015),
               Cycle.4.F.b = c(1.167, 0.809, 0.573, 0.402, 0.300),
               Cycle.4.S.b = c(0.006, 0.499, 0.757, 0.908, 0.990),
               Cycle.5.F.b = c(1.136, 0.850, 0.609, 0.400, 0.335),
               Cycle.5.S.b = c(0.008, 0.541, 0.793, 0.900, 1.046),
               Cycle.6.F.b = c(1.215, 0.921, 0.693, 0.535, 0.417),
               Cycle.6.S.b = c(0.003, 0.451, 0.706, 0.856, 0.963),
               Cycle.7.F.b = c(1.209, 0.923, 0.726, 0.582, 0.470),
               Cycle.7.S.b = c(0.004, 0.440, 0.687, 0.830, 0.935),
               Cycle.8.F.b = c(1.234, 0.979, 0.789, 0.658, 0.553),
               Cycle.8.S.b = c(0.005, 0.402, 0.630, 0.764, 0.863),
               Cycle.9.F.b = c(1.249, 1.014, 0.814, 0.682, 0.587),
               Cycle.9.S.b = c(0.008, 0.377, 0.621, 0.758, 0.846),
               Cycle.10.F.b = c(1.248, 1.028, 0.874, 0.738, 0.656),
               Cycle.10.S.b = c(0.007, 0.355, 0.565, 0.714, 0.792))
AliConc <- vector(mode = "list", length = length(AliAbs))
names(AliConc) <- names(AliAbs)
for (i in 1:length(AliAbs)) {
  if(i %in% c(1:12)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Lithium.1)
  if(i %in% c(13:20)) AliConc[[i]] <- signal2conc(signal = AliAbs[[i]], model = CalModels$Lithium.2)
}
TransFrac <- vector(mode = "list", length = length(AliAbs) / 2)
names(TransFrac) <- names(AliAbs)[seq(1, 20, 2)]
for (i in 1:10) {
  TransFrac[[i]] <- conc2frac(feed = AliConc[[2 * i - 1]], strip = AliConc[[2 * i]],
                              time = c(0, 1.5, 3, 4.5, 6))
}

reusecycles = list(TransFrac[[1]], TransFrac[[2]], TransFrac[[3]], TransFrac[[4]], TransFrac[[5]],
              TransFrac[[6]], TransFrac[[7]], TransFrac[[8]], TransFrac[[9]], TransFrac[[10]])
permcoef(trans = reusecycles[[1]], conc_0 = AliConc[[1]][1], vol = 85, area = pi*1.25^2)
#save(reusecycles, file = "/home/cris/Dropbox/transmem/data/reusecycles.RData")

####################################################
#### planelithium Data. Original file: 19-05-21-LiNa-Mem10.R
StockLi.1.5_3   <- 129.5 * 0.187872 * 0.99 / 0.1200962 * 1.2463 / 50.0083
StockNa.1.200_1 <- 0.0861 * 22990 * 5.0087 / 50.0536
CalCurves <- list(
  Lithium.1 = data.frame(Conc = c(0.0000, 0.0615, 0.1223, 0.3027, 0.5948, 1.2023, 2.4034, 2.7270) *
                           StockLi.1.5_3 / c(6.0000, 6.0084, 6.0084, 6.0344, 6.1404, 6.0674, 6.0297, 6.1499),
                         Signal = c(0.000, 0.008, 0.016, 0.039, 0.077, 0.155, 0.306, 0.339),
                         Conc.S = rep(0.000, 8) *
                           StockNa.1.200_1 / c(6.0000, 6.0084, 6.0084, 6.0344, 6.1404, 6.0674, 6.0297, 6.1499)),
  Lithium.2 = data.frame(Conc = c(0.0000, 0.0624, 0.1180, 0.2995, 0.6141, 1.2080, 2.4169, 2.7044) *
                           StockLi.1.5_3 / c(6.0075, 6.1143, 6.0310, 6.0232, 6.0195, 6.0341, 6.0633, 6.0151),
                         Signal = c(0.000, 0.008, 0.016, 0.041, 0.083, 0.164, 0.313, 0.349),
                         Conc.S = c(0.6026, 0.6020, 0.6056, 0.6257, 0.5895, 0.6113, 0.5844, 0.6145) *
                           StockNa.1.200_1 / c(6.0075, 6.1143, 6.0310, 6.0232, 6.0195, 6.0341, 6.0633, 6.0151)),
  Lithium.3 = data.frame(Conc = c(0.0000, 0.0654, 0.1176, 0.2990, 0.6088, 1.2090, 2.4115, 2.6965) *
                           StockLi.1.5_3 / c(6.0843, 6.0435, 6.0321, 6.0317, 6.0415, 6.0085, 6.0777, 6.1473),
                         Signal = c(0.000, 0.008, 0.016, 0.040, 0.082, 0.164, 0.313, 0.346),
                         Conc.S = c(1.2087, 1.2158, 1.2168, 1.1970, 1.2015, 1.2135, 1.2132, 1.2024) *
                           StockNa.1.200_1 / c(6.0843, 6.0435, 6.0321, 6.0317, 6.0415, 6.0085, 6.0777, 6.1473)),
  Lithium.4 = data.frame(Conc = c(0.0000, 0.0649, 0.1172, 0.3176, 0.6101, 1.2036, 2.4131, 2.7617) *
                           StockLi.1.5_3 / c(6.0384, 6.0057, 6.0783, 6.0397, 6.0010, 6.0468, 6.0233, 6.0914),
                         Signal = c(0.000, 0.010, 0.017, 0.045, 0.088, 0.168, 0.327, 0.372),
                         Conc.S = c(0.6048, 0.6042, 0.6082, 0.6089, 0.6139, 0.5998, 0.5944, 0.6085) *
                           StockNa.1 / c(6.0384, 6.0057, 6.0783, 6.0397, 6.0010, 6.0468, 6.0233, 6.0914)),
  Lithium.5 = data.frame(Conc = c(0.0000, 0.0591, 0.1298, 0.3080, 0.6139, 1.2670, 2.4016, 2.7171) *
                           StockLi.1.5_3 / c(6.0586, 6.0102, 6.0702, 6.0508, 6.0309, 6.0551, 6.0384, 6.0498),
                         Signal = c(0.000, 0.008, 0.019, 0.043, 0.088, 0.177, 0.326, 0.367),
                         Conc.S = c(0.7660, 0.7576, 0.7689, 0.7575, 0.7589, 0.7513, 0.7594, 0.7603) *
                           StockNa.1 / c(6.0586, 6.0102, 6.0702, 6.0508, 6.0309, 6.0551, 6.0384, 6.0498))
)
planelithium <- rbind(CalCurves$Lithium.1, CalCurves$Lithium.2, CalCurves$Lithium.3,
                   CalCurves$Lithium.4, CalCurves$Lithium.5)
summary(calibPlane(planelithium)$model)
planelithium[, 1] <- round(planelithium[, 1], 2)
planelithium[, 3] <- round(planelithium[, 3], 1)
#save(planelithium, file = "/home/cris/Dropbox/transmem/data/planelithium.RData")

####################################################
#### seawaterLiNaK Data. Original file: 19-11-14-LiNaK-SSS_Mem22.R
source("/home/cris/Dropbox/0-UNAM/0-Polymeric Inclusion Membranes/master-data-treatment/19-JuneDecember/19-11-14-LiNaK-SSS_Mem22.R")

seawaterLiNaK <- list(Lithium.1 = TransFrac[[3]], Lithium.2 = TransFrac[[4]],
                      Sodium.1 = TransFrac[[5]], Sodium.2 = TransFrac[[6]],
                      Potassium.1 = TransFrac[[7]], Potassium.2 = TransFrac[[8]])

save(seawaterLiNaK, file = "/home/cris/Dropbox/transmem/data/seawaterLiNaK.RData")
data(seawaterLiNaK)
#'   sepfactor(main = seawaterLiNaK$Lithium.1,
#'             secon = seawaterLiNaK$Sodium.1)
#'   sepfactor(main = seawaterLiNaK$Lithium.1,
#'             secon = seawaterLiNaK$Potassium.1)
#'   sepfactor(main = seawaterLiNaK$Lithium.2,
#'             secon = seawaterLiNaK$Sodium.2)
#'   sepfactor(main = seawaterLiNaK$Lithium.2,
#'             secon = seawaterLiNaK$Potassium.2)
