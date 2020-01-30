find_peaks <- function (x, m = 3) {
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i) {
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

RPM <- function (lum, fps = 240, m = 3, n = 50, plot = FALSE, name = NULL) {
  peaks <- find_peaks(x = lum, m = m)
  if (plot) {
    if (missing(name)) {
      plot(lum, xlim = c(1, n), type = 'o', xlab = 'Fotograma', ylab = 'Luminancia (lumas)')
    } else {
      plot(lum, xlim = c(1, n), type = 'o', main = name, xlab = 'Fotograma', ylab = 'Luminancia (lumas)')
    }
    points(x = peaks, y = lum[peaks], col = 2, pch = 8)
  }
  rev = length(peaks) / 2
  time = length(lum) / 240 / 60
  return(rev / time)
}

RobustRPM <- function (lum, frac = 10, fps = 240, n = 50, m = 3, plot = FALSE) {
  ngr <- trunc(length(lum) / frac)
  values <- vector()
  for (i in 1:frac) {
    values <- c(values, RPM(lum = lum[((i - 1) * ngr + 1):(i * ngr)],
                fps = fps, n = n, m = m, plot = plot))
  }
  return(values)
}

###########################################
rep <- 1:6
files = list.files()[1:36]
velo1 = matrix(ncol = 6, nrow = length(files), dimnames = list(1:length(files)))
par(mfrow = c(length(rep), 2), mar=c(1.5, 0.5, 2, 0.2), oma = c(4, 4, 0.2, 0.2))

for (i in 1:length(files)) {
  data <- read.table(file = files[i], skip = 2, sep = ',')
  rownames(velo1)[i] <- files[i]
  velo1[i, 1] <- RPM(data$V4, n = 100, m = 5, plot = TRUE, name = files[i])
  robust <- RobustRPM(data$V4, frac = 7, n = 100, m = 5)
  velo1[i, 2] <- mean(robust)
  velo1[i, 3] <- sd(robust)
  velo1[i, 4] <- median(robust)
  velo1[i, 5] <- min(robust)
  velo1[i, 6] <- max(robust)
}
(veloR <- velo1[c(2, 4, 6, 8), ]); (veloL <- velo1[c(1, 3, 5, 7), ])
colMeans(veloR); apply(veloR, 2, sd)
colMeans(veloL); apply(veloL, 2, sd)

(veloR <- velo1[c(10, 12), ]); (veloL <- velo1[c(9, 11), ])
colMeans(veloR); apply(veloR, 2, sd)
colMeans(veloL); apply(veloL, 2, sd)

(veloR <- velo1[c(14, 16), ]); (veloL <- velo1[c(13, 15), ])
colMeans(veloR); apply(veloR, 2, sd)
colMeans(veloL); apply(veloL, 2, sd)

(veloR <- velo1[c(18), ]); (veloL <- velo1[c(17), ])
colMeans(veloR); apply(veloR, 2, sd)
colMeans(veloL); apply(veloL, 2, sd)

(veloR <- velo1[c(20, 22, 24), ]); (veloL <- velo1[c(19, 21, 23), ])
colMeans(veloR); apply(veloR, 2, sd)
colMeans(veloL); apply(veloL, 2, sd)

(veloR <- velo1[c(26, 28, 30), ]); (veloL <- velo1[c(25, 27, 29), ])
colMeans(veloR); apply(veloR, 2, sd)
colMeans(veloL); apply(veloL, 2, sd)

(veloR <- velo1[c(32), ]); (veloL <- velo1[c(31), ])

pdf("Tracker.pdf", height = 5, width = 5)
data <- read.table(file = "Vel1/C1V1R-5", skip = 2, sep = ',')
RPM(data$V4, n = 50, m = 5, plot = TRUE)
data <- read.table(file = "Vel6/C1V6L-4", skip = 2, sep = ',')
RPM(data$V4, n = 50, m = 3, plot = TRUE)
dev.off()
