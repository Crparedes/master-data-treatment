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
files = c(paste0("C1V1L-", rep), paste0("C1V1R-", rep))
velo1 = matrix(ncol = 12, nrow = length(files), dimnames = list(1:length(files)))
par(mfrow = c(length(rep), 2), mar=c(1.5, 0.5, 2, 0.2), oma = c(4, 4, 0.2, 0.2))

for (i in 1:length(files)) {
  data <- read.table(file = paste0("Vel1/", files[i]), skip = 2, sep = ',')
  rownames(velo1)[i] <- files[i]
  velo1[i, 1] <- RPM(data$V4, n = 100, m = 5, plot = TRUE, name = files[i])
  robust <- RobustRPM(data$V4, frac = 7, n = 100, m = 5)
  velo1[i, 2] <- mean(robust)
  velo1[i, 3] <- sd(robust)
  velo1[i, 4] <- median(robust)
  velo1[i, 5] <- min(robust)
  velo1[i, 6] <- max(robust)
}
velo1 -> velo
velo
colMeans(velo[1:6, ])
apply(velo[1:6, ], 2, sd)
colMeans(velo[7:12, ])
apply(velo[1:6, ], 2, sd)
###########################################
rep <- 1:6
files = c(paste0("C1V2L-", rep), paste0("C1V2R-", rep))
velo2 = matrix(ncol = 12, nrow = length(files), dimnames = list(1:length(files)))
par(mfrow = c(length(rep), 2), mar=c(1.5, 0.5, 2, 0.2), oma = c(4, 4, 0.2, 0.2))

for (i in 1:length(files)) {
  data <- read.table(file = paste0("Vel2/", files[i]), skip = 2, sep = ',')
  rownames(velo2)[i] <- files[i]
  velo2[i, 1] <- RPM(data$V4, n = 200, m = 5, plot = TRUE, name = files[i])
  robust <- RobustRPM(data$V4, frac = 7, n = 100, m = 5)
  velo2[i, 2] <- mean(robust)
  velo2[i, 3] <- sd(robust)
  velo2[i, 4] <- median(robust)
  velo2[i, 5] <- min(robust)
  velo2[i, 6] <- max(robust)
}
velo2 -> velo
velo
colMeans(velo[1:6, ])
apply(velo[1:6, ], 2, sd)
colMeans(velo[7:12, ])
apply(velo[1:6, ], 2, sd)
###########################################
rep <- 1:6
files = c(paste0("C1V3L-", rep), paste0("C1V3R-", rep))
velo3 = matrix(ncol = 12, nrow = length(files), dimnames = list(1:length(files)))
par(mfrow = c(length(rep), 2), mar=c(1.5, 0.5, 2, 0.2), oma = c(4, 4, 0.2, 0.2))

for (i in 1:length(files)) {
  data <- read.table(file = paste0("Vel3/", files[i]), skip = 2, sep = ',')
  rownames(velo3)[i] <- files[i]
  velo3[i, 1] <- RPM(data$V4, n = 200, m = 5, plot = TRUE, name = files[i])
  robust <- RobustRPM(data$V4, frac = 7, n = 100)
  velo3[i, 2] <- mean(robust)
  velo3[i, 3] <- sd(robust)
  velo3[i, 4] <- median(robust)
  velo3[i, 5] <- min(robust)
  velo3[i, 6] <- max(robust)
}
velo3 -> velo
velo
colMeans(velo[1:6, ])
apply(velo[1:6, ], 2, sd)
colMeans(velo[7:12, ])
apply(velo[1:6, ], 2, sd)
###########################################
rep <- 1:6
files = c(paste0("C1V4L-", rep), paste0("C1V4R-", rep))
velo4 = matrix(ncol = 12, nrow = length(files), dimnames = list(1:length(files)))
par(mfrow = c(length(rep), 2), mar=c(1.5, 0.5, 2, 0.2), oma = c(4, 4, 0.2, 0.2))

for (i in 1:length(files)) {
  data <- read.table(file = paste0("Vel4/", files[i]), skip = 2, sep = ',')
  rownames(velo4)[i] <- files[i]
  velo4[i, 1] <- RPM(data$V4, n = 200, m = 5, plot = TRUE, name = files[i])
  robust <- RobustRPM(data$V4, frac = 7, n = 100)
  velo4[i, 2] <- mean(robust)
  velo4[i, 3] <- sd(robust)
  velo4[i, 4] <- median(robust)
  velo4[i, 5] <- min(robust)
  velo4[i, 6] <- max(robust)
}
velo4 -> velo
velo
colMeans(velo[1:6, ])
apply(velo[1:6, ], 2, sd)
colMeans(velo[7:12, ])
apply(velo[1:6, ], 2, sd)
###########################################
rep <- 1:6
files = c(paste0("C1V5L-", rep), paste0("C1V5R-", rep))
velo5 = matrix(ncol = 12, nrow = length(files), dimnames = list(1:length(files)))
par(mfrow = c(length(rep), 2), mar=c(1.5, 0.5, 2, 0.2), oma = c(4, 4, 0.2, 0.2))

for (i in 1:length(files)) {
  data <- read.table(file = paste0("Vel5/", files[i]), skip = 2, sep = ',')
  rownames(velo5)[i] <- files[i]
  velo5[i, 1] <- RPM(data$V4, n = 200, m = 5, plot = TRUE, name = files[i])
  robust <- RobustRPM(data$V4, frac = 7, n = 100)
  velo5[i, 2] <- mean(robust)
  velo5[i, 3] <- sd(robust)
  velo5[i, 4] <- median(robust)
  velo5[i, 5] <- min(robust)
  velo5[i, 6] <- max(robust)
}
velo5 -> velo
velo
colMeans(velo[1:6, ])
apply(velo[1:6, ], 2, sd)
colMeans(velo[7:12, ])
apply(velo[1:6, ], 2, sd)
###########################################
rep <- 1:6
files = c(paste0("C1V6L-", rep), paste0("C1V6R-", rep))
velo6 = matrix(ncol = 12, nrow = length(files), dimnames = list(1:length(files)))
par(mfrow = c(length(rep), 2), mar=c(1.5, 0.5, 2, 0.2), oma = c(4, 4, 0.2, 0.2))

for (i in 1:length(files)) {
  data <- read.table(file = paste0("Vel6/", files[i]), skip = 2, sep = ',')
  rownames(velo6)[i] <- files[i]
  velo6[i, 1] <- RPM(data$V4, n = 200, m = 3, plot = TRUE, name = files[i])
  robust <- RobustRPM(data$V4, frac = 7, n = 100)
  velo6[i, 2] <- mean(robust)
  velo6[i, 3] <- sd(robust)
  velo6[i, 4] <- median(robust)
  velo6[i, 5] <- min(robust)
  velo6[i, 6] <- max(robust)
}
velo6 -> velo
velo
colMeans(velo[1:6, ])
apply(velo[1:6, ], 2, sd)
colMeans(velo[7:12, ])
apply(velo[1:6, ], 2, sd)


pdf("Tracker.pdf", height = 5, width = 5)
data <- read.table(file = "Vel1/C1V1R-5", skip = 2, sep = ',')
RPM(data$V4, n = 50, m = 5, plot = TRUE)
data <- read.table(file = "Vel6/C1V6L-4", skip = 2, sep = ',')
RPM(data$V4, n = 50, m = 3, plot = TRUE)
dev.off()
