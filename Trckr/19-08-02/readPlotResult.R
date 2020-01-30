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
c(340, 485, 520, 670, 890, 1000, 1240)
n <- 20
files <- c(paste0("Dat", 1:n, "_R"), c(paste0("Dat", 1:n, "_L")))
velo1 <- matrix(ncol = 6, nrow = length(files), dimnames = list(1:length(files)))
par(mfrow = c(5, 2), mar=c(1.5, 0.5, 2, 0.2), oma = c(4, 4, 0.2, 0.2))

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
velo1[1:20, ] -> veloR
colMeans(veloR[1:20, ])
apply(veloR[1:20, ], 2, sd)

vel <- list(v1 = c(11, 14, 16), v2 = c(8, 10, 17), v3 = c(7, 18, 20), v4 = c(12, 13, 19), v5 = c(1, 2, 4),
            v6 = c(3, 5, 15), v7 = c(6, 9))
for (i in 1:7) {
  print(paste0("Vel.", i, " Strip:"))
  print((velo1[vel[[i]], ]))
  print(paste0("Vel.", i, " Feed:"))
  print((velo1[(vel[[i]] + 20), ]))
  cat("\n")
}

velo1[v1, ]
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
