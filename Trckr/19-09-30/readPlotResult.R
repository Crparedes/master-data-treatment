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
files = c("17-01-A-F", "17-01-A-S", "17-02-B-F", "17-02-B-S")
velo1 = matrix(ncol = 12, nrow = length(files), dimnames = list(1:length(files)))
par(mfrow = c(2, 2), mar=c(1.5, 0.5, 2, 0.2), oma = c(4, 4, 0.2, 0.2))

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
vel1 <- velo1

files = c("17-01-A-F2", "17-01-A-S2", "17-02-B-F2", "17-02-B-S2")
velo1 = matrix(ncol = 12, nrow = length(files), dimnames = list(1:length(files)))
par(mfrow = c(2, 2), mar=c(1.5, 0.5, 2, 0.2), oma = c(4, 4, 0.2, 0.2))

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
vel2 <- velo1

# Cargamos los datos y visualizamos las diez primeras filas
data <- read.table("17-01-A-F", skip = 1, header = TRUE, sep = ',')
head(data, n = 10)

RPM(lum = data$V4, n = 50, m = 5, plot = TRUE)
