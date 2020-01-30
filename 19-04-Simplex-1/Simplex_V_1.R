source("functions.R")
library(labsimplex)

memParameters <- c("CTA", "Total.Extr", "Molar.Ratio", "NaOH.Feed", "HCl.Strip")
# Molar ratio refers to LIX-54-100/Cyanex 923
initialPoint  <- c(30, 100, 2, 0.01, 0.1)
stepSize_0    <- c(-10, -20, 1, 0.005, 0.05)

PIM <- labsimplex(N = 5, var.name = memParameters, start = initialPoint, stepsize = stepSize_0)

plotSimplex3D(PIM)
plot(PIM, sel.dim = c("NaOH.Feed", "HCl.Strip"))

#extractantes(TotalMass = c(100, 84.1, rep(104.1, 4)), MolarRatio = c(2, 2, 2.75, 1.75, 1.75, 1.75))
#extractantes(LIX = c(58.8, 50.3, 69.1, 58.3, 56.9, 59.4), Cyanex = c(42.3, 34.6, 36.0, 46.6, 46.9, 47.5))
adjustVertex(PIM, newcoords = list(vertex1 = c(29.6, 101.1, 1.97, NA, NA), vertex2 = c(41.1, 84.9, 2.06, NA, NA),
                                   vertex3 = c(40.9, 105.1, 2.72, NA, NA), vertex4 = c(41.5, 104.9, 1.77, NA, NA),
                                   vertex5 = c(41.6, 103.8, 1.72, NA, NA), vertex6 = c(38.5, 106.9, 1.77, NA, NA)),
             overwrite = TRUE)

source("Vertices1-6.R")
generateVertex(PIM, qflv = QF[, 9], overwrite = TRUE)
plotSimplex3D(PIM)
plot(PIM, sel.dim = c("NaOH.Feed", "HCl.Strip"))

#extractantes(TotalMass = 97.4, MolarRatio = 2.40)
#extractantes(LIX = c(61.1, 69.0), Cyanex = c(31.8, 35.8))
adjustVertex(PIM, newcoords = list(vertex7 = c(34.9, 92.9, 2.72, NA, NA)), overwrite = TRUE)


#pdf("Trans-V_7-V_9.pdf", height = 5, width = 10)
source("Vertices7_3rep.R")
generateVertex(PIM, qflv = QF[1, 9], algor = 'variable', overwrite = TRUE)
plotSimplex3D(PIM)
plot(PIM, sel.dim = c("NaOH.Feed", "HCl.Strip"))

#extractantes(TotalMass = 94.14, MolarRatio = 2.734)
#extractantes(LIX = c(61.7, 57.4), Cyanex = c(32.4, 46.6))

source("Vertices8_6rep.R")
generateVertex(PIM, qflv = 0, algor = 'variable', overwrite = TRUE)
plotSimplex3D(PIM)
plot(PIM, sel.dim = c("NaOH.Feed", "HCl.Strip"))

#extractantes(TotalMass = 119.46, MolarRatio = 2.32)
#extractantes(LIX = c(74.23, 106.69-33.44), Cyanex = c(45.29, 45.08))

source("Vertices9_9rep.R")
#dev.off()

