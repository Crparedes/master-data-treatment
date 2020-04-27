plotALL <- function() {
  plotSimplex3D(simplex, sel.dim = 2:4)
  plotSimplex3D(simplex, sel.dim = 1:3)
  plotSimplex3D(simplex, sel.dim = c(1, 2, 4))
}
plotALL2d <- function() {
  plot(simplex, sel.dim = 1:2)
  plot(simplex, sel.dim = c(1, 3))
  plot(simplex, sel.dim = c(1, 4))
  plot(simplex, sel.dim = c(2, 3))
  plot(simplex, sel.dim = c(2, 4))
  plot(simplex, sel.dim = c(3, 4))
}
library(transmem)
library(labsimplex)

#pdf("simplex-09-19.pdf", height = 7/1.8, width = 9/1.8)
(simplex <- labsimplex(N = 4, var.name = c('Extr', 'MolRat', 'NH4OH', 'HCl'),
                      start = c(70, 2, 0.01, 0.07), stepsize = c(20, 1, 0.01, -0.06)))
smplx <- simplex
# extrMolRat(mass = simplex$coords[, 1], ratio = simplex$coords[, 2])

adjustVertex(simplex = simplex, newcoords = list(Vertex.5=c(NA, 2.75, NA, 0.04)), overwrite = TRUE)
plotALL()


source(file = "19-09-Simplex-2/19-09-10-LiNa-Mem16CLEAN.R")
Mresp <- rbind(colMeans(Parameters[1:2, ]), Parameters[3, ], Parameters[4, ],
               colMeans(Parameters[5:6, ]), colMeans(Parameters[7:8, ]))
generateVertex(simplex = simplex, qflv = Mresp[, 3], crit = "max", algor = "variable", overwrite = TRUE)
generateVertex(simplex = smplx, qflv = (Mresp[, 1] * Mresp[, 3]), crit = "max", algor = "variable", overwrite = TRUE)
#extrMolRat(mass = simplex$coords[nrow(simplex$coords), 1], ratio = simplex$coords[nrow(simplex$coords), 2])
plotALL()

#generateVertex(simplex = simplex, qflv = 0.3, crit = "max", algor = "variable", overwrite = FALSE)
#extrMolRat(mass = 65, ratio = 3.43)

generateVertex(simplex = simplex, qflv = 0, crit = "max", algor = "variable", overwrite = TRUE)
#extrMolRat(mass = simplex$coords[nrow(simplex$coords), 1], ratio = simplex$coords[nrow(simplex$coords), 2])
plotALL()

#source(file = "19-09-20-LiNa-Mem16_6.R")

source(file = "19-09-Simplex-2/19-09-20-LiNa-Mem16_7.R")
Resp <- colMeans(Parameters)[3]
generateVertex(simplex = simplex, qflv = Resp, crit = "max", algor = "variable", overwrite = TRUE)
#extrMolRat(mass = simplex$coords[nrow(simplex$coords), 1], ratio = simplex$coords[nrow(simplex$coords), 2])
plotALL()

source(file = "19-09-Simplex-2/19-09-22-LiNa-Mem16_8.R")
Resp <- colMeans(Parameters)[3]
generateVertex(simplex = simplex, qflv = Resp, crit = "max", algor = "variable", overwrite = TRUE)
#extrMolRat(mass = simplex$coords[nrow(simplex$coords), 1], ratio = simplex$coords[nrow(simplex$coords), 2])
plotALL()

source(file = "19-09-Simplex-2/19-09-23-LiNa-Mem16_9.R")
Resp <- colMeans(Parameters)[3]
generateVertex(simplex = simplex, qflv = Resp, crit = "max", algor = "variable", overwrite = TRUE)
#extrMolRat(mass = simplex$coords[nrow(simplex$coords), 1], ratio = simplex$coords[nrow(simplex$coords), 2])
plotALL()

source(file = "19-09-Simplex-2/19-09-24-LiNa-Mem16_10.R")
Resp <- Parameters[1, 3]
generateVertex(simplex = simplex, qflv = Resp, crit = "max", algor = "variable", overwrite = TRUE)
#extrMolRat(mass = simplex$coords[nrow(simplex$coords), 1], ratio = simplex$coords[nrow(simplex$coords), 2])
plotALL()

source(file = "19-09-Simplex-2/19-09-25-LiNa-Mem16_11.R")
Resp <- colMeans(Parameters)[3]
generateVertex(simplex = simplex, qflv = Resp, crit = "max", algor = "variable", overwrite = TRUE)
#extrMolRat(mass = simplex$coords[nrow(simplex$coords), 1], ratio = simplex$coords[nrow(simplex$coords), 2])
plotALL()

source(file = "19-09-26-LiNa-Mem16_12.R")
Resp <- colMeans(Parameters)[3]
generateVertex(simplex = simplex, qflv = Resp, crit = "max", algor = "variable", overwrite = TRUE)
#extrMolRat(mass = simplex$coords[nrow(simplex$coords), 1], ratio = simplex$coords[nrow(simplex$coords), 2])
plotALL()
plotALL2d()

plotSimplexResponse(simplex, pch = 2)

simplex$qual.fun
VertexNumber <- as.numeric(gsub("Vertex.", "", dimnames(simplex$coords)[[1]]))
VertexNumber <- VertexNumber[-length(VertexNumber)]
p <- ggplot(data = data.frame(Response = simplex$qual.fun, Vertex = VertexNumber), aes(x = Vertex, y = Response)) +
  geom_point(size = 2.5)+#, shape = c(17, 17, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16)) +
  geom_line() + theme_bw() + scale_x_continuous(breaks = 1:12, limits = c(1, 12)) +
  scale_y_continuous(breaks = c(0, 0.1, .2, .3, .4, .5, .6), limits = c(0, 0.6)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")) +
  theme(text = element_text(size = 9))

pdf("Response.pdf", height = 70/25.4, width = 90/25.4)
p
dev.off()
