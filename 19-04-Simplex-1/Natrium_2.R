library(ggplot2)
library('ggformula')
NaStock  <- 0.0861 * 22990 * 0.5136 / 5.1747 * 1.0230 / 10.0315
#-----CURVAS DE CALIBRACIÓN--------------------------------------------------
CurveNa.1 <- c(c(0.0000, 0.0548, 0.1433, 0.3205, 0.6178, 0.9372, 1.4929, 2.0799, 3.0523) * NaStock /
                c(6.0000, 6.1328, 6.0089, 6.1254, 6.1521, 6.1531, 6.1615, 6.1384, 6.1277))
AbsNaCurve.1_2 <- c(0.000, 0.032, 0.090, 0.196, 0.368, 0.543, 0.848, 1.089, 1.424)

#----------------------------------------------------------------------------
#-----MODELOS DE LAS CURVAS--------------------------------------------------
ModelNa.1_4 <- lm(AbsNaCurve.1_2[1:9] ~ CurveNa.1[1:9] + I(CurveNa.1[1:9]^2))

plot(AbsNaCurve.1_2 ~ CurveNa.1, pch = c(rep(1, 8), 2))
curve(ModelNa.1_4$coefficients[1] + ModelNa.1_4$coefficients[2] * x +
        ModelNa.1_4$coefficients[3] * x^2, add = TRUE, col = 4)

plot(ModelNa.1_4, which = 1)

#-----TIEMPOS CONSIDERADOS EN LA TOMA DE ALÍCUOTAS---------------------------
Time.7.Na  <- Time.3b.Na <- c(0, 1, 2.75, 4, 4.5, 5.75, 6.5, 8, 11, 24.33, 25.33)[c(1, 4, 7, 9, 11)]
Time.6b.Na <- c(0, 0.5, 1.5, 2.25, 3.25, 4.5, 6.5, 9.5, 11.75, 24.75, 26.75)[c(1, 4, 7, 9, 11)]
Time.9.Na  <- Time.9b.Na <- c(0, 1, 2, 3.25, 4, 4.5, 6.25, 7.25, 18, 22.25, 24)[c(1, 4, 6, 8, 11)]

#-----FACTOR DE DILUCIÓN DE LAS MUESTRAS-------------------------------------
dil.Feed.8.7  <- c(6.1950/0.0593, 6.0393/0.0592, 6.0999/0.0595, 6.0316/0.0593, 6.0699/0.0591)
dil.Feed.8.3b <- c(6.0373/0.0594, 6.0302/0.0587, 6.0961/0.0584, 6.0751/0.0571, 6.0274/0.0590)
dil.Feed.8.6b <- c(6.1878/0.0598, 6.1682/0.0598, 6.0690/0.0594, 6.1012/0.0591, 6.0982/0.0590)
dil.Feed.8.9  <- c(6.1966/0.0593, 6.3848/0.0588, 6.1669/0.0579, 6.1175/0.0590, 6.1842/0.0586)
dil.Feed.8.9b <- c(6.1634/0.0579, 6.2880/0.0599, 6.3166/0.0601, 6.2289/0.0590, 6.0490/0.0592)

dil.Strip.8.7  <- c(2.0209/0.2192, 2.0240/0.2241, 2.0188/0.2185, 2.0225/0.2222, 2.0204/0.2197)
dil.Strip.8.3b <- c(2.0249/0.2228, 2.0249/0.2228, 2.0221/0.2207, 2.0259/0.2213, 2.0241/0.2212)
dil.Strip.8.6b <- c(2.0205/0.2188, 1.9971/0.2207, 2.0240/0.2207, 2.0231/0.2191, 2.0245/0.2210)
dil.Strip.8.9  <- c(2.0200/0.2216, 2.0190/0.2197, 2.0244/0.2202, 2.0199/0.2212, 1.9600/0.1608)
dil.Strip.8.9b <- c(2.0187/0.2221, 2.0293/0.2228, 2.0242/0.2206, 2.0206/0.2207, 1.9496/0.1454)

#-----ABSORBANCIAS DE LAS ALÍCUOTAS------------------------------------------
abs.Feed.8.7  <- c(0.482, 0.463, 0.437, 0.424, 0.389)
abs.Feed.8.3b <- c(0.400, 0.386, 0.365, 0.358, 0.362)
abs.Feed.8.6b <- c(0.332, 0.322, 0.297, 0.277, 0.276)
abs.Feed.8.9  <- c(0.464, 0.407, 0.411, 0.402, 0.364)
abs.Feed.8.9b <- c(0.442, 0.442, 0.443, 0.434, 0.431)

abs.Strip.8.7  <- c(0.006, 0.502, 0.691, 0.952, 1.198)
abs.Strip.8.3b <- c(0.007, 0.289, 0.350, 0.486, 0.587)
abs.Strip.8.6b <- c(0.006, 0.163, 0.397, 0.616, 0.696)
abs.Strip.8.9  <- c(0.006, 0.312, 0.434, 0.669, 0.800)
abs.Strip.8.9b <- c(0.006, 0.140, 0.142, 0.201, 0.250)

Samples <- c(7, '3b', '6b', 9, '9b')
Phase   <- c('Feed', 'Strip')
#-----CONCENTRACIÓN DE SODIO EN LAS ALÍCUOTAS--------------------------------
library(cmna)
for (j in Phase) {
  for (i in Samples) {
    assign(x = paste0('Na.', j, '.8.', i), value = eval(parse(text = 
      paste0('quadratic2(b2 = ModelNa.1_4$coefficients[[3]], b1 = ModelNa.1_4$coefficients[[2]], ',
                        'b0 = ModelNa.1_4$coefficients[[1]] - abs.', j, '.8.', i, ')[6:10] * dil.', j, '.8.', i))))
  }
}
#-----CONCENTRACIONES A FRACCIONES-------------------------------------------
for (i in Samples) {
  assign(x = paste0('Na.Strip.8.', i), value = eval(parse(text = 
             paste0('Na.Strip.8.', i, '/ Na.Feed.8.', i, '[1]'))))
  assign(x = paste0('Na.Feed.8.', i), value = eval(parse(text = 
             paste0('Na.Feed.8.', i, '/ Na.Feed.8.', i, '[1]'))))
}


#-----LITIO EN LAS MISMAS DISOLUCIONES---------------------------------------
source("Vertices7_3rep.R")
source("Vertices8_6rep.R")
source("Vertices9_9rep.R")

#-----PERFILES DE TRANSPORTE ------------------------------------------------
# pdf("Natrium.pdf", height = 5, width = 10)
for (i in Samples) {
  eval(parse(text = paste0(
    "if (Li.Strip.8.", i, "[length(Li.Strip.8.", i, ")] > 1) {
       Li.Strip.8.", i, " <- Li.Strip.8.", i, " / Li.Strip.8.", i, "[length(Li.Strip.8.", i, ")]
     }")))
  assign(x = paste0("NLS.Strip.", i), value = eval(parse(text = paste0("nls(Li.Strip.8.", i,
     "~ (a * Time.", i, "**2)/(1 / b + (Time.", i, "**2)), start = list(a = 1, b = 0.5))"))))
  
  assign(x = paste0("Trans.Li.", i),
         value = eval(parse(text = paste0("data.frame(Tiempo = c(Time.", i, ", Time.", i, "),
                                          Conc = c(Li.Feed.8.", i, ", Li.Strip.8.", i, "),
                                          Fase = rep(c('Li.Alim.', 'Li.Recup.'), each = length(Time.", i, ")))"))))
  assign(x = paste0("Trans.Na.", i),
         value = eval(parse(text = paste0("data.frame(Tiempo = c(Time.", i, ".Na, Time.", i, ".Na),
                                          Conc = c(Na.Feed.8.", i, ", Na.Strip.8.", i, "),
                                          Fase = rep(c('Na.Alim.', 'Na.Recup.'), each = length(Time.", i, ".Na)))"))))
  
  eval(parse(text = paste0("Plot.", i, " <- ggplot(data = Trans.Li.", i, ") +
    labs(y = 'Fracción transportada', x = 'Tiempo (horas)') +
    stat_function(fun = function(x) (coefficients(NLS.Strip.", i, ")[1] * x**2)
                           / (1/coefficients(NLS.Strip.", i, ")[2] + x**2), color = 'darkgrey') +
    stat_function(fun = function(x) (1 - (coefficients(NLS.Feed.", i, ")[1] * x**2)
                           / (1/coefficients(NLS.Feed.", i, ")[2] + x**2)), color = 'darkgrey') +
    geom_point(data = Trans.Li.", i, ", size = 3, aes(x = Tiempo, y = Conc, group = Fase, color = Fase)) +     
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.05)) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), limits = c(0, 27)) + 
    scale_shape_identity() +
    geom_spline(data = Trans.Na.", i, ", aes(x = Tiempo, y = Conc, group = Fase), color = 'darkgrey') +
    geom_point(data = Trans.Na.", i, ", size = 4, 
               aes(x = Tiempo, y = Conc, group = Fase, shape = 'X', color = Fase))# +
    #ggtitle('Mem.8.", i, "')")))
  
  eval(parse(text = paste0('print(Plot.', i, ')')))
}
# dev.off()
