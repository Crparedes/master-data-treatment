# @book{Austria2019, author = {{Federal Ministry Republic of Austria fos Sustainability and Tourism}}, 
# title = {Wold Mining Data Series}, publisher = {Minerals Production}, year = {2012-2019}, 
# isbn = {978-3-901074-46-2}, volume = {27-34}}

# @book{USGS2010-2020, author = {{U. S. Geological Survey}}, year = {2010-2020}, 
# title = {Mineral commodity summaries 2010-2020}}


#  World Production from mineral, in metric tons 
WPM <- data.frame(year = 2009:2017, 
                  Li = c(39134, 55378, 64363, 73446, 59583, 
                         62950, 65377, 82478, 107322) * 0.46457)

#  World Production, in metric tons (Excluding US)
WPMR <- data.frame(year = 2009:2019, 
                   Li = c(28.1, 34.1, 37, 35, 34, 31.7, 35, 38, 69, 95, 77)*1e3)

#  World Compsumtion, in metric tons 
WCMRM <- data.frame(year = 2013:2019, 
                    Li = c(30, 31, 33.3, 36.6, 39.7, 49.1, 57.7)*1e3)

# Simarized table in millons of kilograms
WCaP <- data.frame(year = rep(2009:2019, 3),
                   tag = rep(c('MinProd', 'TotProd', 'Consume'), each = 11),
                   Values = c(c(39.134, 55.378, 64.363, 73.446, 59.583, 
                               62.950, 65.377, 82.478, 107.322, 0, 0) * 0.46457,
                              c(28.1, 34.1, 37, 35, 34, 31.7, 35, 38, 69, 95, 77),
                              c(0, 0, 0, 0, 30, 31, 33.3, 36.6, 39.7, 49.1, 57.7)))

p <- ggplot(WCaP, aes(x = year, y = Values, fill = tag, color = tag)) +
  scale_x_continuous('Año', limits = c(2008.5, 2019.5), breaks = 2009:2019) +
  scale_y_continuous('Porcentaje de uso', limits = c(0, 100), breaks = seq(0, 100, 25)) +
  geom_line() + labs(fill = 'Mercado') +
  scale_fill_brewer(palette = 'Greys', direction = -1) + theme_bw() + theme(legend.position = 'none')
#Baterias, cerámicos y vidrio, lubricantes, otros, síntesis de polímeros, polvos fundentes, tratamiento de aire
pdf('uses.pdf', width = 7*1.5, height = 3*1.5)
print(p)
dev.off()


# Global end-use markets
GEUM <- data.frame(market = rep(c('Baterias', 'Ceramicos y vidrio', 'Lubricantes', 
                                  'Polímeros', 'Polvos fundentes', 
                                  'Tratamiento de aire', 'Otros'), 9),
                   year = rep(c(2019:2011), each = 7),
                   percentage = c(65, 18, 5, 3, 3, 1, 5,
                                  56, 23, 6, 4, 3, 2, 6,
                                  46, 27, 7, 5, 4, 2, 9,
                                  39, 30, 8, 5, 5, 3, 10,
                                  35, 32, 9, 4, 5, 5, 10,
                                  31, 35, 8, 5, 6, 5, 10,
                                  29, 35, 9, 5, 6, 5, 11,
                                  22, 30, 11, 3, 4, 4, 25,
                                  27, 29, 12, 3, 5, 4, 20))

p <- ggplot(GEUM, aes(x = year, y = percentage, fill = market)) +
  scale_x_continuous('Año', limits = c(2010.5, 2019.5), breaks = 2011:2019) +
  scale_y_continuous('Porcentaje de uso', limits = c(0, 65), breaks = seq(0, 60, 20)) +
  geom_bar(colour="black", position = "dodge", stat = "identity") + labs(fill = 'Mercado') +
  scale_fill_brewer(palette = 'Greys', direction = -1) + theme_bw() + theme(legend.position = 'none')
#Baterias, cerámicos y vidrio, lubricantes, otros, síntesis de polímeros, polvos fundentes, tratamiento de aire
pdf('uses.pdf', width = 6*1.5, height = 3*1.5)
print(p)
dev.off()


# World reserves by country 2019, in metric tons:
WReseBC <- data.frame(country = c('Chile', 'Australia', 'Argentina', 'China', 'Estados Unidos',
                                  'Canadá', 'Zimbabue', 'Brasil', 'Portugal'),
                      reserve = c(8.6, 2.8, 1.7, 1, 0.63, 0.37, 0.26, 0.095, 0.060))

WReseBC <- data.frame(Lat = c(-23, -22, -32, 34.5, 39, 
                              70, -16.5, -6.5, 41),
                      Lon = c(-67, 144, -61, 114, -96,
                              -110, 37, -51, -3.1),
                      country = c('Chile', 'Australia', 'Argentina', 'China', 'Estados Unidos',
                                  'Canadá', 'Zimbabue', 'Brasil', 'Portugal'),
                      reserve = c(8.6, 2.8, 1.7, 1, 0.63, 0.37, 0.26, 0.095, 0.060))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(png, ggplot2, grid)
image <- png::readPNG("mundi.png")

p <- ggplot(WReseBC, aes(x = Lon, y = Lat, size = reserve)) +
  annotation_custom(rasterGrob(image, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  scale_x_continuous('', limits = c(-120, 156), breaks = NULL) +
  scale_y_continuous('', limits = c(-45, 79), breaks = NULL) +
  geom_point() + labs(size = paste0('Reservas de litio\n(', expression(10^9), ' kg)')) +
  scale_size_continuous(range = c(1, 10), breaks = c(0.1, 0.25, 2, 5, 8))

pdf('reserves.pdf', width = 6*1.5, height = 3*1.5)
print(p)
dev.off()


# World resources by country 2019, in 10^6 metric tons:
WResoBC <- data.frame(Lat = c(-12.5, -32, -23, 39, -22, 34.5,
                              0, 55.5, 70, 22, 55, 14, 74,
                              47, -16.5, -6.5, 41.5, 41, -8.5, 51,
                              74, 51),
                      Lon = c(-62, -61, -67, -96, 144, 114,
                              23, 17, -110, -101, 23, 4, 105,
                              27, 37, -51, 3.5, -3.1, -73, 21,
                              33, 74),
                      country = c('Bolivia', 'Argentina', 'Chile', 'Estados Unidos', 'Australia', 'China', 
                                  'Congo', 'Alemania', 'Canadá', 'México', 'Rep. Checa', 'Malí', 'Rusia',
                                  'Serbia', 'Zimbabue', 'Brasil', 'España', 'Portugal', 'Perú', 'Austria', 
                                  'Finlandia', 'Kazajstán'),
                      resource = c(21, 17, 9, 6.8, 6.3, 4.5, 
                                   3, 2.5, 1.7, 1.7, 1.3, 1, 1, 
                                   1, 0.54, 0.4, 0.3, 0.25, 0.13, 0.05, 
                                   0.05, 0.05))

p <- ggplot(WResoBC, aes(x = Lon, y = Lat, size = resource)) +
       annotation_custom(rasterGrob(image, 
                                    width = unit(1,"npc"), 
                                    height = unit(1,"npc")), 
                         -Inf, Inf, -Inf, Inf) +
       scale_x_continuous('', limits = c(-120, 156), breaks = NULL) +
       scale_y_continuous('', limits = c(-45, 79), breaks = NULL) +
       geom_point() + labs(size = paste0('Recursos de litio\n(', expression(10^9), ' kg)')) +
       scale_size_continuous(range = c(2, 12), breaks = c(0.05, 0.25, 1, 5, 10, 20))

pdf('resources.pdf', width = 6*1.5, height = 3*1.5)
print(p)
dev.off()

    
# @book{USGS2010-2020, author = {{U. S. Geological Survey}}, year = {2010-2020}, 
# title = {Mineral commodity summaries 2010-2020}})
# @book{Austria2019, author = {{Federal Ministry Republic of Austria fos Sustainability and Tourism}}, 
# title = {Wold Mining Data Series}, publisher = {Minerals Production}, year = {2012-2019}, 
# isbn = {978-3-901074-46-2}, volume = {27-34}}