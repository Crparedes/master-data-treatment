#Calculus
LiMM <- 6.941; NaMM <- 22.989769; KMM <- 39.0983; MgMM <- 24.3050 #g/gmol
LiFd <- 2 #mg/kg
Na_NaCl <- 0.39337; K_KCl <- 0.52445; Mg_MgCO3 <- 0.28827 #g_Mg/g_MgCO3
MolRat <- 10^c(0, 1, 2)
Na <- LiFd / LiMM * MolRat * NaMM; K <- LiFd / LiMM * MolRat * KMM; Mg <- LiFd / LiMM * MolRat * MgMM #mg M/kg_feed

85 * Na / 5000; 85 * K / 10000; 85 * Mg / 10000 # g MetSol in feed
5000 / Na_NaCl / 1000000 * 50 / 0.995; 10000 / K_KCl / 1000000 * 50 / 0.995; 10000 / Mg_MgCO3 / 1000000 * 50 / 0.98 #G_salt/50g
Mg_mol <- Mg / 1000 / MgMM; "mol M/kg_feed"; 14 + log10(sqrt(1.8e-11 / Mg_mol^2)) #pH at which Mg(OH)2 precipitates
1.734485 / 84.3139 * 2 / 6 * 1000 #ml HCl 6N

700 / Mg_MgCO3 / 1000000 * 180 / 0.98

StockLi.200_2 <- 130.3 * 0.187872 * 0.99 / 0.1205105
1000 * 2 / StockLi.200_2


# RealData...
source(file = "19-10-03-Li-Mem17_1.R")
source(file = "19-10-03-LiNaKMg-Mem17_2-4.R")
source(file = "19-10-11-LiNaKMg-Mem17_5_7.R")
source(file = "19-10-11-LiNaKMg-Mem17_8_10.R")
