extractantes <- function(TotalMass = NULL, MolarRatio = NULL, LIX = NULL, Cyanex = NULL) {
  if (missing(TotalMass) && missing(MolarRatio)) {
    report <- cbind(TotalMass = LIX + Cyanex, 
                    MolarRatio <- (LIX / 246) / (Cyanex / 348))
  } else {
    LIX_MW <- 246; Cya_MW <- 348
    MassRatio <- LIX_MW * MolarRatio / Cya_MW
    report    <- cbind(Lix54 = (TotalMass * MassRatio) / (1 + MassRatio), 
                       Cyanex = TotalMass / (1 + MassRatio))
  }
  return(report)
}

GodnessOfFit <- function(Transport, ModelStrip, ModelFeed) {
  Time <- unique(Transport$Tiempo)
  X_st_pred <- (coefficients(ModelStrip)[1] * Time**2) / (1/coefficients(ModelStrip)[2] + Time**2)
  X_fe_pred <- 1 - ((coefficients(ModelFeed)[1] * Time**2) / (1/coefficients(ModelFeed)[2] + Time**2))
  X_st_exp  <- Transport$Conc[which(Transport$Fase == "Recuperación")]
  X_fe_exp  <- Transport$Conc[which(Transport$Fase == "Alimentación")]
  
  XiSq_Feed <- sum((X_fe_pred - X_fe_exp) ^ 2 / X_fe_exp)
  XiSq_Strp <- sum((X_st_pred - X_st_exp) ^ 2 / X_st_exp)
  return(c(XiSq_Feed, XiSq_Strp))
}


