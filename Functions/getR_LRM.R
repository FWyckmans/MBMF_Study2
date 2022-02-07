getR_LRM <- function(Model, ndigits = 3){
  # extract coefficients
  coefs <- data.frame(coef(summary(Model)))
  
  # use normal distribution to approximate p-value
  coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
  
  #round
  coefs$Estimate <- round(coefs$Estimate, ndigits)
  coefs$Std..Error <- round(coefs$Std..Error, ndigits)
  coefs$t.value <- round(coefs$t.value, ndigits)
  coefs$p.z <- round(coefs$p.z, ndigits)
  
  coefs <- AddDummyCol(coefs, "Sig", "")
  coefs$Sig[coefs$p.z < 0.05] <- "*"
  coefs$Sig[coefs$p.z < 0.01] <- "**"
  coefs$Sig[coefs$p.z < 0.001] <- "***"
  
  # See
  return(coefs)
  print(coefs)
}
