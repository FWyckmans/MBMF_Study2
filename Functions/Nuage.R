Nuage <- function(d, NameSave, VIname, VDname, VI, VD,
                  VImin = 0, VImax = 0,
                  VDmin = 0, VDmax = 0,
                  EqX = 0, EqY = 0) {
  
  if (VDmin == 0){
    VDmin = min(VD, na.rm = T)}
  if (VDmax == 0){
    VDmax = max(VD, na.rm = T)
  }
  
  if (VImin == 0){
    VImin = min(VI, na.rm = T)}
  if (VImax == 0){
    VImax = max(VI, na.rm = T)
  }
  
  if (EqX == 0){
    EqX = VImin + 2}
  if (EqY == 0){
    EqY = VDmax - 2
  }
  
  model <- lm(VD ~ VI)
  summary(model)
  b0 = round(model$coefficients[[1]],2)
  b1 = round(model$coefficients[[2]],2)
  Eq = paste0("Y = ", b0, " + ", b1, "X")
  
  Plot <- ggplot(d, aes(x = VI, y = VD)) +
    geom_point() +
    geom_smooth(method=lm, se=F) +
    theme_classic() +
    xlab(VIname) +
    ylab(VDname) +
    xlim(VImin, VImax) +
    ylim(VDmin, VDmax) +
    
    geom_text(x = EqX, y = EqY, label = Eq)
  ggsave(NameSave, dpi=300)
  Plot
}