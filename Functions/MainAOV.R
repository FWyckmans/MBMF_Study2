MainAOV <- function(d, VoI, Groups, RemoveOutTechnique = 'MAD', as = NA){
  d <- d[complete.cases(d[Groups]),]
  d <- unite(d, "Between", all_of(Groups), remove = F, sep = "_")
  
  if (!is.na(RemoveOutTechnique)){
    if (!RemoveOutTechnique %in% c("MAD", "SD")){
      stop('Non supported outlier detection technique, please use "MAD" or "SD"')
    }
    d <- OutliersModif(d, VoI, Groups = c("Between"), Proxy = RemoveOutTechnique, as = as)}
  
  bp(d, VoI, "Between")
  dDescr <<- DescrFrame(d, Btwn = "Between")
  if (length(Groups) == 1){
    anova <- aov(d[[VoI]] ~ d[["Between"]])
    # print(summary(anova))
  }
  
  if (length(Groups) == 2){
    anova <- aov(d[[VoI]] ~ d[[Groups[1]]]*d[[Groups[2]]])
    # anova$effects
  }
  
  if (length(Groups) == 3){
    anova <- aov(d[[VoI]] ~ d[[Groups[1]]]*d[[Groups[2]]]*d[[Groups[3]]])
    # anova$effects
  }
  
  if (length(Groups) > 3){
    stop("To much between variables, max 3 supported. Please consider to unite some")
  }
  
  summary(anova)
}