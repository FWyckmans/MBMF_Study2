NormalitySkewKurtosis <- function(d, VoI, Groups) {
  # Give the dataframe, the index of the Variable of Interest and the index of the group column
  # Indicate if you want a between-subject comparison
  
  if(!require(dplyr)){install.packages('dplyr')}
  library(dplyr)
  if(!require(tidyr)){install.packages('tidyr')}
  library(tidyr)
  
  spssSkewKurtosis=function(x) {
    w=length(x)
    m1=mean(x, na.rm = T)
    m2=sum((x-m1)^2, na.rm = T)
    m3=sum((x-m1)^3, na.rm = T)
    m4=sum((x-m1)^4, na.rm = T)
    s1=sd(x, na.rm = T)
    skew=w*m3/(w-1)/(w-2)/s1^3
    sdskew=sqrt( 6*w*(w-1) / ((w-2)*(w+1)*(w+3)) )
    kurtosis=(w*(w+1)*m4 - 3*m2^2*(w-1)) / ((w-1)*(w-2)*(w-3)*s1^4)
    sdkurtosis=sqrt( 4*(w^2-1) * sdskew^2 / ((w-3)*(w+5)) )
    mat=matrix(c(skew,kurtosis, sdskew,sdkurtosis), 2,
               dimnames=list(c("skew","kurtosis"), c("estimate","se")))
    return(mat)
  }
  
  dT[[Groups]] <- factor(dT[[Groups]], levels = unique(dT[[Groups]]), ordered = F)
  
  Gr = unique(d[[Groups]])
  dT <- data.frame()
  for (g in c(1:length(Gr))){
    dt <- subset(d, d[[Groups]] == Gr[g])
    dTemp <- data.frame()
    n = 1
    for (i in VoI){
      vect <- dt[[i]]
      moy <- mean(vect, na.rm = T)
      ET <- sd(vect, na.rm = T)
      mdn <- median(vect, na.rm = T)
      dTemp[n,1] = colnames(dt[i], do.NULL = TRUE, prefix = "col")
      dTemp[n,2] = Gr[g]
      dTemp[n,3] = round(moy,digits=2)
      dTemp[n,4] = round(ET,digits=2)
      dTemp[n,5] = round(mdn, digits=2)
      dTemp[n,6] = round(spssSkewKurtosis(vect)[2]/spssSkewKurtosis(vect)[4],digits=5)
      dTemp[n,7] = round(spssSkewKurtosis(vect)[1]/spssSkewKurtosis(vect)[3],digits=5)
      dTemp[n,8] = round(spssSkewKurtosis(vect)[2],digits=5)
      dTemp[n,9] = round(spssSkewKurtosis(vect)[1],digits=5)
      dTemp[n,10] <- 'OK'
      dTemp[n,11] <- length(vect)
      if ((dTemp[n,6] < -1.96 | dTemp[n,6] > 1.96) | (dTemp[n,7] < -1.96 | dTemp[n,7] > 1.96)){
        dTemp[n,10] <- "Not_OK"
      }
      n = n+1
    }
    dT <- rbind(dT, dTemp)
    
  }
  
  dDescr <- rename(dT, Variable = V1, Group = V2, Mean = V3, SD = V4, Median = V5,
                   KurtosisBYSD = V6, SkewnessBYSD = V7, Kurtosis = V8, Skewness = V9,
                   Normality = V10, N = V11)
  
  # Evaluate if normality ok in each group
  AllNorm <- rep("OK", length(dDescr$Variable))
  dDescr <- cbind(dDescr, AllNorm)
  dDescr$AllNorm <- as.character(dDescr$AllNorm)
  for (i in unique(dDescr$Variable)) {
    if ("Not_OK" %in% dDescr$Normality[dDescr$Variable == i]){
      dDescr$AllNorm[dDescr$Variable == i] <- "Not_OK"}
  }
  dDescr <- arrange(dDescr, Variable)
  return(dDescr)
}
#   dMean <- dT%>%
#     select(c(1, 2, 3))%>%
#     spread(key = 2, value = 3)
#   dSD <- dT%>%
#     select(c(1, 2, 4))%>%
#     spread(key = 2, value = 3)
#   dN <- dT%>%
#     select(c(1, 2, 11))%>%
#     spread(key = 2, value = 3)
#   
#   dDescr2 <- cbind(dMean, dSD[,c(2:3)], dN[,c(2:3)])
#   colnames(dDescr2) <- c("Variable", paste0("Mean_", colnames(dDescr2)[2]), paste0("Mean_", colnames(dDescr2)[3]),
#                          paste0("SD_", colnames(dDescr2)[4]), paste0("SD_", colnames(dDescr2)[5]),
#                          paste0("N_", colnames(dDescr2)[6]), paste0("N_", colnames(dDescr2)[7]))
#   
############ Until here things are fine then shit hits the fans
#   # Create nice frame
#   dDescr3 <- dDescr2
#   n = 0
#   for (g in c(2:((length(dDescr2)+1)/2))) {
#     NewCol <- rep(0, length(dDescr2$V1))
#     dDescr3 <- cbind(dDescr3, NewCol)
#     for (i in c(1:length(dDescr3$V1))){
#       dDescr3[i, ncol(dDescr3)] <- paste0(dDescr2[i,g], " (", dDescr2[i,g+2], ")")
#     }
#     new_col_names <- paste0(" (n = ", dDescr$N[dDescr$Group == dDescr$Group[g]][1], ")")
#     names(dDescr3)[ncol(dDescr3)] <- paste0(names(dDescr3)[g], new_col_names, sep = "\n")
#     n <- n+1
#   }
#   
#   names(dDescr3)[1] <- "Variable"
#   dDescr3 <- dDescr3[ -c(2:(g+n)) ]
#   
#   if ((Btwn == TRUE) & (length(unique(d[[Groups]] == 2)))){
#     NewCol <- rep(0, length(dDescr3$Variable))
#     dDescr3 <- cbind(dDescr3, NewCol)
#     
#     for (i in c(1:length(VoI))) {
#       # print(i)
#       Gr1 <- filter(d, d[[Groups]] == Gr[1])
#       Gr2 <- filter(d, d[[Groups]] == Gr[2])
#       
#       ColName <- names(d[VoI[i]])
#       
#       if (dDescr$AllNorm[dDescr$Variable==ColName][1] == "OK"){
#         
#         Comp <- t.test(Gr1[grep(paste0("^", ColName, "$"), colnames(Gr1))],
#                        Gr2[grep(paste0("^", ColName, "$"), colnames(Gr2))])
#         t = round(as.numeric(Comp[1]), digits = 2)
#         df = round(as.numeric(Comp[2]), digits = 2)
#         p = round(as.numeric(Comp[3]), digits = 5)
#         dDescr3[i,(length(VoI)+2)] <- paste0('t(', df, ') = ', t, ", p = ", p)
#       }
#       
#       else{
#         voi <- VoI[1]
#         Comp <- wilcox.test(d[[voi]] ~ d[[Groups]])
#         w = round(as.numeric(Comp[1]), digits = 2)
#         p = round(as.numeric(Comp[3]), digits = 4)
#         dDescr3[i,(length(VoI)+2)] <- paste0('W = ', w, ", p = ", p)
#       }
#     }
#     
#     
#     names(dDescr3)[ncol(dDescr3)] <- "Between-subject Comparison"
#   }
#   
#   # Change order from dDescr
#   dDescr <- arrange(dDescr, Group, Variable)
#   
#   Output <- list(dDescr, dDescr3)
#   return(Output)
# }
