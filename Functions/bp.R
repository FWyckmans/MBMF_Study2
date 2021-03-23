bp <- function(d, VoI, Groups = NA, OutliersRem = T){
  ## Additionnal requirment
  # This function requires dplyr to work
  if(!require(dplyr)){install.packages('dplyr')}
  if(!require(tidyr)){install.packages('tidyr')}
  library(dplyr)
  library(tidyr)
  
  # Add dummy group if no group
  if (NA %in% (Groups)){
    d <- AddDummyCol(d, ToAdd = "DummyCol", Val = "Dummy")
    Groups <- "DummyCol"
  }
  
  # Get group index
  if (is.character(Groups)){
    Groups <- FromColNameToIndex(d, Groups)
  }
  
  # Get VoI index
  if (is.character(VoI)){
    VoI <- FromColNameToIndex(d, VoI)
  }
  
  # Remove missing values in group
  d <- d[complete.cases(d[,c(Groups)]),]
  
  # United the between-subject columns into one
  grn = length(Groups)
  if(grn > 1){
    btwnName <- colnames(d[c(Groups)],)
    d <- unite(d, GrCol, all_of(Groups), remove = T, sep = "_")
    Groups <- FromColNameToIndex(d, "GrCol")
    
    # Change CoI indexes to match the unite
    if(VoI > Groups){
      VoI <- VoI - (grn-1)
    }
  }
  
  # Remove outliers if needed
  if (OutliersRem == T){
    d <- OutliersModif(d, VoI, Groups)
  }
  
  
  boxplot(d[[VoI]] ~ d[[Groups]], ylab = colnames(d[VoI]))
}