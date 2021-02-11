AddDummyCol <- function(d, ToAdd){
  ##### Give the df you want to add dummy columns to and the column names #####
  
  for (i in ToAdd) {
    Vect <- rep(NA, length(d[[1]]))
    d <- cbind(d, Vect)
    names(d)[names(d) == "Vect"] <- ToAdd[i]
  }
  return(d)
}