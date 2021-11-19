ScaleCol <- function(d, ScaleToDo, method = "standardize"){
  d <- AddDummyCol(d, ScaleToDo[[2]], NA)
  Compt = 1
  for (i in ScaleToDo[["CoI"]]) {
    M = mean(d[[i]], na.rm = T)
    SD = sd(d[[i]], na.rm = T)
    for (p in c(1:length(d[[i]]))) {
      if (method == "standardize"){
        NewVal <- (d[[p, i]]-M)/SD
        }
      if (method == "center"){
        NewVal <- (d[[p, i]]-M)
      }
      d[p, ScaleToDo$NewCol[Compt]] <- NewVal
    }
    Compt = Compt+1
  }
  return(d)
}