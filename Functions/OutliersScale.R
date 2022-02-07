# Dw <- OutliersScale(d, c("dCortiM", "w"), Scale = "center")
# ToCheck <- c("dCortiM", "w")
OutliersScale <- function(d, ToCheck, Proxy = "MAD", Scale = "standardize", OutRem = T){
  if (OutRem == T){
    for (i in ToCheck) {
      d <- OutliersModif(d, i, Proxy = Proxy)
      d <- d[complete.cases(d[,i]),]
    }
  }
  # d <- d[complete.cases(d[,ToCheck]),]
  ScaleToDo <- list(CoI = c("w", "dCortiM"),
                    NewCol = c("zw", "zdCortiM"))
  
  if (!is.na(Scale)){
    CoI = c(ToCheck)
    NewCol <- c()
    c = 1
    
    for (i in ToCheck) {
      NewCol[c] = paste0("z", i)
      c = c + 1
    }
  
    ScaleToDo <- list(CoI = c(CoI), NewCol = c(NewCol))
    
    d <- ScaleCol(d, ScaleToDo, method = Scale)
  }
    return(d)
}
