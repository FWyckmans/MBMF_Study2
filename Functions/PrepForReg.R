PrepForReg <- function(OutliersReplacement = NA){
  ##### Factorize Sample
  d$SampleC <- factor(d$SampleC, c(-1,1), labels = c("PG", "HC"))
  
  # Frame without Outliers in dCortiM or w
  if (is.na(OutliersReplacement)){
    dNoO <- OutliersModif(d, c("dCortiM", "w"), Proxy = "MAD", mult = 3, as = NA)
    dNoO <- dNoO%>%
      filter(!is.na(dCortiM))%>%
      filter(!is.na(w))
  } else {
    if (OutliersReplacement == "MinMax"){
      dNoO <- OutliersModif(d, c("w", "dCortiM"), Proxy = "MAD", mult = 3, as = "MinMax")
      dNoO <- dNoO%>%
        filter(!is.na(dCortiM))
    }
  }
  
  ScaleToDo <- list(CoI = c("MFsw", "MBsw", "MBURsw",
                            "w", "beta1", "beta2", "a1", "a2", "pi", "lambda",
                            "MBv", "MFv",
                            "RewRT1", "UnRewRT1", "CommonRT2", "RareRT2", "dRT1", "dRT2",
                            "OSPAN", "Raven","dCortiM", "dCorti",
                            "SRRS", "STAIA", "Beck", "SCL90R", "RewardSens", "DSM", "SOGS"),
                    NewCol = c("zMFs", "zMBs", "zMBURs",
                               "zw", "zbeta1", "zbeta2", "za1", "za2", "zpi", "zlambda",
                               "zMBv", "zMFv",
                               "zRewRT1", "zUnRewRT1", "zCommonRT2", "zRareRT2", "zdRT1", "zdRT2",
                               "zOSPAN", "zRaven", "zdCortiM", "zdCorti",
                               "zSRRS", "zSTAIA", "zBeck", "zSCL90R", "zRewardSens", "zDSM", "zSOGS"))
  d <<- ScaleCol(d, ScaleToDo)
  dNoO <<- ScaleCol(dNoO, ScaleToDo)
}