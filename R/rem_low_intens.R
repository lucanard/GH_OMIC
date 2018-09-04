rem_low_intens <- function(peaklist2) {
  target <- grep("mzML.1", colnames(peaklist2))
  samples <- peaklist2[,target]
  samples1 <- samples[,-blanks]
  int1 <- apply(samples1, 1, max)
  lim <- min(int1) *10
  peaklist3 <- peaklist2[which(int1 >= lim), ]
  return(peaklist3)
}
