rem_conditioning_QC <- function(peaklist, QC = QCs, list_badQC = list_conQC) {
  target <- grep("mzML.1", colnames(peaklist))
  if (!is.null(list_badQC)) {badQC <- list_badQC} else {badQC <- QC[1:5]}
  samples1 <- samples[,-badQC]
  peaklist1 <- cbind(peaklist[,1:(target[1]-1)], samples1, peaklist[,(max(target)+1): ncol(peaklist)])
return(peaklist1)
}
