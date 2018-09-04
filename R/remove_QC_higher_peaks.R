remove_QC_higher_peaks <- function (clean_peaklist, QC = QCss) {
  target <- grep("mzML.1", colnames(clean_peaklist))
  samples <- clean_peaklist[,target]
  max.QC <- apply(samples[,QC], 1, max)
  max.sam <- apply(samples[,-QC], 1, max)
  clean_peaklist1 <- clean_peaklist[which(max.QC <= max.sam),]
  return(clean_peaklist1)
}
