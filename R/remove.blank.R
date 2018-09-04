remove.blank <- function(peaklist1, blank = blanks) {
  target <- grep("mzML.1", colnames(peaklist1))
  samples <- peaklist1[,target]
  med.blank <- apply(samples[,blank], 1, median)
  med.samples <- apply(samples[,-blank], 1, median)
  clean_peaklist <- peaklist1[-which(med.samples <= med.blank),]
  return(clean_peaklist)
}
