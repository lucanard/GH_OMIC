remove_high_cv <- function(clean_peaklist1, QC = QCss) {
  target <- grep("mzML.1", colnames(clean_peaklist1))
  samples <- clean_peaklist1[,target]
  cv <- (apply(samples[,QC], 1, sd)/apply(samples[,QC], 1, mean)) * 100
  cv[is.na(cv)] <- 0
peaklist2 <- clean_peaklist1[which(cv <= 50),]
}
