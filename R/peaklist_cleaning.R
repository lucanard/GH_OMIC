peaklist_cleaning <- function(peaklist, blanks, QCs, no_conditioning_QC, list_conQC, RT_max) {
  if (no_conditioning_QC == "ON") {
    if (!is.null(list_conQC)) {badQCs <- list_conQC} else {badQCs <- QCs[1:5]}
peaklist1 <- rem_conditioning_QC(peaklist)
  } else {
    peaklist1 <- peaklist
  }
  QCss <- QCs[-which(badQCs == QCs)] - length(badQCs)
  clean_peaklist <- remove.blank(peaklist1)
  clean_peaklist1 <- remove_QC_higher_peaks(clean_peaklist)
  peaklist2 <- remove_high_cv(clean_peaklist1)
  peaklist3 <- rem_low_intens(peaklist2)
  peaklist4 <- RT_cleaner(peaklist3, RT_max)
  return(peaklist4)
}
