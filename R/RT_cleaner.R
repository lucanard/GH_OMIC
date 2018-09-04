RT_cleaner <- function(peaklist3, RTmax) {
  if (is.null(RTmax)) {
    peaklist4 <- peaklist3
    } else {
  RT <- peaklist3$rt/60
  peaklist4 <- peaklist3[which(RT <= RTmax), ]
  return(peaklist4)
    }
}
