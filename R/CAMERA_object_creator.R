CAMERA_object_creator <- function(xset4, grouping_mode = c("EIC_corr", "area_corr"), NomRapport) {

no_cores <- detectCores() - 1
ppmcamera=5
mzabs=0.01

n <- length(unlist(xset4@phenoData))
xs<- CAMERA::xsAnnotate(xset4, nSlaves = no_cores, polarity = polarity,  sample = c(1:n))
xs<- CAMERA::groupFWHM(xs, perfwhm=0.7)
xsI <- CAMERA::findIsotopes(xs, ppm=ppmcamera*2, mzabs=mzabs*2)
if (polarity=="positive"){
  casmirules <- read.csv(system.file("extdata", "CASMI_extended_POS_rules.csv", package = "GH.OMIC"), sep = ",", header = T, stringsAsFactors = F)
  }else{
  casmirules <- read.csv(system.file("extdata", "CASMI_extended_NEG_rules.csv", package = "GH.OMIC"), sep = ",", header = T, stringsAsFactors = F)
  }
if (grouping_mode == "area_corr") {
xsI<- CAMERA::groupCorr(xsI, calcIso=TRUE, calcCaS=TRUE, cor_exp_th= 0.7, pval=0.1, intval = "into")
}
if (grouping_mode == "EIC_corr") {
  xsI <- CAMERA::groupCorr(xsI, calcIso = T, calcis = T, cor_eic_th = 0.7, pval = 0.1, intval = "into")
}
xsA <- CAMERA::findAdducts(xsI, ppm=ppmcamera, mzabs=mzabs, rules=casmirules, polarity=polarity)
peaklist <- CAMERA:: getPeaklist(xsA)
write.table(peaklist, sep="\t", row.names=F, file=paste(NomRapport,"_","CAMERA_peaklist",".tsv"))
return(peaklist)
}
