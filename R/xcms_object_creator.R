xcms_object_creator <- function(datapath, use.IPO, retcorvar, intvalpar, centerQC, NomRapport) {

  if (use.IPO == TRUE) {
   require(IPO)
    print("IPO not present")
 } else {
   ppm=25
   mzabs=0.01
   mzdiff=0.01
   prefilter=c(3,10000)
   snthresh=6
   peakwidth=c(5,35)
   noise=0
   bw=10
   mzwid=0.01
   minfrac=0.75
   minsamp=1
   QCname="QCs"
   autoCVQCs=TRUE
   cvth=0.45
 }
cwp <- CentWaveParam(ppm=ppm, prefilter=prefilter, snthresh=snthresh, peakwidth=peakwidth, mzdiff=mzdiff, integrate=1, noise=noise)
raw_data <- MSnbase::readMSData(cdffiles, mode = "onDisk", msLevel=1)
xset <- xcms::findChromPeaks(raw_data, param = cwp, return.type="xcmsSet", BPPARAM = SnowParam(detectCores()-1))

if (retcorvar == TRUE){
  xsg1<- xcms::group(xset, method="density", bw=30, mzwid=mzwid, minfrac=minfrac, minsamp=minsamp, max=30)
  xsg1ret<- xcms::retcor(xsg1, plottype="deviation")
  xsg1retg2<- xcms::group(xsg1ret, method="density", bw=15, mzwid=mzwid, minfrac=minfrac, minsamp=minsamp, max=30)
  xsg1retg2ret<- xcms::retcor(xsg1retg2, plottype="deviation")
  xset3<- xcms::group(xsg1retg2ret, method="density", bw=5, mzwid=mzwid, minfrac=minfrac, minsamp=minsamp, max=30)
  dev.copy2pdf(file=paste("RetCorrLoess_",NomRapport, format(Sys.time(), format = "%Y%m%d_%Hh%M"),".pdf", sep=""))
}else{
  if (is.null(centerQC)){
    xset2 <- xcms::retcor(xset, method="obiwarp", plottype = c("deviation"), profStep=1)
    object= xset2
    peakmat <- xcms::peaks(object)
    samples <- xcms::sampnames(object)
    N <- length(samples)
    plength <- rep(0, N)
    for(i in 1:N){
      plength[i] <- length(which(peakmat[,"sample"]==i))
    }
    center <- which.max(plength)
    mtext(paste("center sample: \n", samples[center]), cex=0.7)
    dev.off()
    rm(object)
    }else{
    xset2 <- xcms::retcor(xset, method="obiwarp", plottype = c("deviation"), profStep=1, center=centerQC)
    object= xset2
    peakmat <- xcms::peaks(object)
    samples <- xcms::sampnames(object)
    N <- length(samples)
    plength <- rep(0, N)
    for(i in 1:N){
      plength[i] <- length(which(peakmat[,"sample"]==i))
    }
    center <- center
    mtext(paste("center sample: \n", samples[center]), cex=0.7)
    dev.off()
    rm(object)
    }
  xset3 <- xcms::group(xset2, method="density", bw=bw, mzwid=mzwid, minfrac=minfrac, minsamp=minsamp, max=30)
}

xset4 <- xcms::fillPeaks(xset3)
save.image(paste(NomRapport, format(Sys.time(), format = "%Y%m%d_%Hh%M"),".RData",sep=""))
return(xset4)
}

