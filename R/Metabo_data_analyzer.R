#' @title Metabo_data_analyzer
#' @description The function transforms you raw mzML data files in a deconvoluted peaklist eliminating also the the redundant informations
#' @param datapath the path source where is the data to analyze
#' @param pathresult the path where to store the analyzed data
#' @param author the name of project author. Default to Luca
#' @param use_IPO either use IPO or not to determine the best wcms parameters for peak picking. (Not available yet)
#' @param ret_corvar chose the method to align the samples between "density" and "obiwarp"
#' @param intvalpar what kind of peak measurement should the algorithm use? default to IPO
#' @param center should the obiwarp method centered to a specific QC sample
#' @param normalization should the sample batch normalized?
#' @param NomRapport what is the name of Rapport? default to a name obtained from the datapath
#' @param grouping_mode what kind of grouping mode should CAMERA use to determine the pseudospectra? chose between EIC_corr or area_corr
#' @param no_conditioning_QC should be conditioning QC erased from the normalization process? Default set to yes
#' @param list_conQC list of conditioning QC. If null, the algorithm will try to determine what are the conditioning QCs
#' @param RT_max What is the limit of RT to consider a peak included in the peaklist?
#' @import xcms
#' @import CAMERA
#' @import stringr
#' @usage Metabo_data_analyzer(datapath, pathresult = NULL, author = "LNA", use_IPO = FALSE, ret_corvar=FALSE, intvalpar="into", center=NULL, normalization=TRUE, NomRapport = NULL, grouping_mode, no_conditioning_QC = "ON", list_conQC= "NULL", RT_max)
#' @author Luca Narduzzi "nardluca@gmail.com"
#' @return peaky
#' @export "Metabo_data_analyzer"
#'
#' @examples
#' peaky <- Metabo_data_analyzer(datapath = datapathy, grouping_mode = "area_corr", RT_max = 17)
#'
Metabo_data_analyzer <- function(datapath, pathresult = NULL, author = "LNA", use_IPO = FALSE, ret_corvar=FALSE,
                                 intvalpar="into", center=NULL, normalization=TRUE, NomRapport = NULL, grouping_mode,
                                 no_conditioning_QC = "ON", list_conQC= "NULL", RT_max) {
  require(stringr)
  require(xcms)
  if (stringr::str_detect(tolower(datapath), "positive") == T) {
    polarity = "positive"
  }
  if (stringr::str_detect(tolower(datapath), "negative") == T) {
    polarity = "negative"
  }  else {
    polarity <- readline("What is the polarity of the experiment?")
  }
  polarity <- gsub("[^[:alnum:][:space:]]","", polarity)
  if (exists(grouping_mode) == F) {
  grouping_mode <- readline("What is the grouping_mode? Choose between EIC_corr and area_corr
                            ")
  } else {
    grouping_mode = grouping_mode
  }
  if (is.null(author)) {author <-  substr(datapath, 4,6)} else {author = author}
  cdffiles <- loadcdf(datapath)
  if (is.null(pathresult)) {
    setwd(datapath)
    pathresult <- datapath
  } else {setwd(pathresult)}
  if (is.null(NomRapport)) {NomRapport = substr(cdffiles[1], 4, 20) } else {NomRapport = NomRapport}
  NomRapport <- stringr::str_replace_all(NomRapport, "/", "")
  NomRapport <- stringr::str_replace_all(NomRapport, " ", "")
  QCs <- which(stringr::str_detect(cdffiles, "QC") == TRUE)
  Control <- which(stringr::str_detect(tolower(cdffiles), "control"))
  Treated <- which(stringr::str_detect(tolower(cdffiles), "treated"))
  blanks <- which(stringr::str_detect(tolower(cdffiles), "blanks"))
  injectorder <- stringr::str_order(substr(cdffiles, (nchar(cdffiles)-7), (nchar(cdffiles)-5)))

xset4 <- xcms_object_creator(datapath = datapath, use.IPO = use_IPO, retcorvar = ret_corvar, intvalpar = intvalpar,
                             center = center, NomRapport = NomRapport)

peaklist <- CAMERA_object_creator(xset4, grouping_mode = grouping_mode, NomRapport = NomRapport)
peaky <- peaklist_cleaning(peaklist, blanks, QCs, no_conditioning_QC = no_conditioning_QC,
                           list_conQC = list_conQC, RT_max)
return(peaky)
}
