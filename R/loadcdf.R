loadcdf <- function (path = NULL) {
  require(tcltk)
  if (is.null(path)) {
    pathCDF = tclvalue(tkchooseDirectory(initialdir = getwd(), 
                                         title = "Please, select your CDF directory"))
  }
  else {
    pathCDF = path
  }
  filepattern <- c("[Cc][Dd][Ff]", "[Nn][Cc]", "([Mm][Zz])?[Xx][Mm][Ll]", 
                   "[Mm][Zz][Dd][Aa][Tt][Aa]", "[Mm][Zz][Mm][Ll]")
  filepattern <- paste(paste("\\.", filepattern, "$", sep = ""), 
                       collapse = "|")
  cdffiles <- list.files(path = pathCDF, pattern = filepattern, 
                         all.files = FALSE, recursive = TRUE, full.names = TRUE, 
                         ignore.case = FALSE)
  assign("cdffiles", cdffiles, envir = .GlobalEnv)
  assign("pathCDF", pathCDF, envir = .GlobalEnv)
  return(cdffiles)
}
