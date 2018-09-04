##Y. Guitton for Laberca
##2015
##renameMxTy version 1.0

##use when you have a matrix from xcms/CAMERA with no more MxTy information but still mz (or mzmed) and rt (or rtmed) information
#file your tsv file (or csv juste change sep=",")
#rt name of the rt containing column
#mz name of the mz containing column
#sep field separator in file ("\t" or "," or ";", or " ")
#mzdec number of digit to keep in mz part feature name M353T566  or M358.23T566
#rtdec, number of digit for RT part in feature name

renameMxTy<-function(file,rt="rt", mz="mz", sep="\t", mzdec=0, rtdec=0){
mat<-read.table(file, h=T,sep=sep, stringsAsFactor=FALSE)

	mzfmt <- paste("%.", mzdec, "f", sep = "")
    rtfmt <- paste("%.", rtdec, "f", sep = "")

    gnames <- paste("M", sprintf(mzfmt, mat[,mz]), "T",
                    sprintf(rtfmt, mat[,rt]), sep = "")
					
   if (any(dup <- duplicated(gnames)))
        for (dupname in unique(gnames[dup])) {
            dupidx <- which(gnames == dupname)
            gnames[dupidx] <- paste(gnames[dupidx], seq(along = dupidx), sep = "_")
        }

return(gnames)

}