# GH_OMIC
The package aims to simply deconvolute orbitrap like data from the format mzML to a peaklist. It also automatically removes useless data from the peaklist like peaks higher in blanks, peaks with high RSD, peaks higher in QCs, peaks out of the expected RT range.

### to download
just type
```R
library("devtools")
install_github("lucanard/GH_OMIC")
```
