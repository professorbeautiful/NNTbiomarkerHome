PaikFitFile = system.file(package="NNTbiomarker",  "Paik-fit.R")
cat("global.R: PaikFitFile is ", PaikFitFile, "\n")
if( ! identical(ls(pattern="Paik_nnt", pos=1), 1) )
  source(PaikFitFile, local=TRUE)

boxcolors = colorRampPalette(c("lightgrey", "red"))(6)
