if( ! identical(ls(pattern="Paik_nnt", pos=1), 1) )
  source(system.file(package="NNTbiomarker",  "inst/Paik-fit.R"), local=TRUE)

boxcolors = colorRampPalette(c("lightgrey", "red"))(6)
