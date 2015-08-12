.onAttach = function(libname, pkgname) {
  desc <- packageDescription(pkgname)
  cat("This is ", pkgname, " Version ", desc$Version , " ", desc$Date, "\n")
  return(invisible(NULL))
}
