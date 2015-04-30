#' run
#'
#' Run a shiny app for this package. There


run = function(shinyDir) {
  if(missing(shinyDir)) {
     shinyDirs = dir(pattern = "^shiny",
               path = file.path(find.package("NNTbiomarker")))
     shinyDir = shinyDirs[menu(shinyDirs)]
  }
  runApp(
    file.path(find.package("NNTbiomarker"), shinyDir)
  )
}
runElicit = function() run("shinyElicit")
runContraBayes = function() run("shinyContraBayes")
