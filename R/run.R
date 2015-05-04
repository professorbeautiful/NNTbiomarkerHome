#' run
#'
#' Run a shiny app for this package.
#' @param shinyDir Current options are "shinyElicit" and "shinyContraBayes"
#' @details Currently, runElicit is built from the combinePlots branch. It does not walk
#' through the design process. The master branch has a walk-through of the steps,
#' but is not yet supported. When that is activated, the combinePlots version will
#' be moved to a new directory, shinyCombinePlots.

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

#' runElicit
#'
#' @seealso run
runElicit = function() run("shinyElicit")

#' runCombinePlots
#'
#' @seealso run
runCombinePlots = function() run("shinyCombinePlots")


