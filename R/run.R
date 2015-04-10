run = function(which=menu(dir(pattern = "^shiny",
  path = file.path(find.package("NNTbiomarker"))
  ))) {

  require(shiny)
  shiny:::runApp(
    file.path(find.package("NNTbiomarker"), which)
  )
}
runElicit = function() run("shinyElicit")
runContraBayes = function() run("shinyContraBayes")
