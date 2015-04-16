deploy = function(app="shinyElicit"){
  devtools::install_github("professorbeautiful/NNTbiomarkerHome")
  setwd("~/Dropbox/NNTbiomarkerHome/inst/" %&% app)
  require("shinyapps")
  deployApp()
  setwd("../..")
}
