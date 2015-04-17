# shinyapps::setAccountInfo(name='trials', token='0B4BEE2FB18CEA0BCFD76843D64453A5', secret='z30BF4mYXM3llWXFsbjF7I826F8DtVqaXUN3UyjY')

deploy = function(app="shinyElicit"){
  devtools::install_github("professorbeautiful/NNTbiomarkerHome")
  setwd("~/Dropbox/NNTbiomarkerHome/inst/" %&% app)
  require("shinyapps")
  deployApp()
  setwd("../..")
}

runDeployed = function(app="shinyElicit"){
  system("open https://trials.shinyapps.io/" %&% app)
}

