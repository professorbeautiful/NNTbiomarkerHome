# shinyapps::setAccountInfo(name='trials', token='0B4BEE2FB18CEA0BCFD76843D64453A5', secret='z30BF4mYXM3llWXFsbjF7I826F8DtVqaXUN3UyjY')

# NOTE: I had tried to run rstan, and followed directions
# for soft links as in "NOTES on rstan for monte carlo markov chain.rtf"
# That messed up the deploy:
#   *** arch - R
# ERROR: sub-architecture 'R' is not installed
# *** arch - x86_64
# ERROR: sub-architecture 'x86_64' is not installed
# Solution remove the soft links especially the third one.

### NOTE:  do not include "inst" in the argument.

.installFromGithubNNTbiomarker = function()
  devtools::install_github("professorbeautiful/NNTbiomarkerHome", build_vignettes=TRUE)


.deploy = function(app=c("shinyElicit", "shinyCombinePlots"), reInstall=TRUE){
  ## TODO: first check that the html files are created committed and pushed.
  if(reInstall)
    .installFromGithubNNTbiomarker()
  apps = app
  for (app in apps) {
    if(substr(app, 1, 5) == "inst/")
      warning(".deploy: do not include 'inst' in app name.")
    cat("wd is " %&% getwd() %&% "\n")
    cat("wd changing to " %&% "inst/" %&% app %&% "\n")
    setwd("inst/" %&% app)
    tryCatch({
      require("shinyapps")
      deployApp()
    },
    finally={
      cat("shinyapps::showLogs(appDir = 'inst/" %&% app %&% "')\n")
      setwd("../..")}
    )
  }
}

.runDeployed = function(app="shinyElicit"){
  system("open https://trials.shinyapps.io/" %&% app)
  cat("shinyapps::showLogs(appDir = 'inst/" %&% app %&% "')\n")
}

