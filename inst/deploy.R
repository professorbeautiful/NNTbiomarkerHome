# shinyapps::setAccountInfo(name='trials', token='0B4BEE2FB18CEA0BCFD76843D64453A5', secret='z30BF4mYXM3llWXFsbjF7I826F8DtVqaXUN3UyjY')

# NOTE: I had tried to run rstan, and followed directions
# for soft links as in "NOTES on rstan for monte carlo markov chain.rtf"
# That messed up the deploy:
#   *** arch - R
# ERROR: sub-architecture 'R' is not installed
# *** arch - x86_64
# ERROR: sub-architecture 'x86_64' is not installed
# Solution remove the soft links especially the third one.


installFromGithubNNTbiomarker = function()
  devtools::install_github("professorbeautiful/NNTbiomarkerHome", build_vignettes=TRUE)


.deploy = function(app="shinyElicit", reInstall=TRUE){
  if(reInstall)
    installFromGithubNNTbiomarker()
  setwd(paste0("inst/", app))
  tryCatch({
    require("shinyapps")
    deployApp()
  },
  finally={setwd("../..")}
  )
}

runDeployed = function(app="shinyElicit"){
  system("open https://trials.shinyapps.io/" %&% app)
}

