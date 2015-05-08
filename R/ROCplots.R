#' ROCplots
#'
#' Plots for a binary target and a single continuous predictor.
#'
#' @param data Data frame with columns "class" (binary target variable) and "X" (predictor).
#' @param N Sample size
#' @param prev Prevalence
#' @param diffInSd Difference: E(X | group=1) - E(X | group=0),measured in units of S.D (common to the 2 groups).
#' @param whichPlots Which plots to do. Options are c("density", "raw", "ROC", "pv", "nnt")
#' @param NNTlower Subjective input. If NNT < NNTlower, the decision is clearly to Treat .
#' @param NNTupper Subjective input. If NNT > NNTupper, the decision is clearly to Wait .

ROCplots = function(data, N= 1000, prev=0.2, diffInSD=2,
                    whichPlots=c("density", "raw", "ROC", "pv", "nnt"),
                    NNTlower=3, NNTupper=10) {
  if(missing(data)) {  ## Simulate
    muD=0; muN=diffInSD; sd=1
    class = rbinom(N, 1, prev)
    nD = sum(class)
    nH = N - nD
    X = rnorm(N, c(muD, muN)[1+class], sd)
  }
  else {
    class = data$class
    X = data$class
    nD = sum(class)
    nH = N - nD
  }
  if(is.element(el = "density", set=whichPlots)) {
    plot(density(X))
    rug(X)
  }
  if(is.element(el = "raw", set=whichPlots))
    plot(X, class)
  data = data.frame(class, X) [order(X), ]
  sens = (nD - cumsum(data$class == 1))/ nD  # 1 - FN/nD = (TP/nD)
  spec = cumsum(data$class == 0)/nH       # TN/nH
  if(is.element(el = "ROC", set=whichPlots))
    plot(1-spec, sens, type="l")
  ### You can't plot ppv versus npv at the -Inf or +Inf cutoffs,
  ### unlike ROCs. You need at least one Pos one Neg.
  nPos = (N-1):1
  nNeg = 1:(N-1)
  ppv = (nD - cumsum(data$class == 1))[-N]/ nPos  #  TP/Pos
  npv = cumsum(data$class == 0)[-N]/ nNeg  ## TN/Neg
  if(is.element(el = "pv", set=whichPlots)) {
    if(N <= 10)
      plot(ppv, npv, type="b", pch=as.character(1:N))
    else
      plot(ppv, npv, type="l")
  }
  ## plotting NNTpos vs NNTneg is even more limited, because, well, infinity.
  NNTpos = 1/ppv
  NNTneg = 1/(1-npv)
  if(is.element(el = "nnt", set=whichPlots)) {
    if(N <= 10)
      plot(NNTpos, NNTneg, type="b", pch=as.character(1:N))
    else
      plot(NNTpos, NNTneg, type="l")
    lines(c(par()$usr[1], NNTlower), c(NNTupper,NNTupper), lty=2, col="blue")
    lines(c(NNTlower, NNTlower), c(par()$usr[4],NNTupper), lty=2, col="blue")
  }
  return(invisible(data))
}
theData = ROCplots()
