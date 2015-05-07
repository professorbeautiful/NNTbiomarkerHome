

ROCplots = function(N= 1000, prev=0.2, muD=0, muN=2, sd=1,
                    whichPlots=c("density", "raw", "ROC", "pv", "nnt"),
                    nntLower=3, nntUpper=10) {
  class = rbinom(N, 1, prev)
  nD = sum(class)
  nH = N - nD
  X = rnorm(N, c(muD, muN)[1+class], sd)
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
  ## plotting nntPos vs nntNeg is even more limited, because, well, infinity.
  nntPos = 1/ppv
  nntNeg = 1/(1-npv)
  if(is.element(el = "nnt", set=whichPlots)) {
    if(N <= 10)
      plot(nntPos, nntNeg, type="b", pch=as.character(1:N))
    else
      plot(nntPos, nntNeg, type="l")
    lines(c(par()$usr[1], nntLower), c(nntUpper,nntUpper), lty=2, col="blue")
    lines(c(nntLower, nntLower), c(par()$usr[4],nntUpper), lty=2, col="blue")
  }
  return(invisible(data))
}
theData = ROCplots()
