rectangle = function(x, y, log=c(), ...) {
  symbols(x=mean(x),
          y=mean(y),
          rectangles=cbind(diff(x),
                           diff(y)),
          inches=F,
          ...)
}


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
  seeThroughGrey = "#80808088"
  if(missing(data)) {  ## Simulate
    muD=0; muN=diffInSD; sd=1
    class = rbinom(N, 1, prev)
    X = rnorm(N, c(muD, muN)[1+class], sd)
    weights = rep(1, length(X))
    data = data.frame(class=class, X=X, weights=weights)
  }
  else {
    class = data$class
    X = data$X
    if(!is.null(data$weights))
      weights = data$weights
    else
      weights = rep(1, length(X))
  }
  N = nrow(data)
  nD = sum(class)
  nH = N - nD
  if(is.element(el = "density", set=whichPlots)) {
    plot(density(X, weights = weights))
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
  ppvMin = 1/NNTlower
  npvMin = 1 - 1/NNTupper
  lines(c(ppvMin, ppvMin), c(npvMin, 1), col="blue")
  lines(c(ppvMin, 1), c(npvMin, npvMin), col="blue")
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
    legend("topleft", legend="acceptable region", box.col="blue", text.col="blue")
    rectangle(par()$usr[1:2], c(par()$usr[3], NNTupper),
              bg=seeThroughGrey, add=TRUE)
    rectangle(c(NNTlower, par()$usr[2]), par()$usr[3:4],
              bg=seeThroughGrey, add=TRUE)
    text(x=par()$usr[1], y=NNTupper, col="blue", labels = "NNTupper",
         xpd=NA, pos=2, cex=0.7)
    text(x=NNTlower, y=par()$usr[4], col="blue", labels = "NNTlower",
         xpd=NA, pos=3, cex=0.7)
  }

  Xtrunc = data$X[-length(data$X)]   # [-1]
 plot(c(NNTpos, NNTneg), c(Xtrunc, Xtrunc), pch="",
       ylab="cutoff", xlab="NNT", log="x")
#     geom_point() +
#      geom_line(aes(NNTpos, Xtrunc)) +
#      geom_line(aes(NNTneg, Xtrunc))
#   print(PLOT)
  crossovers = c(min(Xtrunc[NNTpos < NNTlower]),
                 max(Xtrunc[NNTupper < NNTneg]))
  # abline(v=c(NNTlower, NNTupper))
  lines(x=c(NNTlower, NNTlower), y=c(par()$usr[3], crossovers[1]))
  lines(x=c(NNTupper, NNTupper), y=c(par()$usr[4], crossovers[2]))
  text(x=NNTlower, y=par()$usr[3], "NNTlower", col="blue",
       srt=90, pos=4)
  text(x=NNTupper, y=par()$usr[4], "NNTupper", col="blue",
       srt=90, pos=2)
  # abline(h=crossovers)
  invalid = which( !
                     (Xtrunc > crossovers[1]
                      & Xtrunc < crossovers[2])
  )
  valid = which(
                     (Xtrunc > crossovers[1]
                      & Xtrunc < crossovers[2])
  )
  sapply(invalid, function(cutoff)
    lines(c(NNTpos[cutoff], NNTneg[cutoff]),
          c(Xtrunc[cutoff], Xtrunc[cutoff]),
          col="lightgrey")
  )
  lines(NNTpos, Xtrunc)
  lines(NNTneg, Xtrunc)
  geometricMean = function(x) exp(mean(log(x)))
  text(x=geometricMean(c(NNTpos[valid], NNTneg[valid])),
       y=mean(Xtrunc[valid]),
       labels="acceptable", col="blue")
#   symbols(NNTupper, mean(crossovers),
#           rectangles=cbind(10, diff(crossovers)),
#           inches=F,
#           bg=seeThroughGrey, add=TRUE)
#   rectangle(par()$usr[1:2], crossovers,
#             bg=seeThroughGrey, add=TRUE, log="x")

  return(invisible(data))
}
theData = ROCplots()
