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
#' A variety of ROC-related plots for a binary target and a single continuous predictor.
#'
#' @param data Data frame with columns "class" (binary target variable) and "X" (predictor).
#' @param whichPlots Which plots to do. Options are c("density", "raw", "ROC", "pv", "nnt")
#' @param NNTlower Subjective input. If NNT < NNTlower, the decision is clearly to Treat.
#' @param NNTupper Subjective input. If NNT > NNTupper, the decision is clearly to Wait.
#' @param N For simulated data: sample size
#' @param prev For simulated data: Prevalence
#' @param diffInSd For simulated data: Difference: E(X | group=1) - E(X | group=0),measured in units of S.D (common to the 2 groups).
#' @details The "whichPlots" options are as follows:
#' \itemize {
#'  \item {"density"}{Marginal density of X, with rug.}
#'  \item {"raw"}{X versus class.}
#'  \item {"ROC"}{Standard ROC curve.}
#'  \item {"pv"}{Plot of ppv versus npv, with indication of the acceptable region}
#'  \item {"nnt"}{Plot of NNTpos versus NNTneg, with indication of the acceptable region}
#'  \item {"nntRange"}{Plot of NNTpos and NNTneg versus cutoff, Plot of ppv versus npv, with indication of the acceptable range.}
#' }
#'
#' By default, all are done.

ROCplots = function(data,
                    whichPlots=c("density", "raw", "ROC", "pv", "nnt", "nntRange"),
                    NNTlower=3, NNTupper=10,
                    N= 1000, prev=0.2, diffInSD=2) {
  print(whichPlots)
  seeThroughGrey = paste0("#404040", "88")
  if(missing(data)) {  ## Simulate
    muD=0; muN=diffInSD; sd=1
    class = rbinom(N, 1, prev)
    X = rnorm(N, c(muD, muN)[1+class], sd)
    weights = rep(1, length(X))  ### Multiplicity
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
    plot(density(X, weights = weights/sum(weights)))
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
  ### unlike ROCs. You need at least one Pos and one Neg.
  # Ranks for cutoffs:
  nPos = (N-1):1
  nNeg = 1:(N-1)
  ## vectors of ppv and npv for all cutoffs.
  ppv = (nD - cumsum(data$class == 1))[-N]/ nPos  #  TP/Pos
  npv = cumsum(data$class == 0)[-N]/ nNeg  ## TN/Neg
  if(is.element(el = "pv", set=whichPlots)) {
    if(N <= 10)
      plot(ppv, npv, type="b", pch=as.character(1:N))
    else
      plot(ppv, npv, type="l")
    ppvMin = 1/NNTlower
    npvMin = 1 - 1/NNTupper
    lines(c(ppvMin, ppvMin), c(npvMin, 1), col="blue")
    lines(c(ppvMin, 1), c(npvMin, npvMin), col="blue")
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
    legend("topleft", legend="acceptable region", box.col="blue", text.col="blue")
    rect(par()$usr[1], par()$usr[3], par()$usr[2], NNTupper,
              col=seeThroughGrey)
    rect(NNTlower, par()$usr[3],par()$usr[2], par()$usr[4],
              col=seeThroughGrey)
    text(x=par()$usr[1], y=NNTupper, col="blue", labels = "NNTupper",
         xpd=NA, pos=2, cex=0.7)
    text(x=NNTlower, y=par()$usr[4], col="blue", labels = "NNTlower",
         xpd=NA, pos=3, cex=0.7)
  }

  if(is.element(el = "nntRange", set=whichPlots)) {
    Xtrunc = data$X[-length(data$X)]   # [-1]
    plot(c(NNTpos, NNTneg), c(Xtrunc, Xtrunc), pch="",
         ylab="cutoff", xlab="NNT", log="x")
    crossovers = c(min(Xtrunc[NNTpos < NNTlower]),
                   max(Xtrunc[NNTupper < NNTneg]))
    NNTneg = pmin(NNTneg, 10^par()$usr[2])
    # abline(v=c(NNTlower, NNTupper))
    lines(x=c(NNTlower, NNTlower), y=c(par()$usr[3], crossovers[1]))
    lines(x=c(NNTupper, NNTupper), y=c(par()$usr[4], crossovers[2]))
    text(x=NNTlower, y=par()$usr[3], "NNTlower", col="blue",
         srt=90, pos=4)
    text(x=NNTupper, y=par()$usr[4], "NNTupper", col="blue",
         srt=90, pos=2)
    # abline(h=crossovers)
    nntPosTooBig = which(Xtrunc < crossovers[1])
    nntNegTooSmall = which(Xtrunc > crossovers[2])
    invalid = c(nntPosTooBig, nntNegTooSmall)
    valid = which(
      (Xtrunc > crossovers[1]
       & Xtrunc < crossovers[2])
    )
    lines(NNTpos, Xtrunc)
    lines(NNTneg, Xtrunc)
    polygon(x=c(NNTpos[valid], rev(NNTneg[valid])),
            y=c(Xtrunc[valid], rev(Xtrunc[valid])),
            col=ifelse(exists("polygonColorNNT"),
                       polygonColorNNT,
                       paste0(rgb(0,0,.5), "22")))
    polygon(x=c(NNTpos[nntPosTooBig], rev(NNTneg[nntPosTooBig])),
            y=c(Xtrunc[nntPosTooBig], rev(Xtrunc[nntPosTooBig])),
            col=paste0(rgb(0.9,0.1,.1), "22"))
    polygon(x=c(NNTpos[nntNegTooSmall], rev(NNTneg[nntNegTooSmall])),
            y=c(Xtrunc[nntNegTooSmall], rev(Xtrunc[nntNegTooSmall])),
            col=paste0(rgb(0.9,0.1,.1), "22"))
    geometricMean = function(x) exp(mean(log(x)))
    text(x=geometricMean(c(NNTpos[valid], NNTneg[valid])),
         y=mean(Xtrunc[valid]),
         labels="acceptable", col="blue", bg="white")
    text(x=geometricMean(c(NNTpos[nntNegTooSmall], NNTneg[nntNegTooSmall])),
         y=mean(Xtrunc[nntNegTooSmall]),
         labels="nntNegTooSmall", col="red")
    text(x=geometricMean(c(NNTpos[nntPosTooBig], NNTneg[nntPosTooBig])),
         y=mean(Xtrunc[nntPosTooBig]),
         labels="nntPosTooBig", col="red")
    #   symbols(NNTupper, mean(crossovers),
    #           rectangles=cbind(10, diff(crossovers)),
    #           inches=F,
    #           bg=seeThroughGrey, add=TRUE)
    #   rectangle(par()$usr[1:2], crossovers,
    #             bg=seeThroughGrey, add=TRUE, log="x")
  }
  return(invisible(data))
}
theData = ROCplots()
