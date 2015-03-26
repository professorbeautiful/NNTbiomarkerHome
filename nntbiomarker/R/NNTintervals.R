# independent flat priors for SN and SP

#' NNTintervals
#'
#' Produce Bayesian and classical intervals for NNT from
#' observations in a prospective study.
#' Useful for "anticipated results" when designing a study.
#'
#' @param NNTpos Number of true positives.
#' @param NNTneg Number of true negatives.
#'

NNTintervalsProspective = function(
  NNTpos=2,
  NNTneg=30,
  Npositives = 10,
  Nnegatives = 30,
  prev = 0.15
){
  ppv = 1/NNTpos
  npv = 1 - 1/NNTneg

  #  (A)  for prospective study:
  # N = Npositives; tp ~= N * PPV = 5
  tp = round(Npositives*ppv)
  fn = round(Npositives*(1-ppv))
  # N = Nnegatives; tn ~= N * NPV
  tn = round(Nnegatives*npv)
  fp = round(Nnegatives*(1-npv))

  alpha = 0.025
  # predictive intervals
  prior = c(1/2,1/2)  ## Jeffreys
  # prior = c(0,0)  # won't work with fp=1
  ifVerboseCat(ppvPI = qbeta(c(alpha, 1-alpha),
                 tp+prior[1]-1, fn+prior[2]-1))
  ifVerboseCat("  #CI for SN", binom.confint(tp, Npositives))
  nntPosPI <- 1/ppvPI[2:1]

  npvPI = qbeta(c(alpha, 1-alpha),
                 tn+prior[1]-1, fp+prior[2]-1)
  ifVerboseCat("  #CI for SP", binom.confint(tn, Nnegatives))
  NNTnegPI <- 1/(1-npvPI)

  # (B) retrospective study

  spFunctionPos = function(sn, nntvalue, prev.=prev)
    c(sp=1 - sn*(nntvalue-1)*prev./(1-prev.))
  spFunctionNeg = function(sn, nntvalue, prev.=prev)
    c(sp=(1-sn)*(nntvalue-1)*prev./(1-prev.))

  NNT.from.sesp(se=.3, sp=spFunctionPos(.3, 2), prev=prev)[1]
  NNT.from.sesp(se=.3, sp=spFunctionNeg(.3, 2), prev=prev)[2]
  # this confirms that spFunctionPos and Neg are OK.

  ptemp<-(1:99)/100
  plot(spFunctionPos(ptemp, 2), spFunctionNeg(ptemp, 2))
(  pv.from.sesp(se=.8, sp=spFunctionPos(.8, 2), prev=prev))
(  pv.from.NNT(NNTpos = NNTpos, NNTneg = NNTneg, prev=prev))
(sesp = sesp.from.pv(ppv = ppv, npv = npv, prev=prev))
  # 0.8333, 0.8529
  ### note- specificity of "no test" is already 85%!
  (sesp.from.NNT(NNTpos = NNTpos, NNTneg=NNTneg, prev=prev))
  # same
  snHat = sesp["se"]
  tp = round(22*snHat)
  fn = round(22*(1-snHat))
  binom.confint(tp,22)  #CI for SN

  spHat = sesp["sp"]
  tn = round(40*spHat)
  fp = round(40*(1-spHat))
  binom.confint(tn,40)  #CI for SP

  Asn1=tp; Asn2=fn; Asp1=fp; Asp2=tn;
  prior = 1/2
  kernelPos = function(sn, nntvalue.. ){
    sp = spFunctionPos(sn, nntvalue=nntvalue..)
    dbeta(sn, shape1=Asn1+prior-1, shape2=Asn2+prior-1) *
      pbeta(sp, shape1=Asp1+prior-1, shape2=Asp2+prior-1) *
      (prev/(1-prev))^2 *sn^2 *(1-sn)^2 *
      beta(tn,fp-1)/beta(tn,fp)
  }

kernelNegInner = function(sp)
  dbeta(sp, shape1=Asp1+prior-1,
            shape2=Asp2+prior-1) /
    abs(2*sp-1)
  #    beta(tn,fp-1)/beta(tn,fp)

kernelNeg = function(sn, nntvalue..){
    #spUpper = spFunctionNeg(sn, nntvalue=nntvalue..)
  upperLimit = spFunctionNeg(sn1, nntvalue=nntvalue..)
  multiplier = (prev/(1-prev) * sn * (1-sn)) ^2 *
      dbeta(sn, shape1=Asn1+prior-1, shape2=Asn2+prior-1)
  integrals = sapply(sn, function(sn1)
        integrate(
          lower=0,
          upper=upperLimit,
          kernelNegInner
        )$value
      )
  return(multiplier * integrals)
  }

  marginalNNTpos = function(nntvalue.)
    integrate(f=kernelPos,
              nntvalue..=nntvalue.,
              lower = 0.001, upper = 0.999
    )$value


  marginalNNTneg = function(nntvalue.)
    integrate(f=kernelNeg,
              nntvalue..=nntvalue.,
              lower = 0.001, upper = 0.999
    )$value


  plot(sn<-seq(.01,.99,.01), sapply(sn, kernelPos, nnt=2))
  lines(sn<-seq(.01,.99,.01), sapply(sn, kernelPos, nnt=3))

  plot(nnt<-seq(1.1,10,.1),
       1-(sapply(nnt, marginalNNTpos ))
  )
  marginalNNTpos(1)   # 0.004856781
  plot(nnt<-seq(1,50,.1),
       sapply(nnt, marginalNNTneg ))
  marginalNNTneg(1000)  # 0.004856781
  marginalNNTneg(1)  # 0.004856781

  plot(1:10, sapply(1:10, kernelPos, sn=.8))
  plot(1:40, sapply(1:40, marginalNNTpos))


  snRandom<-rbeta(100000, shape1=Asn1+prior-1, shape2=Asn2+prior-1)
  spRandom<-1-rbeta(100000, shape1=Asp1+prior-1, shape2=Asp2+prior-1)
  #plot(snRandom, spRandom)
  nntPos = NNT.from.sesp(se=snRandom, sp=spRandom, prev=prev)[,1]
  NNTneg = NNT.from.sesp(se=snRandom, sp=spRandom, prev=prev)[,2]
  summary(nntPos)
  summary(NNTneg)
  ###  All this looks pretty good.
  quantile(nntPos, probs = c(alpha, 1-alpha))
  quantile(NNTneg, probs = c(alpha, 1-alpha))
  ###  Reasonable.
  cor(nntPos, NNTneg)
  plot(nntPos, NNTneg, log="y")

  par(mfrow=c(1,2))
  plot(density(nntPos), xlab=NA,main=NA)
  abline(v=NNTpos, col="blue")
  plot(density(NNTneg), xlab=NA,main=NA, xlim=c(1,70))
  abline(v=NNTneg, col="blue")


  findCDFforNNTneg = function(plimit)
    uniroot(f = function(nnt) plimit -
              marginalNNTneg(nnt),
            interval=c(1.1,10000)
    )
  alpha = 0.05
  findCDFforNNTneg(alpha)$root    ## should be near 16
  findCDFforNNTneg(1-alpha)$root  ## should be near 88
}
