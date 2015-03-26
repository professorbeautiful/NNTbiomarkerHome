# independent flat priors for SN and SP

nntlower=2
nntupper=30
ppv = 1/nntlower
npv = 1 - 1/nntupper
prev = 0.15

Npositives = 10
Nnegatives = 30

#  (A)  for prospective study:
# N = 10 positives; tp = N * PPV = 5
tp = round(10*ppv)
fn = round(10*(1-ppv))
# N = 30 negatives; tn ~~ N * NPV = 29
tn = round(30*npv)
fp = round(30*(1-npv))

alpha = 0.025

# predictive intervals
prior = c(1/2,1/2)  ## Jeffreys
# prior = c(0,0)  # won't work with fp=1
(ppvPI = qbeta(c(alpha, 1-alpha), 
               tp+prior[1]-1, fn+prior[2]-1))
(binom.confint(5, 10))  #CI for SN
(nntPosPI = 1/ppvPI[2:1])

(npvPI = qbeta(c(alpha, 1-alpha), 
               tn+prior[1]-1, fp+prior[2]-1))
binom.confint(29, 30)  #CI for SP
(nntNegPI = 1/(1-npvPI))



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
pv.from.sesp(se=.8, sp=spFunctionPos(.8, 2), prev=prev)

pv.from.NNT(NNTpos = nntlower, NNTneg = nntupper, prev=prev)
(sesp = sesp.from.pv(ppv = ppv, npv = npv, prev=prev))
# 0.8333, 0.8529
### note- specificity of "no test" is already 85%!
sesp.from.NNT(NNTpos = nntlower, NNTneg=nntupper,
                     prev=prev)
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

kernelNeg = function(sn, nntvalue..){
  #spUpper = spFunctionNeg(sn, nntvalue=nntvalue..)
  ( prev/(1-prev) * sn * (1-sn)) ^2 *
    dbeta(sn, shape1=Asn1+prior-1, shape2=Asn2+prior-1) *
    sapply(sn, function(sn1)
      integrate(
        lower=0, 
        upper=spFunctionNeg(sn1, nntvalue=nntvalue..),
        function(sp)
          dbeta(sp, shape1=Asp1+prior-1, 
                shape2=Asp2+prior-1) / abs(2*sp-1)
      )$value
    )
  #    beta(tn,fp-1)/beta(tn,fp)
}

marginalNNTPos = function(nntvalue.)
  integrate(f=kernelPos,
    nntvalue..=nntvalue.,
    lower = 0.001, upper = 0.999
  )$value


marginalNNTNeg = function(nntvalue.)
  integrate(f=kernelNeg,
            nntvalue..=nntvalue.,
            lower = 0.001, upper = 0.999
  )$value

  
plot(sn<-seq(.01,.99,.01), sapply(sn, kernelPos, nnt=2))
lines(sn<-seq(.01,.99,.01), sapply(sn, kernelPos, nnt=3))

plot(nnt<-seq(1.1,10,.1), 
     1-(sapply(nnt, marginalNNTPos ))
     )
marginalNNTPos(1)   # 0.004856781
plot(nnt<-seq(1,50,.1), 
     sapply(nnt, marginalNNTNeg ))
marginalNNTNeg(1000)  # 0.004856781
marginalNNTNeg(1)  # 0.004856781

plot(1:10, sapply(1:10, kernelPos, sn=.8))
plot(1:40, sapply(1:40, marginalNNTPos))


snRandom<-rbeta(100000, shape1=Asn1+prior-1, shape2=Asn2+prior-1)
spRandom<-1-rbeta(100000, shape1=Asp1+prior-1, shape2=Asp2+prior-1)
#plot(snRandom, spRandom)
nntPos = NNT.from.sesp(se=snRandom, sp=spRandom, prev=prev)[,1]
nntNeg = NNT.from.sesp(se=snRandom, sp=spRandom, prev=prev)[,2]
summary(nntPos)
summary(nntNeg)
###  All this looks pretty good.
quantile(nntPos, probs = c(alpha, 1-alpha))
quantile(nntNeg, probs = c(alpha, 1-alpha))
###  Reasonable.
cor(nntPos, nntNeg)
plot(nntPos, nntNeg, log="y")

par(mfrow=c(1,2))
plot(density(nntPos), xlab=NA,main=NA)
abline(v=2, col="blue")
plot(density(nntNeg), xlab=NA,main=NA, xlim=c(1,70))
abline(v=30, col="blue")


findCDFforNNTNeg = function(plimit)
  uniroot(f = function(nnt) plimit - 
            marginalNNTNeg(nnt),
          interval=c(1.1,10000)
  )
alpha = 0.05
findCDFforNNTNeg(alpha)$root    ## should be near 16
findCDFforNNTNeg(1-alpha)$root  ## should be near 88


