# library(XML)
# getNodeSet(xmlTreeParse("/Users/Roger/Dropbox/_HOME/Paper_on_Biomarker_Studies_and_NNT/oncotypeDX-risk-functions.jpg.xml"),
#             "point")
## I'd have to understand getNodeSet() better!

OncotypeRScutoffs = c(17, 30)  ### upper boundaries
TailorXRScutoffs = c(11, 25)  ### upper boundaries

pointStrings = grep(value=T, pattern = "<point",
                    read.csv(header=F,
                             "/Users/Roger/Dropbox/_HOME/Paper_on_Biomarker_Studies_and_NNT/Paik2006-10yearDFS."
                             %&% "TAM.xml",
                             stringsAsFactors = F)[[1]]
)
dxBegin = regexpr("dx='", pointStrings)+4
dyBegin = regexpr("dy='", pointStrings)+4
dxEnd = dyBegin - 7
dyEnd = nchar(pointStrings) - 4
allX_TAM = as.numeric(substr(pointStrings, dxBegin, dxEnd))
allY_TAM = as.numeric(substr(pointStrings, dyBegin, dyEnd))


### Create pointStrings ####
pointStrings = grep(value=T, pattern = "<point",
     read.csv(header=F,
              "/Users/Roger/Dropbox/_HOME/Paper_on_Biomarker_Studies_and_NNT/Paik2006-10yearDFS."
              %&% "TAM+CHEMO.xml",
              stringsAsFactors = F)[[1]]
)
dxBegin = regexpr("dx='", pointStrings)+4
dyBegin = regexpr("dy='", pointStrings)+4
dxEnd = dyBegin - 7
dyEnd = nchar(pointStrings) - 4
allX_TAM_CHEMO = as.numeric(substr(pointStrings, dxBegin, dxEnd))
allY_TAM_CHEMO = as.numeric(substr(pointStrings, dyBegin, dyEnd))
plot(allX_TAM, allY_TAM)
points(allX_TAM_CHEMO, allY_TAM_CHEMO)

lm_TenYearDFS_TAM = lm(allY_TAM ~ poly(allX_TAM,2))
lm_TenYearDFS_TAM_CHEMO = lm(allY_TAM_CHEMO ~ poly(allX_TAM_CHEMO,2))
tenYearDFS = data.frame(RS=c(allX_TAM, allX_TAM_CHEMO),
                       Recur=c(allY_TAM, allY_TAM_CHEMO),
                       group=rep(c('TAM', 'TAM_CHEMO'),
                                 times=c(length(allX_TAM), length(allX_TAM_CHEMO)))
)
tenYearDFS$RS = pmax(0, tenYearDFS$RS)
tenYearDFS$one = 1
tenYearDFS$RS2 = tenYearDFS$RS^2
lm_TenYearDFS = lm(data=tenYearDFS,
                   Recur ~ group*RS + group*RS2 - group
) ### shared intercept

predict(lm_TenYearDFS, newdata=
          data.frame(RS=c(0,0), RS2=c(0,0), group=c("TAM", "TAM_CHEMO"), one=c(1,1)))
RSvector = seq(0,100)
nRS = length(RSvector)
tenYearDFS_long = data.frame(RS=rep(RSvector,2), RS2=rep((RSvector)^2,2),
                           group=rep(c('TAM','TAM_CHEMO'), each=nRS), one=1)
predicted_TenYearDFS = predict( lm_TenYearDFS, newdata=tenYearDFS_long)
tenYearDFS_long$predicted = predicted_TenYearDFS

############# RS distribution modeling, it to match the categories   ##############
ProportionByRiskCat = c(51, 22, 27)/100
sampleSize = 668
### Let's try a gamma fit.
# shapescale = nlm(function(shapescale)
#   (pgamma(OncotypeRScutoffs[1], shape=shapescale[1], scale=shapescale[2]) - 0.51)^2 +
#       (pgamma(OncotypeRScutoffs[2], shape=shapescale[1], scale=shapescale[2]) -
#          pgamma(OncotypeRScutoffs[2], shape=shapescale[1], scale=shapescale[2]) - 0.22)^2 ,
#   p=c(shape=5, scale=17/5) # starting values
# )$estimate
# pgamma(OncotypeRScutoffs[1], shape=shapescale[1], scale=shapescale[2])
# pgamma(OncotypeRScutoffs[2], shape=shapescale[1], scale=shapescale[2])
#Perfect.  But generates long tail values.
### Let's try a beta fit.
abParam = nlm(function(abParam)
  (pbeta(OncotypeRScutoffs[1]/50, shape1=abParam[1], shape2=abParam[2]) - 0.51)^2 +
    (pbeta(OncotypeRScutoffs[2]/50, shape1=abParam[1], shape2=abParam[2]) -
       pbeta(OncotypeRScutoffs[1]/50, shape1=abParam[1], shape2=abParam[2]) - 0.22)^2 ,
  p=c(shape1=2, shape2=2)
)$estimate
pbeta(OncotypeRScutoffs[1]/50, shape1=abParam[1], shape2=abParam[2])
pbeta(OncotypeRScutoffs[2]/50, shape1=abParam[1], shape2=abParam[2])
#Perfect.

##########   BEGINNING THE RECURRENCE RISK BENEFIT PLOTTING  ###########################
BenefitPlot <- function (
  sampleSize=PaikSampleSize) {

  initial.Random.seed = .Random.seed
  with(tenYearDFS, plot(RS, Recur,
                        xlab="Recurrence Score (RS)",
                        ylab="Recurrence by 10 years",
                        ylim=c(0,0.35),
                        pch="" #,pch=c("C","T")[1 + (tenYearDFS$group=="TAM")])
  ))
  points(x=tenYearDFS$RS,
         predict(lm_TenYearDFS),
         pch=c("C","T")[1 + (tenYearDFS$group=="TAM")],
         col=2 + (tenYearDFS$group=="TAM"))
  lines(lwd=3, RSvector, predicted_TenYearDFS[tenYearDFS_long$group=='TAM'], col="green")
  lines(lwd=3, RSvector, predicted_TenYearDFS[tenYearDFS_long$group=='TAM_CHEMO'], col="red")

  benefit =
    (-1)*apply(matrix(predicted_TenYearDFS, ncol=2), 1, diff)
  tenYearDFS_long$benefit = c(rep(0, length(benefit)),
                              benefit)
  tenYearDFS_long_treated = tenYearDFS_long[tenYearDFS_long$group=="TAM_CHEMO", ]
  tenYearDFS_long_treated$control = tenYearDFS_long$predicted[1:101]
  tenYearDFS_long_treated$benefit2 =  ### OK; the same.
    tenYearDFS_long_treated$control -
    tenYearDFS_long_treated$predicted

  #RSsample = rgamma(sampleSize, shape=shapescale[1], scale=shapescale[2])
  RSsample = 50 * rbeta(sampleSize, shape1=abParam[1], shape2=abParam[2])
  #RSsample = sort(RSsample[RSsample < 50])
  RSsample = sort(RSsample)
  RSsampleBenefit = tenYearDFS_long_treated$benefit[
    match(ceiling(RSsample), tenYearDFS_long_treated$RS)]
  RSsampleBenefit = pmax(0, RSsampleBenefit)
  lines(RSsample, RSsampleBenefit, col="blue", lwd=3)

  whichBenefitted = which(1==rbinom(sampleSize, 1, RSsampleBenefit))
  theseBenefitted = RSsample[whichBenefitted]

  ### Who benefits from T+C?   ##

  rug(RSsample, col="red", ticksize= -0.04)
  rug(theseBenefitted, col="blue", ticksize = -0.08, lwd=2)
  abline(v=OncotypeRScutoffs, lty=2)

  legend(x="topleft",
         legend=c("TAM only", "TAM+CHEMO",
                  "would have benefitted: " %&% length(theseBenefitted)
                  %&% "/" %&% PaikSampleSize),
         col = c("green", "red", "blue"), pch=c("T","C", ""), lwd=c(3,3, 3))
  split(RSsample, cut(RSsample, c(0, 11, 30)))
  split(theseBenefitted, cut(theseBenefitted, c(0, 11, 30)))
  benefitTable = table( RSsample %in% theseBenefitted,
                        cut(RSsample, c(0, OncotypeRScutoffs, Inf )))
  text(c(5, 21, 35), c(0.25, 0.25, 0.25),
       c(
         benefitTable[2,1]  %&% "/" %&% benefitTable[1,1],
         benefitTable[2,2]  %&% "/" %&% benefitTable[1,2],
         benefitTable[2,3] %&% "/" %&% benefitTable[1,3]),
       col="blue")
  invisible(list(whichBenefitted=whichBenefitted,
                 theseBenefitted=theseBenefitted,
                 RSsampleBenefit=RSsampleBenefit,
                 RSsample=RSsample,
                 .Random.seed=initial.Random.seed))  # Worth saving.
}


benefitPlotOutput = BenefitPlot()
whichBenefitted = benefitPlotOutput$whichBenefitted
theseBenefitted = benefitPlotOutput$theseBenefitted
RSsampleBenefit = benefitPlotOutput$RSsampleBenefit
RSsample = benefitPlotOutput$RSsample
.Random.seed = benefitPlotOutput$.Random.seed
set.seed(benefitPlotOutput$.Random.seed)


#### NUMBER NEEDED TO TREAT   ####

nnt = 1/benefit
nnt[nnt<=0] = Inf
names(nnt) = RSvector
OncotypeRScutoffs = c(17, 30)  ### upper boundaries
TailorXRScutoffs = c(11, 25)  ### upper boundaries
OncotypeNNTrange = round(nnt[as.character(OncotypeRScutoffs)])
TailorXNNTrange = round(nnt[as.character(TailorXRScutoffs)])
#nnt = pmin(nnt, 1000)
plot(RSvector, nnt, log='y', ylim=c(1,1000), type="l", lwd=3,
     xlab="Recurrence score", ylab="Number needed to treat")
argmin = function(v, target=0) which(abs(v-target) == min(abs(v-target))[1])
RSforNNTupper = argmin(nnt,NNTupper)
RSforNNTlower = argmin(nnt,NNTlower)
# points(tenYearDFS_long$RS[c(RSforNNTupper, RSforNNTlower)],
#        c(NNTupper, NNTlower),
#        col='darkgreen', type="h", lty=2, lwd=2)
# abline(h=c(NNTlower, NNTupper), col='darkgreen', lty=2, lwd=2)
points(OncotypeRScutoffs, nnt[as.character(OncotypeRScutoffs)],
       col='red', type="h", lty=1, lwd=2)
for(iCut in 1:2)
  lines(c(-10, OncotypeRScutoffs[iCut]),
      rep(times=2, nnt[as.character(OncotypeRScutoffs[iCut])]),
      col="red", lty=2)
text(x=OncotypeRScutoffs, y=nnt[as.character(OncotypeRScutoffs)],
     srt=90, adj=c(0,1), xpd=NA, col='red', "OncotypeDX")
points(TailorXRScutoffs, nnt[as.character(TailorXRScutoffs)],
       col='blue', type="h", lty=1, lwd=2)
text(x=TailorXRScutoffs, y=nnt[as.character(TailorXRScutoffs)],
     srt=90, adj=c(0, 1), xpd=NA, col='blue', "TailorX")
for(iCut in 1:2)
  lines(c(-10, TailorXRScutoffs[iCut]),
        rep(times=2, nnt[as.character(TailorXRScutoffs[iCut])]),
        col="blue", lty=2)
legend("topright",
       legend=c(
                "Oncotype DX risk groups ",
                "  Cutoffs " %&%  paste(OncotypeRScutoffs, collapse=", ") ,
                "  NNT range "  %&% paste(OncotypeNNTrange, collapse=", ") ,
                "",
                "TailorX risk groups ",
                "  Cutoffs " %&%  paste(TailorXRScutoffs, collapse=", ") ,
                "  NNT range "  %&% paste(TailorXNNTrange, collapse=", ")
                ),
                lty=c(0, 1, 2, 0, 0, 1, 2),
       text.col= c("red", "red", "red", "white","blue", "blue", "blue" ),
       col=      c("white", "red", "red", "white", "white", "blue", "blue" ),
       lwd = c(0, 2, 2, 0, 0,2, 2)
)
title("NNT by Oncotype DX RS")
########
########
RSsampleBtoT = rbinom(length(RSsampleBenefit), 1, RSsampleBenefit)
prevalence = sum(RSsampleBtoT)/length(RSsampleBtoT)  ### 3% benefit

boxplot(split(RSsample, RSsampleBtoT))
stupidBarPlot(split(RSsample, RSsampleBtoT),
              groupNames=c("Did not recur", "Recurred"), ylab= "RS", xlab="Outcome")
smartBarPlot(split(RSsample, RSsampleBtoT), boxfill="#FF000033",
             pointcol="grey",
             groupNames=c("Did not recur", "Recurred"), ylab= "RS", xlab="Outcome")


# Predicting recurrence if TAM group.  ####
ROCplots(data=data.frame(
  class=tenYearDFS$Recur,
  X=tenYearDFS$RS) [tenYearDFS$group=="TAM", ],
#  NNTlower = OncotypeNNTrange[2], NNTupper = OncotypeNNTrange[1],
#  whichPlots=c("pv")
#  whichPlots=c("density", "raw", "ROC", "pv", "nnt", "nntRange")
  NNTlower=NA,   whichPlots="ROC"
)


# Predicting benefit from +CHEMO   ####
ROCplots(data=data.frame(
  class=(1==rbinom(PaikSampleSize, 1, RSsampleBenefit)),
  X=tenYearDFS$RS) [tenYearDFS$group=="TAM", ],
  #  NNTlower = OncotypeNNTrange[2], NNTupper = OncotypeNNTrange[1],
  #  whichPlots=c("pv")
  #  whichPlots=c("density", "raw", "ROC", "pv", "nnt", "nntRange")
  NNTlower=NA,   whichPlots="ROC"
)



RecurDiff = diff(
  laply(split(tenYearDFS$Recur,tenYearDFS$group), diff)
  )

ROCplots(data=data.frame(
  class=(1:length(RSsample) %in% whichBenefitted),
    X=RSsample),
    NNTlower=6,   whichPlots="ROC"
  )

benefitTable = table( RSsample %in% theseBenefitted,
       cut(RSsample, c(0, OncotypeRScutoffs, Inf )))

sensitivity30 = benefitTable["TRUE", "(30,Inf]"]/ sum(benefitTable["TRUE", ])
specificity30 = 1 - benefitTable["FALSE", "(30,Inf]"]/ sum(benefitTable["FALSE", ])
points(pch="O", x=1-specificity30, y=sensitivity30, col="red")
text(labels="x0=30", x=1-specificity30, y=sensitivity30, adj=c(1.3,0),col="red")
points(pch="O", x=1-specificity30, y=sensitivity30, col="red")
text(labels="x0=30", x=1-specificity30, y=sensitivity30, adj=c(1.3,0),col="red")

sensitivity17 = 1 - benefitTable["TRUE", "(0,17]"]/ sum(benefitTable["TRUE", ])
specificity17 = benefitTable["FALSE", "(0,17]"]/ sum(benefitTable["FALSE", ])
points(pch="O", x=1-specificity17, y=sensitivity17, col="red")
text(labels="x0=17", x=1-specificity17, y=sensitivity17, adj=c(1.3,0),col="red")
points(pch="O", x=1-specificity17, y=sensitivity17, col="red")
text(labels="x0=17", x=1-specificity17, y=sensitivity17, adj=c(1.3,0),col="red")



ROCplots(data=data.frame(
  class=(1:length(RSsample) %in% whichBenefitted),
  X=RSsample),
  NNTlower=NA,   whichPlots="pv",
  title="My PV plot"
)
ppv30 = benefitTable["TRUE", "(30,Inf]"]/ sum(benefitTable[ , "(30,Inf]"])
npv30 = 1 - benefitTable["FALSE", "(30,Inf]"]/ sum(benefitTable[ ,  "(30,Inf]"])
points(pch="O", x=npv30, y=ppv30, col="red")
text(labels="x0=30", x=1-npv30, y=ppv30, adj=c(1.3,0),col="red")
points(pch="O", x=1-npv30, y=ppv30, col="red")
text(labels="x0=30", x=1-npv30, y=ppv30, adj=c(1.3,0),col="red")

ppv17 = 1 - benefitTable["TRUE", "(0,17]"]/ sum(benefitTable["TRUE", ])
npv17 = benefitTable["FALSE", "(0,17]"]/ sum(benefitTable["FALSE", ])
points(pch="O", x=1-npv17, y=ppv17, col="red")
text(labels="x0=17", x=1-npv17, y=ppv17, adj=c(1.3,0),col="red")
points(pch="O", x=1-npv17, y=ppv17, col="red")
text(labels="x0=17", x=1-npv17, y=ppv17, adj=c(1.3,0),col="red")
