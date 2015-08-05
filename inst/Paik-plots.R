##  These plots require sourceing Paik-fit first.

if( ! identical(ls(pattern="Paik_nnt", pos=1), 1) )
  source("inst/Paik-fit.R")

# set.seed(benefitPlotOutput$.Random.seed)
benefitPlotOutput = BenefitPlot()
#.Random.seed = benefitPlotOutput$.Random.seed
set.seed(benefitPlotOutput$.Random.seed)


##### Pr(benefit | would recur, RS)  increases with RS. ####

# library(magrittr)
# library(plyr)

par(mai=c(1,1,2,1))
with(tenYearDFS_long %>% subset(., group=="TAM_CHEMO"), {
  plot(RS, benefit / predicted,
       xlim=c(0,50), ylab='Pr(benefit | would recur)')
  abline(v=c(OncotypeRScutoffs, 50),
         h=print((benefit / predicted )[argmin(RS, c(OncotypeRScutoffs, 50))]), col="red")
  print((benefit )[argmin(RS, c(OncotypeRScutoffs, 50))])
#   [1] 0.4053006 0.8855322 1.5891365
#   [1] 0.02481807 0.07211056 0.20559292
})


#####  Plotting  NNT by Oncotype DX RS  #####
OncotypeNNTrange = round(nnt[as.character(OncotypeRScutoffs)])
TailorXNNTrange = round(nnt[as.character(TailorXRScutoffs)])

plot(RSvector, nnt,   xaxs="i",   yaxs="i", ##  log='y',
     xlim=c(0, 50),
     ylim=c(0,120), type="l", lwd=4,
     col=Benefitcolor,
     xlab="Recurrence score", ylab="Number needed to treat")

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
     srt=90, adj=c(0,1), xpd=NA, col='red', " OncotypeDX")
points(TailorXRScutoffs, nnt[as.character(TailorXRScutoffs)],
       col='blue', type="h", lty=1, lwd=2)
text(x=TailorXRScutoffs[2], y=nnt[as.character(TailorXRScutoffs)][2],
     srt=90, adj=c(0, 1), xpd=NA, col='blue', " TailorX")
text(x=9, y=100,
     srt=90, adj=c(0, 1), xpd=NA, col='blue', " TailorX")
for(iCut in 1:2)
  lines(c(-10, TailorXRScutoffs[iCut]),
        rep(times=2, nnt[as.character(TailorXRScutoffs[iCut])]),
        col="blue", lty=2)
'%&%' = function(a, b) paste0(a,b)
legend("topright",
       legend=c(
         "Number needed to treat",
         "One patient who benefits",
         "",
         "Oncotype DX intermediate risk",
         "  RS Boundaries " %&%  paste(OncotypeRScutoffs, collapse=", ") ,
         "  NNT Boundaries "  %&% paste(OncotypeNNTrange, collapse=", ") ,
         "",
         "TailorX intermediate risk",
         "  RS Boundaries " %&%  paste(TailorXRScutoffs, collapse=", ") ,
         "  NNT Boundaries "  %&% paste(TailorXNNTrange, collapse=", ")
       ),
       lty=c(1, 1, 0, 0, 1, 2, 0, 0, 1, 2),
       text.col= c("darkgreen", "green", "white",
                   "red", "red", "red", "white",
                   "blue", "blue", "blue", "white"),
       col= c("darkgreen", "green", "white",
              "red", "red", "red", "white",
              "blue", "blue", "blue", "white"),
       lwd = c(4, 6, 0, 0, 2, 2, 0, 0, 2, 2)
)
title("NNT by Oncotype DX RS")
rect(0, 0, 50, 1, col = 'green')  # the patient who benefits


####  box plots ####
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
  X=RSsampleBenefit),
  #  NNTlower = OncotypeNNTrange[2], NNTupper = OncotypeNNTrange[1],
  #  whichPlots=c("pv")
  #  whichPlots=c("density", "raw", "ROC", "pv", "nnt", "nntRange")
  NNTlower=14, NNTupper=40,   whichPlots="ROC"
)

#### ROC curves ####
ROCplots(data=data.frame(
  class=(1:length(RSsample) %in% whichBenefitted),
  X=RSsample),
  NNTlower=14, NNTupper=40,   whichPlots="ROC"
)

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

