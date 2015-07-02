getNodeSet(xmlTreeParse("/Users/Roger/Dropbox/_HOME/Paper_on_Biomarker_Studies_and_NNT/oncotypeDX-risk-functions.jpg.xml"),
            "point")

pointStrings = grep(value=T, pattern = "<point",
     read.csv(header=F,
              "/Users/Roger/Dropbox/_HOME/Paper_on_Biomarker_Studies_and_NNT/Paik2006-10yearDFS.TAM+CHEMO.xml",
              stringsAsFactors = F)[[1]]
)
dxBegin = regexpr("dx='", pointStrings)+4
dyBegin = regexpr("dy='", pointStrings)+4
dxEnd = dyBegin - 7
dyEnd = nchar(pointStrings) - 4
allX_TAM_CHEMO = as.numeric(substr(pointStrings, dxBegin, dxEnd))
allY_TAM_CHEMO = as.numeric(substr(pointStrings, dyBegin, dyEnd))
lines(allX_TAM, allY_TAM)
lines(allX_TAM_CHEMO, allY_TAM_CHEMO)
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
legend(x="topleft", legend=c("TAM only", "TAM+CHEMO"),col = c("green", "red"), pch=c("T","C"), lwd=c(3,3))

nnt = 1/benefit
nnt[nnt<=0] = Inf
#nnt = pmin(nnt, 1000)
plot(RSvector, nnt, log='y', ylim=c(1,1000), type="l")
argmin = function(v, target=0) which(abs(v-target) == min(abs(v-target))[1])
points(tenYearDFS_long$RS[argmin(nnt,50)], nnt[argmin(nnt,50)],
      col='red', type="h")
points(tenYearDFS_long$RS[argmin(nnt,5)], nnt[argmin(nnt,5)], col='red', type="h")
abline(h=c(11,20), lty=2)

########
########

### prevalence

ProportionByRiskCat = c(51, 22, 27)/100
sampleSize = 668
### Let's try a gamma fit.
shapescale = nlm(function(shapescale)
  (pgamma(17, shape=shapescale[1], scale=shapescale[2]) - 0.51)^2 +
      (pgamma(30, shape=shapescale[1], scale=shapescale[2]) -
         pgamma(17, shape=shapescale[1], scale=shapescale[2]) - 0.22)^2 ,
  p=c(shape=5, scale=17/5)
)$estimate
pgamma(17, shape=shapescale[1], scale=shapescale[2])
pgamma(30, shape=shapescale[1], scale=shapescale[2])
#Perfect.

RSsample = rgamma(sampleSize, shape=shapescale[1], scale=shapescale[2])
RSsample = sort(RSsample[RSsample < 50])
RSsampleBenefit = tenYearDFS_long_treated$benefit[match(ceiling(RSsample),tenYearDFS_long_treated$RS)]
RSsampleBenefit = pmax(0, RSsampleBenefit)
lines(RSsample, RSsampleBenefit, col="blue")
rug(jitter(sample(RSsample, 668)), col="blue")

RSsampleBtoT = rbinom(length(RSsampleBenefit), 1, RSsampleBenefit)
prevalence = sum(RSsampleBtoT)/length(RSsampleBtoT)  ### 3% benefit

boxplot(split(RSsample, RSsampleBtoT))
stupidBarPlot(split(RSsample, RSsampleBtoT),
              groupNames=c("Did not recur", "Recurred"), ylab= "RS", xlab="Outcome")
smartBarPlot(split(RSsample, RSsampleBtoT), boxfill="#FF000033",
             pointcol="grey",
             groupNames=c("Did not recur", "Recurred"), ylab= "RS", xlab="Outcome")


# Predicting recurrence if TAM group.
ROCplots(data=data.frame(
  class=tenYearDFS$Recur,
  X=tenYearDFS$RS) [tenYearDFS$group=="TAM", ],
  NNTlower = 5, NNTupper = 30,
  whichPlots=c("density", "raw", "ROC", "pv", "nnt", "nntRange")
  )


RecurDiff = diff(
  laply(split(tenYearDFS$Recur,tenYearDFS$group), diff)
  )
ROCplots(data=data.frame(
  class=tenYearDFS$Recur,
  X=tenYearDFS$RS) [tenYearDFS$group=="TAM", ],
  NNTlower = 5, NNTupper = 30,
  whichPlots=c("density", "raw", "ROC", "pv", "nnt", "nntRange")
)

