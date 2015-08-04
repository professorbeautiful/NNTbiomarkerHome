##########   BEGINNING THE RECURRENCE RISK BENEFIT PLOTTING  ###########################
BenefitPlot <- function (
  sampleSize=PaikSampleSize) {

  initial.Random.seed = .Random.seed

  TAMcolor = 'red'
  TAM_CHEMOcolor = 'blue'
  Benefitcolor = 'darkgreen'


  with(tenYearDFS, plot(RS, Recur,
                        xlab="Recurrence Score (RS)",
                        ylab="Recurrence by 10 years",
                        ylim=c(0,0.35),
                        pch="" #,pch=c("C","T")[1 + (tenYearDFS$group=="TAM")])
  ))
  points(x=tenYearDFS$RS,
         predict(lm_TenYearDFS),
         pch=c("C","T")[1 + (tenYearDFS$group=="TAM")],
         col=ifelse(tenYearDFS$group=="TAM", TAMcolor, TAM_CHEMOcolor) )
  lines(lwd=3, RSvector, predicted_tenYearDFS[tenYearDFS_long$group=='TAM'],
        col=TAMcolor)
  lines(lwd=3, RSvector, predicted_tenYearDFS[tenYearDFS_long$group=='TAM_CHEMO'],
        col=TAM_CHEMOcolor)
 lines(0:100, benefit, col=Benefitcolor, lwd=3)


  rug(RSsample, col=TAMcolor, ticksize= -0.04)
  rug(theseBenefitted, col=Benefitcolor, ticksize = -0.08, lwd=2)
  abline(v=OncotypeRScutoffs, lty=2)

  legend(x="topleft",
         legend=c("TAM only", "TAM+CHEMO",
                  "would have benefitted: " %&% length(theseBenefitted)
                  %&% "/" %&% PaikSampleSize),
         col = c(TAMcolor, TAM_CHEMOcolor, Benefitcolor), pch=c("T","C", ""), lwd=c(3,3, 3))
  graphics::text(c(5, 21, 35), c(0.25, 0.25, 0.25),
       c(
         benefitTable[2,1]  %&% "/" %&% benefitTable[1,1],
         benefitTable[2,2]  %&% "/" %&% benefitTable[1,2],
         benefitTable[2,3] %&% "/" %&% benefitTable[1,3]),
       col=Benefitcolor)
  invisible(list(whichBenefitted=whichBenefitted,
                 theseBenefitted=theseBenefitted,
                 benefit=benefit,
                 # RSsampleBenefit=RSsampleBenefit,
                 RSsample=RSsample,
                 .Random.seed=initial.Random.seed))  # Worth saving.
}
