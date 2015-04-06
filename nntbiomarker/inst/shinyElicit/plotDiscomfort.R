
plotDiscomfort = function(
    barHeight = 0.05,
    barY = 0.1,
    NNTlower = 7,
    NNTupper = 16,
    NNTpos = NNTlower-1,
    NNTneg=NNTupper+1){

  rightSideLimit = max(NNTneg * 1.15, NNTneg + 3)
  plot(c(1,rightSideLimit), c(0,0.16), pch="",
       axes=F, xlab="", ylab="")

  #abline(v=c(1,NNTlower,NNTupper))
  actBarLeft = 1
  actBarRight = NNTlower
  actBarX = actBarLeft + (actBarRight - actBarLeft)/2
  actBarWidth = actBarRight - actBarLeft
  symbols(actBarX, barY, inches=F,
          rectangles=matrix(c(actBarWidth, barHeight), nrow=1), bg="green", add=T)
  text(actBarX, barY, "Act!", cex=1.5)

  discomfortBarLeft = NNTlower
  discomfortBarRight = NNTupper
  discomfortBarX = discomfortBarLeft + (discomfortBarRight - discomfortBarLeft)/2
  discomfortBarWidth = discomfortBarRight - discomfortBarLeft
  symbols(discomfortBarX, barY, inches=F,
          rectangles=matrix(c(discomfortBarWidth, barHeight), nrow=1),
          bg="red", add=T)
  text(discomfortBarX, barY, "discomfort range", cex=1.5)

  waitBarLeft = NNTupper
  waitBarRight = rightSideLimit
  waitBarX = waitBarLeft + (waitBarRight - waitBarLeft)/2
  waitBarWidth = waitBarRight - waitBarLeft
  symbols(waitBarX, barY, inches=F,
          rectangles=matrix(c(waitBarWidth, barHeight), nrow=1),
          bg="green", add=T)
  text(waitBarX, barY, "Wait!", cex=1.5)

  numbersY = 0.02
  text(1:rightSideLimit, numbersY, 1:rightSideLimit, cex=1.5,
       col=c("black", "red")
       [1+(1:rightSideLimit) %between% c(NNTlower, NNTupper)]
  )
  symbols(c(NNTupper, NNTlower), c(numbersY,numbersY),
          circles=c(0.2,0.2),
  inches=F, add=T, fg="red", xpd=F)
  points(NNTlower, 0.05, cex=3, pch=25, bg="red")
  points(NNTupper, 0.05, cex=3, pch=25, bg="red")
  text(discomfortBarLeft, barY-barHeight/2,
       "NNTlower", pos=1, cex=2, col="red")
  text(discomfortBarRight, barY-barHeight/2,
       "NNTupper", pos=1, cex=2, col="red")
  points(NNTpos, barY+barHeight/1.8, cex=3, pch=24, bg="darkgreen")
  points(NNTneg, barY+barHeight/1.8, cex=3, pch=24, bg="darkgreen")
  text(NNTpos, barY+barHeight/1.6,
       "NNTpos", pos=3, cex=2, col="darkgreen")
  text(NNTneg, barY+barHeight/1.6,
       "NNTneg", pos=3, cex=2, col="darkgreen")
}

#plotDiscomfort()
