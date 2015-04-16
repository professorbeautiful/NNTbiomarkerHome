
plotDiscomfort = function(
    barHeight = 0.15,
    barY = 0.65,
    NNTlower = 7,
    NNTupper = 16,
    NNTpos = NNTlower-1,
    NNTneg=NNTupper+1,
    drawAxes=F,
    drawPosNeg=T){

  labelCex = 2.0
  triangleDown = 25
  numbersY = 0.1
  YtriangleDown = 0.25
  triangleUpPch = 24
  YnntLowerUpper = numbersY+0.2
  YnntPosNeg = 0.85
  YtriangleUp = 0.8
  rightSideLimit = max(NNTneg * 1.15, NNTneg + 3)
  plot(c(1,rightSideLimit), c(0,1), pch="",
       axes=drawAxes, xlab="", ylab="")

  #abline(v=c(1,NNTlower,NNTupper))
  actBarLeft = 1
  actBarRight = NNTlower
  actBarX = actBarLeft + (actBarRight - actBarLeft)/2
  actBarWidth = actBarRight - actBarLeft
  symbols(actBarX, barY, inches=F,
          rectangles=matrix(c(actBarWidth, barHeight), nrow=1), bg="green", add=T)
  text(actBarX, barY, "Act!", cex=labelCex)

  discomfortBarLeft = NNTlower
  discomfortBarRight = NNTupper
  discomfortBarX = discomfortBarLeft + (discomfortBarRight - discomfortBarLeft)/2
  discomfortBarWidth = discomfortBarRight - discomfortBarLeft
  symbols(discomfortBarX, barY, inches=F,
          rectangles=matrix(c(discomfortBarWidth, barHeight), nrow=1),
          bg="red", add=T)
  text(discomfortBarX, barY, "discomfort range", cex=labelCex)

  waitBarLeft = NNTupper
  waitBarRight = rightSideLimit
  waitBarX = waitBarLeft + (waitBarRight - waitBarLeft)/2
  waitBarWidth = waitBarRight - waitBarLeft
  symbols(waitBarX, barY, inches=F,
          rectangles=matrix(c(waitBarWidth, barHeight), nrow=1),
          bg="green", add=T)
  text(waitBarX, barY, "Wait!", cex=  labelCex)

  text(1:rightSideLimit, numbersY, 1:rightSideLimit, cex=labelCex,
       col=c("black", "red")
       [1+(1:rightSideLimit) %between% c(NNTlower, NNTupper)]
  )
  symbols(c(NNTupper, NNTlower), c(numbersY,numbersY),
          circles=c(0.2,0.2),
  inches=F, add=T, fg="red", xpd=F)
  points(NNTlower, YnntLowerUpper, cex=3, pch=triangleDown, bg="red")
  points(NNTupper, YnntLowerUpper, cex=3, pch=triangleDown, bg="red")
  text(discomfortBarLeft, barY-barHeight/2,
       "NNTlower", pos=1, cex=2, col="red")
  text(discomfortBarRight, barY-barHeight/2,
       "NNTupper", pos=1, cex=2, col="red")
  if(drawPosNeg) {
    points(NNTpos, YtriangleUp, cex=3, pch=triangleUpPch, bg="darkgreen")
    points(NNTneg, YtriangleUp, cex=3, pch=triangleUpPch, bg="darkgreen")
    text(NNTpos, YnntPosNeg,
         "NNTpos", pos=3, cex=2, col="darkgreen")
    text(NNTneg, YnntPosNeg,
         "NNTneg", pos=3, cex=2, col="darkgreen")
  }
}

#plotDiscomfort()
