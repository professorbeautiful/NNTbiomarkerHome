
plotDiscomfort = function(
  rightSideLimit = 21,
  barHeight = 0.25,
  barY = 0.7,
  NNTlower = 7,
  NNTupper = 16){
  plot(c(1,rightSideLimit), c(0,1), pch="", axes=F, xlab="", ylab="")

  #abline(v=c(1,NNTlower,NNTupper))
  actBarLeft = 1
  actBarRight = NNTlower
  actBarX = actBarLeft + (actBarRight - actBarLeft)/2
  actBarWidth = actBarRight - actBarLeft
  symbols(actBarX, barY, inches=F,
          rectangles=matrix(c(actBarWidth, barHeight), nrow=1), bg="green", add=T)
  text(actBarX, barY, "Act!")

  discomfortBarLeft = NNTlower
  discomfortBarRight = NNTupper
  discomfortBarX = discomfortBarLeft + (discomfortBarRight - discomfortBarLeft)/2
  discomfortBarWidth = discomfortBarRight - discomfortBarLeft
  symbols(discomfortBarX, barY, inches=F,
          rectangles=matrix(c(discomfortBarWidth, barHeight), nrow=1),
          bg="grey", add=T)
  text(discomfortBarX, barY, "discomfort range")

  discomfortBarLeft = NNTupper
  discomfortBarRight = rightSideLimit
  discomfortBarX = discomfortBarLeft + (discomfortBarRight - discomfortBarLeft)/2
  discomfortBarWidth = discomfortBarRight - discomfortBarLeft
  symbols(discomfortBarX, barY, inches=F,
          rectangles=matrix(c(discomfortBarWidth, barHeight), nrow=1),
          bg="pink", add=T)
  text(discomfortBarX, barY, "Wait!")

  text(1:rightSideLimit, 0, 1:rightSideLimit, cex=0.5)
}

#plotDiscomfort()
