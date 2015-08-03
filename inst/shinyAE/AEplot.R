#### Adverse event NNT bars ####
#### For CMFT, from table 5 of fisher1997.

AEplot = function(RSinput = 30){
  if(is.null(RSinput))
    RSinput = 48
  par(mai=c(0,0,1,0))
  aeProb = c(2.9,15,57,20,5,0.1)
  boxwidths = c(1, (nnt[RSinput] - 1) * aeProb / 100)
  opts = options(warn=-1)
  symbols(x=rep(0, 7), y=7:1, inches=F,
          xlim=c(-ceiling(max(boxwidths)), ceiling(max(boxwidths))) * 0.75,
          rectangles = cbind(boxwidths, 1), bg = c("green", boxcolors), 
          axes=F, 
          xlab="", ylab="")
  options(opts)
  "%except%" <-  function (vector, condition) vector[match(vector, condition, 0) == 0]
  verticalsX = lapply(boxwidths[-1], function(bw)
    if(bw <= 1)  numeric(0)  else  -floor(bw/2):floor(bw/2)
  )
  verticalsY = rep(6:1, times=sapply(verticalsX, length))
  segments(x0= unlist(verticalsX),
           y0 = verticalsY - 1/2, y1 = verticalsY + 1/2
  )
  text(x = boxwidths/2, y=7:1,
       c("Benefitted", "No AE", "Mild", "Moderate", "Severe", "Life-threatening", "Died"),
       pos=4 , xpd=NA)
  text(x = - boxwidths/2, y=7:1, round(boxwidths, 1),
       pos=2 )
  title(paste0("Outcomes for ", round(nnt[RSinput]), " patients treated, if 1 benefits\n", 
               "RS = ", RSinput, "  NNT = ", round(nnt[RSinput])))
}
# 
# for(RSinput in c(OncotypeRScutoffs, TailorXRScutoffs))
#   AEplot(RSinput)
