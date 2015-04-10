####   Revised 
####   From NNT to PPV and NPV
####   From prev and  PPV and NPV to se and sp

compare.decisions = function(NNTpos, NNTneg, prev, sp, se) {
	ppv.odds = 1/(NNTpos-1)
	ppv = ppv.odds/(1+ppv.odds)
	npv.odds = (NNTneg-1)
	npv = npv.odds/(1+npv.odds)
	odds = prev/(1-prev)
	saved.options = options(digits=4)

		#calculate expected utility)
		# U(wrong thing)=0
		EU.LB = (NNTpos-1)*prev*se + 1*(1-prev)*sp
		EU.UB = (NNTneg-1)*prev*se + 1*(1-prev)*sp
		ppv = prev*se/(prev*se + (1-prev)*(1-sp))
		npv = (1-prev)*sp/(prev*(1-se) + (1-prev)*sp)
		EU.LB.gP = (NNTpos-1)*ppv
		### compare to not treating, EU.gP.noTreat = (1-ppv)
		EU.UB.gP = (NNTneg-1)*ppv
		### compare to not treating, EU.gP.noTreat = (1-ppv)
		EU.gN = 1*npv
		### compare to not treating, EU.gN.Treat = (1-npv)(NNT-1)
		cat("U=", EU.LB.gP,   " \tif P, treat (NNTpos)\n")
		cat("U=", EU.UB.gP,   " \tif P, treat (NNTneg) \n")
		cat("U=", 1-ppv,   " \tif P, wait \n")
		cat("------------------------------------\n")
		cat("U=", EU.gN,   " \tif N, treat  \n")
		cat("U=", (1-npv)*(NNTneg-1),   " \tif N, wait  (NNTneg) \n")
		cat("U=", (1-npv)*(NNTpos-1),   " \tif P, wait  (NNTpos)\n")
		options(saved.options)
		return(c(NNTpos=NNTpos, NNTneg=NNTneg, EU.LB=EU.LB, EU.UB=EU.UB,
			EU.LB.gP=EU.LB.gP, EU.UB.gP=EU.UB.gP, EU.gN=EU.gN))
}




### I need to change to NNTPos and NNTNeg- remember
###    NNTPos < NNTLower < NNTUpper < NNTNeg


NNT.from.pv(.1,.1)



NNT.from.ppv.npv(pv=
	pv.from.NNT(NNT=c(NNTpos=2, NNTneg=10))
)




se.sp.required.from.pv(ppv=.1, npv=0.99, prev=0.001, feas=FALSE) 
se.sp.required.from.pv(.1, 0.90, 0.001, feas=FALSE) 

odds = function(prob) prob/(1-prob)
prob.from.odds = function(odds) odds/(1+odds)
pvp.pvn = function(se=0.8, sp=0.99, prev=0.001) {
	c(pvp=prob.from.odds(odds(prev)*se/(1-sp)),
	  pvn=prob.from.odds(1/odds(prev)*sp/(1-se)))
}
pvp.pvn(se=0.8, sp=0.999, prev=0.001)
pvp.pvn(se=0.9, sp=0.9, prev=0.5)

se.sp.required.from.NNT(NNTpos=2,  NNTneg=20, prev=0.15)

compare.decisions(2, 20, 0.15, sp=0.99, se=0.99)
compare.decisions(2, 20, 0.15, sp=0.86924, se=0.74074)

compare.decisions(NNTpos=4, NNTneg=20, prev=0.15, sp=0.99, se=0.99)

compare.decisions(2, 5, 0.15)
compare.decisions(2, 5, 0.15, se=.95, sp=.95)


pvp.pvn(se=0.95, sp=0.95, prev=0.15)

compare.decisions(5, 500, 0.1)
compare.decisionsrequired(5, 100, 0.001)
compare.decisions(10, 100, 0.1)
compare.decisions(10, 100, 0.2)

compare.decisions(10, 100, 0.001)

pvp.pvn(se=0.001, sp=1, prev=0.1)
pvp.pvn(se=0.001, sp=.99, prev=0.001)

pvp.pvn(se=0.999, sp=.999, prev=0.001)

pvp.pvn(se=0.9, sp=1-5e-3, prev=0.001)

x=pvp.pvn(se=0.98, sp=1-1e-1, prev=0.001)


pv.to.sesp(sesp.to.pv(c(.7,.15),prev=0.2),prev=0.2)
sesp.to.pv(pv.to.sesp(c(ppv=.2593,npv=.8696),prev=0.2),prev=0.2)
pv=sesp.to.pv(c(.7,.15),prev=0.2)
ppv=pv[1];npv=pv[2];
ppv.odds=ppv/(1-ppv);npv.odds=npv/(1-npv)
odds = 2/8
(odds/ppv.odds - 1)/(1/npv.odds/ppv.odds - 1) ### sp = 0.15; OK. sp.odds=1
(odds-ppv.odds)/(1/npv.odds-ppv.odds) ### sp = 0.15; OK. sp.odds=1

(ppv.odds-odds)/(odds-1/npv.odds) ### sp/(1-sp).  OK.
(1/odds-npv.odds)/(1/ppv.odds-npv.odds) ### se = 0.7  OK.
(1/odds-npv.odds)/(1/ppv.odds-1/odds) ### se/(1-se) = 2.333  OK.

sesp.to.pv(pv.to.sesp(c(.1,.99),prev=0.001),prev=0.001)
sesp.to.pv(c(.9,.999), prev=0.001)

compare.decisions(10, 100, 0.001)



achievable.se.sp()
achievable.se.sp(axes="NNT", drawTable=F, drawArrows=F, ylim=c(1,100))

ppv.temp = the.prev/2

npv.temp = 1-the.prev/2
sesp = pv.to.sesp(c(ppv.temp, npv.temp), the.prev)
text(ppv.temp, npv.temp, paste("se=", sesp["se"], "\nsp=", sesp["sp"])  )

##### If se < 0, upper left, so increase ppv to the.prev and above.
sesp=pv.to.sesp(cbind(ppv=seq(the.prev,1,len=10), nvp=npv.temp), prev=the.prev)

ppv.temp = 1-the.prev/2
npv.temp = the.prev/2
sesp = pv.to.sesp(c(ppv.temp, npv.temp), the.prev)
text(ppv.temp, npv.temp, paste("se=", sesp["se"], "\nsp=", sesp["sp"])  )

##### If sp < 0, lower right, so increase npv to the.prev and above.
sesp=print(pv.to.sesp(cbind(ppv=ppv.temp, npv=seq(the.prev,1,len=10)), prev=the.prev))


###AHA!!!

