
AERiskTable = c(.75, .20, .05, .001)   #Data from B20 trial; CMFT
names(AERiskTable) = c("NG1G2", "G3", "G4", "Death")

groupcolors = colorRampPalette(c("lightgrey", "red"))(6)

Gvec = kronecker(matrix(AERiskTable, nrow=1), matrix(nnt-1))

str(Gvec)
Gcum = apply(Gvec, 1, cumsum)
str(Gcum)


NG1G2vec = AERiskTable[1]*(nnt-1)
G3vec = AERiskTable[2]*(nnt-1)
G4vec = AERiskTable[3]*nnt
Dvec = AERiskTable[4]*nnt
lines(x = RSvector, y = NG1G2vec, col = groupcolors[3])
lines(x = RSvector, y = G3cum <- G3vec + NG1G2vec, col = groupcolors[4])
lines(x = RSvector, y = G4cum <- G4vec + G3vec + NG1G2vec, col = groupcolors[5])
lines(x = RSvector, y = G5cum <- Dvec + G4vec + G3vec + NG1G2vec, col = groupcolors[6])

xdom = c(1:50, 50:1)
polygon(xdom, c(rep(1, 50), NG1G2vec[50:1]), col = groupcolors[3])
polygon(xdom, c(G3cum[1:50], NG1G2vec[50:1]), col = groupcolors[4])
polygon(xdom, c(G4cum[1:50], G3cum[50:1]), col = groupcolors[5])
polygon(xdom, c(G5cum[1:50], G4cum[50:1]), col = groupcolors[6])



rect(0, 0, 50, 1, col = 'black')
rect(xleft = 12.5, ybottom = 1, xright = 13.5, ytop = a <- AERiskTable[1] * (nnt[argmin(v = RSvector, target = 13)]-1), col = 'blue')
rect(xleft = 12.5, ybottom = a, xright = 13.5, ytop = b <- (a + AERiskTable[2] * (nnt[argmin(v = RSvector, target = 13)]-1)), col = 'green')
rect(xleft = 12.5, ybottom = b, xright = 13.5, ytop = c <- (b + AERiskTable[3] * (nnt[argmin(v = RSvector, target = 13)]-1)), col = 'yellow')
rect(xleft = 12.5, ybottom = c, xright = 13.5, ytop = d <- (c + AERiskTable[4] * (nnt[argmin(v = RSvector, target = 13)]-1)), col = 'red')
### Remember to remove the helped patient!  NNT-1.