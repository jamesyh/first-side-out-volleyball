OH <- 1
wfso <- read.csv("wfso.csv")
wfso <- wfso[wfso$posHitter %in% OH,]

wfso <- wfso[wfso$same == 0,]

x = wfso$x3
y = wfso$y3

y = y - 9
x = x - 4.5

ux = x[y > 0 ]
uy = y[y > 0 ]

lx = x[y < 0 ]
ly = y[y < 0 ]

lx = lx*-1
ly = ly*-1

wfso$nx3 <-  c(ux,lx)
wfso$ny3 <- c(uy,ly)

wfso$z1 <- ifelse((-4.5 < wfso$nx3 & wfso$nx3 < -1.5 & 4.5 < wfso$ny3 & wfso$ny3 < 9),1,0)
r1 <- -4.5 < wfso$nx3 & wfso$nx3 < -1.5 & 4.5 < wfso$ny3 & wfso$ny3 < 9 &
  wfso$First_Side_Out == 1

wfso$z2 <- ifelse(-4.5 < wfso$nx3 & wfso$nx3 < -1.5 & 0 < wfso$ny3 & wfso$ny3 < 4.5,1,0)
r2 <- -4.5 < wfso$nx3 & wfso$nx3 < -1.5 & 0 < wfso$ny3 & wfso$ny3 < 4.5 &
  wfso$First_Side_Out == 1

wfso$z6 <- ifelse(-1.5 < wfso$nx3 & wfso$nx3 < 1.5 & 4.5 < wfso$ny3 & wfso$ny3 < 9,1,0)
r6 <- -1.5 < wfso$nx3 & wfso$nx3 < 1.5 & 4.5 < wfso$ny3 & wfso$ny3 < 9 &
  wfso$First_Side_Out == 1

wfso$z3 <- ifelse(-1.5 < wfso$nx3 & wfso$nx3 < 1.5 & 0 < wfso$ny3 & wfso$ny3 < 4.5,1,0)
r3 <- -1.5 < wfso$nx3 & wfso$nx3 < 1.5 & 0 < wfso$ny3 & wfso$ny3 < 4.5&
  wfso$First_Side_Out == 1

wfso$z5 <- ifelse(1.5 < wfso$nx3 & wfso$nx3 < 4.5 & 4.5 < wfso$ny3 & wfso$ny3 < 9,1,0)
r5 <- 1.5 < wfso$nx3 & wfso$nx3 < 4.5 & 4.5 < wfso$ny3 & wfso$ny3 < 9&
  wfso$First_Side_Out == 1

wfso$z4 <- ifelse(1.5 < wfso$nx3 & wfso$nx3 < 4.5 & 0 < wfso$ny3 & wfso$ny3 < 4.5,1,0)
r4 <- 1.5 < wfso$nx3 & wfso$nx3 < 4.5 & 0 < wfso$ny3 & wfso$ny3 < 4.5&
  wfso$First_Side_Out == 1

b = -4.5 < wfso$nx3 & wfso$nx3 < -1.5 & 4.5 < wfso$ny3 & wfso$ny3


x <- wfso$z1 | wfso$z2 | wfso$z3 | wfso$z4 | wfso$z5 | wfso$z6

r <- !(r1 | r2 | r3 | r4 | r5 | r6  )

r <- r & x

plot(wfso$nx3[r],wfso$ny3[r],col = 'lavenderblush3',pch = 20,
     xlab="Net",ylab="", xaxt='n', yaxt='n', mgp=c(0,0,0),xlim=c(-4.2,4.2),
     main = "FSO with no SPH by Area",
     ylim = c(.4,9))

# abline(h = 9)
abline(h= 0 )
abline(h= 4.5 )


# abline(v = -4.5)
# abline(v = 4.5)
abline(v = 1.5)
abline(v = -1.5)

points(wfso$nx3[r1],wfso$ny3[r1],col = adjustcolor( "red", alpha.f = .6) )
points(wfso$nx3[r2],wfso$ny3[r2],col = adjustcolor( "blue", alpha.f = .6))
points(wfso$nx3[r3],wfso$ny3[r3],col = adjustcolor( "green", alpha.f = .6))
points(wfso$nx3[r4],wfso$ny3[r4],col = adjustcolor( "purple", alpha.f = .6))
points(wfso$nx3[r5],wfso$ny3[r5],col = adjustcolor( "orange", alpha.f = .6))
points(wfso$nx3[r6],wfso$ny3[r6],col = adjustcolor( "yellow", alpha.f = .6))


ci1 <- "(0.437,0.462)"
ci2 <- "(0.408,0.522)"
ci3 <- "(0.436,0.545)"
ci4 <- "(0.415,0.509)"
ci5 <- "(0.430,0.453)"
ci6 <- "(0.438,0.457)"

color = "black"
text(-3,6.5,ci1,col = color,font = 2,cex = 1.7)
text(-3,2.3,ci2,col = color,font = 2,cex = 1.7)
text(0,2.3,ci3,col = color,font = 2,cex = 1.7)
text(3,2.3,ci4,col = color,font = 2,cex = 1.7)
text(3,6.5,ci5,col = color,font = 2,cex = 1.7)
text(0,6.5,ci6,col = color,font = 2,cex = 1.7)
