library(R2jags)

# wfso <- read.csv("wfso.csv")

y <- wfso$First_Side_Out

wfso$z1 <- 0
wfso$z1 <- ifelse(-4.5 < wfso$nx3 & wfso$nx3 < -1.5 & 4.5 < wfso$ny3 & wfso$ny3 < 9,1,0)
r1 <- -4.5 < wfso$nx3 & wfso$nx3 < -1.5 & 4.5 < wfso$ny3 & wfso$ny3 < 9

wfso$z2 <- 0
wfso$z2 <- ifelse(-4.5 < wfso$nx3 & wfso$nx3 < -1.5 & 0 < wfso$ny3 & wfso$ny3 < 4.5,1,0)
r2 <- -4.5 < wfso$nx3 & wfso$nx3 < -1.5 & 0 < wfso$ny3 & wfso$ny3 < 4.5 

wfso$z6 <- 0
wfso$z6 <- ifelse(-1.5 < wfso$nx3 & wfso$nx3 < 1.5 & 4.5 < wfso$ny3 & wfso$ny3 < 9,1,0)
r6 <- -1.5 < wfso$nx3 & wfso$nx3 < 1.5 & 4.5 < wfso$ny3 & wfso$ny3 < 9

wfso$z3 <- 0
wfso$z3 <- ifelse(-1.5 < wfso$nx3 & wfso$nx3 < 1.5 & 0 < wfso$ny3 & wfso$ny3 < 4.5,1,0)
r3 <- -1.5 < wfso$nx3 & wfso$nx3 < 1.5 & 0 < wfso$ny3 & wfso$ny3 < 4.5

wfso$z5 <- 0
wfso$z5 <- ifelse(1.5 < wfso$nx3 & wfso$nx3 < 4.5 & 4.5 < wfso$ny3 & wfso$ny3 < 9,1,0)
r5 <- 1.5 < wfso$nx3 & wfso$nx3 < 4.5 & 4.5 < wfso$ny3 & wfso$ny3 < 9

wfso$z4 <- 0
wfso$z4 <- ifelse(1.5 < wfso$nx3 & wfso$nx3 < 4.5 & 0 < wfso$ny3 & wfso$ny3 < 4.5,1,0)
r4 <- 1.5 < wfso$nx3 & wfso$nx3 < 4.5 & 0 < wfso$ny3 & wfso$ny3 < 4.5



area <- ifelse( r1 == T,1,
                ifelse(r2 == T,2,
                       ifelse(r3 == T,3,
                              ifelse(r4 == T,4,
                                     ifelse(r5 == T,5,
                                            ifelse(r6 == T,6,0))))))

y = y[area != 0]

area <- area[area != 0]

n <- length(area)





mdl <- " model{
for(i in 1:n){

y[i] ~ dbern(p[area[i]]) 


}

for(i in 1:6) {

p[i] ~ dbeta(1,1)

}



}"


writeLines(mdl,'logreg.txt')



data.jags <- c('y','area','n')
parms <- c('p')
logreg.sim <- jags(data=data.jags,inits=NULL,parameters.to.save=parms,
                   model.file="logreg.txt",
                   n.iter=100000,n.burn=5000,n.chains=1,n.thin=1)


log.sim <- as.mcmc(logreg.sim)
chains <- as.matrix(log.sim)
head(chains)


p1 <- chains[,2]
p2 <- chains[,3]
p3 <- chains[,4]
p4 <- chains[,5]
p5 <- chains[,6]
p6 <- chains[,7]

quantile(p1-p2,c(.025,.975))
quantile(p1-p3,c(.025,.975))
quantile(p1-p4,c(.025,.975))
quantile(p1-p5,c(.025,.975))
quantile(p1-p6,c(.025,.975))

quantile(p2-p3,c(.025,.975))
quantile(p2-p4,c(.025,.975))
quantile(p2-p5,c(.025,.975))
quantile(p2-p6,c(.025,.975))

quantile(p3-p4,c(.025,.975))
quantile(p3-p5,c(.025,.975))
quantile(p3-p6,c(.025,.975))

quantile(p4-p5,c(.025,.975))
quantile(p4-p6,c(.025,.975))

quantile(p5-p6,c(.025,.975))


mean(p1-p2 > 0)
mean(p1-p3 > 0)
mean(p1-p4 > 0)
mean(p1-p5 > 0)
mean(p1-p6 > 0)

mean(p2-p3 > 0)
mean(p2-p4 > 0)
mean(p2-p5 > 0)
mean(p2-p6 > 0)

mean(p3-p4 > 0)
mean(p3-p5 > 0)
mean(p3-p6 > 0)

mean(p4-p5 > 0)
mean(p4-p6 > 0)

mean(p5-p6 > 0)



quantile(p1,c(.025,.5,.975))
quantile(p2,c(.025,.5,.975))
quantile(p3,c(.025,.5,.975))
quantile(p4,c(.025,.5,.975))
quantile(p5,c(.025,.5,.975))
quantile(p6,c(.025,.5,.975))







