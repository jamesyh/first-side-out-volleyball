library(R2jags)

OHFso <- read.csv("OHFso.csv")


y <- OHFso$First_Side_Out


OHFso$z1 <- ifelse(-4.5 < OHFso$nx3 & OHFso$nx3 < -1.5 & 4.5 < OHFso$ny3 & OHFso$ny3 < 9,1,0)
r1 <- -4.5 < OHFso$nx3 & OHFso$nx3 < -1.5 & 4.5 < OHFso$ny3 & OHFso$ny3 < 9

OHFso$z2 <- ifelse(-4.5 < OHFso$nx3 & OHFso$nx3 < -1.5 & 0 < OHFso$ny3 & OHFso$ny3 < 4.5,1,0)
r2 <- -4.5 < OHFso$nx3 & OHFso$nx3 < -1.5 & 0 < OHFso$ny3 & OHFso$ny3 < 4.5 

OHFso$z6 <- ifelse(-1.5 < OHFso$nx3 & OHFso$nx3 < 1.5 & 4.5 < OHFso$ny3 & OHFso$ny3 < 9,1,0)
r6 <- -1.5 < OHFso$nx3 & OHFso$nx3 < 1.5 & 4.5 < OHFso$ny3 & OHFso$ny3 < 9

OHFso$z3 <- ifelse(-1.5 < OHFso$nx3 & OHFso$nx3 < 1.5 & 0 < OHFso$ny3 & OHFso$ny3 < 4.5,1,0)
r3 <- -1.5 < OHFso$nx3 & OHFso$nx3 < 1.5 & 0 < OHFso$ny3 & OHFso$ny3 < 4.5

OHFso$z5 <- ifelse(1.5 < OHFso$nx3 & OHFso$nx3 < 4.5 & 4.5 < OHFso$ny3 & OHFso$ny3 < 9,1,0)
r5 <- 1.5 < OHFso$nx3 & OHFso$nx3 < 4.5 & 4.5 < OHFso$ny3 & OHFso$ny3 < 9

OHFso$z4 <- ifelse(1.5 < OHFso$nx3 & OHFso$nx3 < 4.5 & 0 < OHFso$ny3 & OHFso$ny3 < 4.5,1,0)
r4 <- 1.5 < OHFso$nx3 & OHFso$nx3 < 4.5 & 0 < OHFso$ny3 & OHFso$ny3 < 4.5



area <- ifelse( r1 == T,1,
                ifelse(r2 == T,2,
                       ifelse(r3 == T,3,
                              ifelse(r4 == T,4,
                                     ifelse(r5 == T,5,
                                            ifelse(r6 == T,6,0))))))

same <- OHFso$same

same <- same[area != 0]

y = y[area != 0]

area <- area[area != 0]

n = length(area)

mdl <- " model{
for(i in 1:n){

y[i] ~ dbinom(p[i],1) 

logit(p[i]) <- b0[area[i]] + b1*same[i]


}

for(i in 1:6) {

b0[i] ~ dnorm(0,1)

}


b1 ~ dnorm(0,1)


}"


writeLines(mdl,'logreg.txt')


data.jags <- c('y','area','same','n')
parms <- c('b0','b1')
logreg.sim <- jags(data=data.jags,inits=NULL,parameters.to.save=parms,
                   model.file="logreg.txt",
                   n.iter=100000,n.burn=5000,n.chains=1,n.thin=1)


log.sim <- as.mcmc(logreg.sim)
chains <- as.matrix(log.sim)
head(chains)
write.csv(chains,"chainsSame.csv")

