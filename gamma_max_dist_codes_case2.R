# -------- Obtain the Control Statistic-------------------
set.seed(123)
cntrlstat <- function(n, alp, bet)
{
  x <- rgamma(n, shape = alp, scale = bet)
  refr <- max(x) 
  w <- abs(x - refr) / sum(abs(x - refr))
  y <- w * x
  ybar <- mean(y)
  xbar <- mean(x)
  return(cbind(xbar, ybar))
}

#-------------Function Signal------------------------

sgnl<-function(n,alp, bet,u1,l1,u2,l2)
{
  x.RL<-0
  y.RL<-0
  mn<-cntrlstat(n=n,alp=alp,bet=bet)
  while( l1 < mn[1]  && mn[1] < u1 )
  {
    mn<-cntrlstat(n=n,alp=alp,bet=bet)
    x.RL<-x.RL+1
  }
  mn<-cntrlstat(n=n,alp=alp,bet=bet)
  while( l2 < mn[2]  && mn[2] < u2 )
  {
    mn<-cntrlstat(n=n,alp=alp,bet=bet)
    y.RL<-y.RL+1
  }
  return(cbind(x.RL,y.RL))
}  

#-------n=5----------------

#------------Obtain the Empirical Distribution n=5---------------------

nsim <-1e6
n <- 5
alp <- 0.5
bet <- 1

inputsg <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
emg16 <- mapply(FUN = cntrlstat, inputsg$n, inputsg$alp, inputsg$bet)

tremg16 <- t(emg16)
emsg16 <- tremg16[, 1]
emwg16 <- tremg16[, 2]


#-------- Compute control limits n=5--------------
alpha <- 0.005
csg16 <-  as.vector(quantile(emsg16, c(alpha, 1-alpha)))
cwg16 <- as.vector(quantile(emwg16, c(alpha, 1- alpha)))


#--------Assign control limits n=5-----------

l1 <- csg16[1]
u1 <- csg16[2]
l2 <- cwg16[1]
u2 <- cwg16[2]

#-------------- Run length distribution n=5--------------------------------
set.seed(123)
nsim1<-50000
alp1<-c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,1,1.25,1.5)
bet1 <-c(1,1.25,1.5,1.75,2,2.5)
n1<-5

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1g16<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputsg16<-mapply(FUN = sgnl, inputs1g16$n1,inputs1g16$alp1,inputs1g16$bet1,inputs1g16$u1,inputs1g16$l1,inputs1g16$u2,inputs1g16$l2)
resultsg16<-na.omit(cbind(inputs1g16,t(outputsg16)))

#-------------------ARL, SDRL for n=5----------------------------
arls<- by(resultsg16[,9], INDICES=resultsg16[,3:4], FUN=mean,  simplify=TRUE)
arlw<- by(resultsg16[,10], INDICES=resultsg16[,3:4], FUN=mean,  simplify=TRUE)
sdrls<- by(resultsg16[,9], INDICES=resultsg16[,3:4], FUN=sd,  simplify=TRUE)
sdrlw<- by(resultsg16[,10], INDICES=resultsg16[,3:4], FUN=sd,  simplify=TRUE)

arlg16<-cbind(arls,sdrls,arlw,sdrlw)
arlg16<-round(arlg16,2)
saveRDS(arlg16,file="arlg16.RDs")
readRDS("arlg16.RDs")


#---------------- n=10 -------------------------------
#------------Obtain the Empirical Distribution n=10---------------------
set.seed(123)
nsim <-1e6
n <- 10
alp <- 0.5
bet <- 1

inputsg <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
emg17 <- mapply(FUN = cntrlstat, inputsg$n, inputsg$alp, inputsg$bet)

tremg17 <- t(emg17)
emsg17 <- tremg17[, 1]
emwg17 <- tremg17[, 2]


#-------- Compute control limits n=10--------------
alpha <- 0.005

csg17 <-  as.vector(quantile(emsg17, c(alpha, 1-alpha)))
cwg17 <- as.vector(quantile(emwg17, c(alpha, 1- alpha)))

#--------Assign control limits n=10-----------

l1 <- csg17[1]
u1 <- csg17[2]
l2 <- cwg17[1]
u2 <- cwg17[2]


#-------------- Run length distribution n=10 --------------------------------
set.seed(123)
nsim1<-50000
alp1<-c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,1,1.25,1.5)
bet1 <-c(1,1.25,1.5,1.75,2,2.5)
n1<-10


arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1g17<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputsg17<-mapply(FUN = sgnl, inputs1g17$n1,inputs1g17$alp1,inputs1g17$bet1,inputs1g17$u1,inputs1g17$l1,inputs1g17$u2,inputs1g17$l2)

resultsg17<-na.omit(cbind(inputs1g17,t(outputsg17)))


#-------------------ARL, SDRL for n=10----------------------------
arls <- by(resultsg17[,9], INDICES=resultsg17[,3:4], FUN=mean,  simplify=TRUE)
arlw <- by(resultsg17[,10], INDICES=resultsg17[,3:4], FUN=mean,  simplify=TRUE)
sdrls <- by(resultsg17[,9], INDICES=resultsg17[,3:4], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultsg17[,10], INDICES=resultsg17[,3:4], FUN=sd,  simplify=TRUE)

arlg17<-cbind(arls,sdrls,arlw,sdrlw)
arlg17<-round(arlg17,2)
saveRDS(arlg17,file="arlg17.RDs")
readRDS("arlg17.RDs")



#-------------------------n=20 ------------------
#------------Obtain the Empirical Distribution n=20---------------------
set.seed(123)
nsim <-1e6
n <- 20
alp <- 0.5
bet <- 1

inputsg <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
emg18 <- mapply(FUN = cntrlstat, inputsg$n, inputsg$alp, inputsg$bet)

tremg18 <- t(emg18)
emsg18 <- tremg18[, 1]
emwg18 <- tremg18[, 2]


#-------- Compute control limits n=20--------------
alpha <- 0.005
csg18<-  as.vector(quantile(emsg18, c(alpha, 1-alpha)))
cwg18 <- as.vector(quantile(emwg18, c(alpha, 1- alpha)))

#--------Assign control limits n=20-----------
l1 <- csg18[1]
u1 <- csg18[2]
l2 <- cwg18[1]
u2 <- cwg18[2]



#-------------- Run length distribution--------------------------------
set.seed(123)
nsim1<-50000
alp1<-c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,1,1.25,1.5)
bet1 <-c(1,1.25,1.5,1.75,2,2.5)
n1<-20

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1g18<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputsg18<-mapply(FUN = sgnl, inputs1g18$n1,inputs1g18$alp1,inputs1g18$bet1,inputs1g18$u1,inputs1g18$l1,inputs1g18$u2,inputs1g18$l2)

resultsg18<-na.omit(cbind(inputs1g18,t(outputsg18)))

#-------------------ARL, SDRL for n=20----------------------------

arls <- by(resultsg18[,9], INDICES=resultsg18[,3:4], FUN=mean,  simplify=TRUE)
arlw <- by(resultsg18[,10], INDICES=resultsg18[,3:4], FUN=mean,  simplify=TRUE)
sdrls <- by(resultsg18[,9], INDICES=resultsg18[,3:4], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultsg18[,10], INDICES=resultsg18[,3:4], FUN=sd,  simplify=TRUE)

arlg18<-cbind(arls,sdrls,arlw,sdrlw)
arlg18<-round(arlg18,2)
saveRDS(arlg18,file="arlg18.RDs")
readRDS("arlg18.RDs")


