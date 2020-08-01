# -------- Obtain the Control Statistic-------------------
set.seed(123)
cntrlstat <- function(n, alp, bet)
{
  x <- rgamma(n, shape = alp, scale = bet)
  refr <- max(x) # Set minimum, maximum,  mean, median etc as the reference point
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
alp <- 1.1
bet <- 1.5

inputsg <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
emg10 <- mapply(FUN = cntrlstat, inputsg$n, inputsg$alp, inputsg$bet)

tremg10 <- t(emg10)
emsg10 <- tremg10[, 1]
emwg10 <- tremg10[, 2]


#-------- Compute control limits n=5--------------
alpha <- 0.005
csg10 <-  as.vector(quantile(emsg10, c(alpha, 1-alpha)))
cwg10 <- as.vector(quantile(emwg10, c(alpha, 1- alpha)))


#--------Assign control limits n=5-----------

l1 <- csg10[1]
u1 <- csg10[2]
l2 <- cwg10[1]
u2 <- cwg10[2]

#-------------- Run length distribution n=5--------------------------------
set.seed(123)
nsim1<-50000
alp1<-c(0.3,0.4,0.5,0.7,0.9,1.1,1.3,1.5,1.75,2,2.3,3.0,4.9)
bet1<-c(1.5,1.75,2,2.25,2.5,3,3.5)
n1<-5

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1g10<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputsg10<-mapply(FUN = sgnl, inputs1g10$n1,inputs1g10$alp1,inputs1g10$bet1,inputs1g10$u1,inputs1g10$l1,inputs1g10$u2,inputs1g10$l2)
resultsg10<-na.omit(cbind(inputs1g10,t(outputsg10)))

#-------------------ARL, SDRL for n=5----------------------------
arls<- by(resultsg10[,9], INDICES=resultsg10[,3:4], FUN=mean,  simplify=TRUE)
arlw<- by(resultsg10[,10], INDICES=resultsg10[,3:4], FUN=mean,  simplify=TRUE)
sdrls<- by(resultsg10[,9], INDICES=resultsg10[,3:4], FUN=sd,  simplify=TRUE)
sdrlw<- by(resultsg10[,10], INDICES=resultsg10[,3:4], FUN=sd,  simplify=TRUE)

arlg10<-cbind(arls,sdrls,arlw,sdrlw)
arlg10<-round(arlg10,2)
saveRDS(arlg10,file="arlg10.RDs")
readRDS("arlg10.RDs")


#---------------- n=10 -------------------------------
#------------Obtain the Empirical Distribution n=10---------------------
set.seed(123)
nsim <-1e6
n <- 10
alp <- 1.1
bet <- 1.5

inputsg <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
emg11 <- mapply(FUN = cntrlstat, inputsg$n, inputsg$alp, inputsg$bet)

tremg11 <- t(emg11)
emsg11 <- tremg11[, 1]
emwg11 <- tremg11[, 2]


#-------- Compute control limits n=10--------------
alpha <- 0.005

csg11 <-  as.vector(quantile(emsg11, c(alpha, 1-alpha)))
cwg11 <- as.vector(quantile(emwg11, c(alpha, 1- alpha)))

#--------Assign control limits n=10-----------

l1 <- csg11[1]
u1 <- csg11[2]
l2 <- cwg11[1]
u2 <- cwg11[2]


#-------------- Run length distribution n=10 --------------------------------
set.seed(123)
nsim1<-50000
alp1<-c(0.3,0.4,0.5,0.7,0.9,1.1,1.3,1.5,1.75,2,2.3,3.0,4.9)
bet1<-c(1.5,1.75,2,2.25,2.5,3,3.5)
n1<-10


arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1g11<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputsg11<-mapply(FUN = sgnl, inputs1g11$n1,inputs1g11$alp1,inputs1g11$bet1,inputs1g11$u1,inputs1g11$l1,inputs1g11$u2,inputs1g11$l2)

resultsg11<-na.omit(cbind(inputs1g11,t(outputsg11)))


#-------------------ARL, SDRL for n=10----------------------------
arls <- by(resultsg11[,9], INDICES=resultsg11[,3:4], FUN=mean,  simplify=TRUE)
arlw <- by(resultsg11[,10], INDICES=resultsg11[,3:4], FUN=mean,  simplify=TRUE)
sdrls <- by(resultsg11[,9], INDICES=resultsg11[,3:4], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultsg11[,10], INDICES=resultsg11[,3:4], FUN=sd,  simplify=TRUE)

arlg11<-cbind(arls,sdrls,arlw,sdrlw)
arlg11<-round(arlg11,2)
saveRDS(arlg11,file="arlg11.RDs")
readRDS("arlg11.RDs")



#-------------------------n=20 ------------------
#------------Obtain the Empirical Distribution n=20---------------------
set.seed(123)
nsim <-1e6
n <- 20
alp <- 1.1
bet <- 1.5

inputsg <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
emg12 <- mapply(FUN = cntrlstat, inputsg$n, inputsg$alp, inputsg$bet)

tremg12 <- t(emg12)
emsg12 <- tremg12[, 1]
emwg12 <- tremg12[, 2]


#-------- Compute control limits n=20--------------
alpha <- 0.005
csg12<-  as.vector(quantile(emsg12, c(alpha, 1-alpha)))
cwg12 <- as.vector(quantile(emwg12, c(alpha, 1- alpha)))

#--------Assign control limits n=20-----------
l1 <- csg12[1]
u1 <- csg12[2]
l2 <- cwg12[1]
u2 <- cwg12[2]



#-------------- Run length distribution--------------------------------
set.seed(123)
nsim1<-50000
alp1<-c(0.3,0.4,0.5,0.7,0.9,1.1,1.3,1.5,1.75,2,2.3,3.0,4.9)
bet1<-c(1.5,1.75,2,2.25,2.5,3,3.5)
n1<-20

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1g12<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputsg12<-mapply(FUN = sgnl, inputs1g12$n1,inputs1g12$alp1,inputs1g12$bet1,inputs1g12$u1,inputs1g12$l1,inputs1g12$u2,inputs1g12$l2)

resultsg12<-na.omit(cbind(inputs1g12,t(outputsg12)))

#-------------------ARL, SDRL for n=20----------------------------

arls <- by(resultsg12[,9], INDICES=resultsg12[,3:4], FUN=mean,  simplify=TRUE)
arlw <- by(resultsg12[,10], INDICES=resultsg12[,3:4], FUN=mean,  simplify=TRUE)
sdrls <- by(resultsg12[,9], INDICES=resultsg12[,3:4], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultsg12[,10], INDICES=resultsg12[,3:4], FUN=sd,  simplify=TRUE)

arlg12<-cbind(arls,sdrls,arlw,sdrlw)
arlg12<-round(arlg12,2)
saveRDS(arlg12,file="arlg12.RDs")
readRDS("arlg12.RDs")


