# -------- Obtain the Control Statistic-------------------
set.seed(123)
cntrlstat <- function(n, alp, bet)
{
  x <- rweibull(n, shape = alp, scale = bet)
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
alp <- 0.5 
bet <- 1

inputswei <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
emwei16 <- mapply(FUN = cntrlstat, inputswei$n, inputswei$alp, inputswei$bet)

tremwei16 <- t(emwei16)
emswei16 <- tremwei16[, 1]
emwwei16 <- tremwei16[, 2]


#-------- Compute control limits n=5--------------
alpha <- 0.005
cswei16 <-  as.vector(quantile(emswei16, c(alpha, 1-alpha)))
cwwei16 <- as.vector(quantile(emwwei16, c(alpha, 1- alpha)))


#--------Assign control limits n=5-----------

l1 <- cswei16[1]
u1 <- cswei16[2]
l2 <- cwwei16[1]
u2 <- cwwei16[2]

#-------------- Run length distribution n=5--------------------------------
set.seed(123)
nsim1<-50000
alp1<-c(0.2,0.3,0.35,0.4,0.45,0.5,0.51,0.52,0.55)
bet1 <-c(1,1.2,1.5,1.75,2,2.5,3,3.5,4)
n1<-5

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1wei16<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputswei16<-mapply(FUN = sgnl, inputs1wei16$n1,inputs1wei16$alp1,inputs1wei16$bet1,inputs1wei16$u1,inputs1wei16$l1,inputs1wei16$u2,inputs1wei16$l2)
resultswei16<-na.omit(cbind(inputs1wei16,t(outputswei16)))

#-------------------ARL, SDRL for n=5----------------------------
arls<- by(resultswei16[,9], INDICES=resultswei16[,3:4], FUN=mean,  simplify=TRUE)
arlw<- by(resultswei16[,10], INDICES=resultswei16[,3:4], FUN=mean,  simplify=TRUE)
sdrls<- by(resultswei16[,9], INDICES=resultswei16[,3:4], FUN=sd,  simplify=TRUE)
sdrlw<- by(resultswei16[,10], INDICES=resultswei16[,3:4], FUN=sd,  simplify=TRUE)

arlwei16<-cbind(arls,sdrls,arlw,sdrlw)
arlwei16<-round(arlwei16,2)
saveRDS(arlwei16,file="arlwei16.RDs")
readRDS("arlwei16.RDs")


#---------------- n=10 -------------------------------
#------------Obtain the Empirical Distribution n=10---------------------

nsim <-1e6
n <- 10
alp <- 0.5 
bet <- 1

inputswei <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
emwei17 <- mapply(FUN = cntrlstat, inputswei$n, inputswei$alp, inputswei$bet)

tremwei17 <- t(emwei17)
emswei17 <- tremwei17[, 1]
emwwei17 <- tremwei17[, 2]

#-------- Compute control limits n=10--------------
alpha <- 0.005
cswei17 <-  as.vector(quantile(emswei17, c(alpha, 1-alpha)))
cwwei17 <- as.vector(quantile(emwwei17, c(alpha, 1- alpha)))

#--------Assign control limits n=10-----------

l1 <- cswei17[1]
u1 <- cswei17[2]
l2 <- cwwei17[1]
u2 <- cwwei17[2]

#-------------- Run length distribution n=10 --------------------------------

set.seed(123)
nsim1<-50000
alp1<-0.5
bet1 <-c(1,1.2,1.5,1.75,2,2.5,3,3.5,4)
n1<-10

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1wei17<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputswei17<-mapply(FUN = sgnl, inputs1wei17$n1,inputs1wei17$alp1,inputs1wei17$bet1,inputs1wei17$u1,inputs1wei17$l1,inputs1wei17$u2,inputs1wei17$l2)
resultswei17<-na.omit(cbind(inputs1wei17,t(outputswei17)))

#-------------------ARL, SDRL for n=10----------------------------

arls<- by(resultswei17[,9], INDICES=resultswei17[,3:4], FUN=mean,  simplify=TRUE)
arlw<- by(resultswei17[,10], INDICES=resultswei17[,3:4], FUN=mean,  simplify=TRUE)
sdrls<- by(resultswei17[,9], INDICES=resultswei17[,3:4], FUN=sd,  simplify=TRUE)
sdrlw<- by(resultswei17[,10], INDICES=resultswei17[,3:4], FUN=sd,  simplify=TRUE)

arlwei17<-cbind(arls,sdrls,arlw,sdrlw)
arlwei17<-round(arlwei17,2)
saveRDS(arlwei17,file="arlwei17.RDs")
readRDS("arlwei17.RDs")



#-------------------------n=20 ------------------
#------------Obtain the Empirical Distribution n=20---------------------
nsim <-1e6
n <- 20
alp <- 0.5 
bet <- 1

inputswei <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
emwei18 <- mapply(FUN = cntrlstat, inputswei$n, inputswei$alp, inputswei$bet)

tremwei18 <- t(emwei18)
emswei18 <- tremwei18[, 1]
emwwei18 <- tremwei18[, 2]


#-------- Compute control limits n=20--------------

alpha <- 0.005
cswei18 <-  as.vector(quantile(emswei18, c(alpha, 1-alpha)))
cwwei18 <- as.vector(quantile(emwwei18, c(alpha, 1- alpha)))

#--------Assign control limits n=20-----------

l1 <- cswei18[1]
u1 <- cswei18[2]
l2 <- cwwei18[1]
u2 <- cwwei18[2]

#-------------- Run length distribution--------------------------------

set.seed(123)
nsim1<-50000
alp1<-0.5
bet1 <-c(1,1.2,1.5,1.75,2,2.5,3,3.5,4)
n1<-20

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1wei18<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputswei18<-mapply(FUN = sgnl, inputs1wei18$n1,inputs1wei18$alp1,inputs1wei18$bet1,inputs1wei18$u1,inputs1wei18$l1,inputs1wei18$u2,inputs1wei18$l2)
resultswei18<-na.omit(cbind(inputs1wei18,t(outputswei18)))


#-------------------ARL, SDRL for n=20----------------------------

arls<- by(resultswei18[,9], INDICES=resultswei18[,3:4], FUN=mean,  simplify=TRUE)
arlw<- by(resultswei18[,10], INDICES=resultswei18[,3:4], FUN=mean,  simplify=TRUE)
sdrls<- by(resultswei18[,9], INDICES=resultswei18[,3:4], FUN=sd,  simplify=TRUE)
sdrlw<- by(resultswei18[,10], INDICES=resultswei18[,3:4], FUN=sd,  simplify=TRUE)

arlwei18<-cbind(arls,sdrls,arlw,sdrlw)
arlwei18<-round(arlwei18,2)
saveRDS(arlwei18,file="arlwei18.RDs")
readRDS("arlwei18.RDs")