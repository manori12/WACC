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
alp <- 1
bet <- 1.6

inputswei <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
emwei6 <- mapply(FUN = cntrlstat, inputswei$n, inputswei$alp, inputswei$bet)

tremwei6 <- t(emwei6)
emswei6 <- tremwei6[, 1]
emwwei6 <- tremwei6[, 2]


#-------- Compute control limits n=5--------------
alpha <- 0.005
cswei6 <-  as.vector(quantile(emswei6, c(alpha, 1-alpha)))
cwwei6 <- as.vector(quantile(emwwei6, c(alpha, 1- alpha)))


#--------Assign control limits n=5-----------

l1 <- cswei6[1]
u1 <- cswei6[2]
l2 <- cwwei6[1]
u2 <- cwwei6[2]

#-------------- Run length distribution n=5--------------------------------
set.seed(123)
nsim1<-50000
alp1<-1
bet1 <-c(1.6,1.75,2,2.25,3,3.5,4)
n1<-5

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1wei4<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputswei4<-mapply(FUN = sgnl, inputs1wei4$n1,inputs1wei4$alp1,inputs1wei4$bet1,inputs1wei4$u1,inputs1wei4$l1,inputs1wei4$u2,inputs1wei4$l2)
resultswei4<-na.omit(cbind(inputs1wei4,t(outputswei4)))

#-------------------ARL, SDRL for n=5----------------------------
arls<- by(resultswei4[,9], INDICES=resultswei4[,4], FUN=mean,  simplify=TRUE)
arlw<- by(resultswei4[,10], INDICES=resultswei4[,4], FUN=mean,  simplify=TRUE)
sdrls<- by(resultswei4[,9], INDICES=resultswei4[,4], FUN=sd,  simplify=TRUE)
sdrlw<- by(resultswei4[,10], INDICES=resultswei4[,4], FUN=sd,  simplify=TRUE)

arlwei4<-cbind(arls,sdrls,arlw,sdrlw)
arlwei4<-round(arlwei4,2)
saveRDS(arlwei4,file="arlwei4.RDs")
readRDS("arlwei4.RDs")


#---------------- n=10 -------------------------------
#------------Obtain the Empirical Distribution n=10---------------------
set.seed(123)
nsim <-1e6
n <- 10
alp <- 1
bet <- 1.6

inputswei <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
emwei5 <- mapply(FUN = cntrlstat, inputswei$n, inputswei$alp, inputswei$bet)

tremwei5 <- t(emwei5)
emswei5 <- tremwei5[, 1]
emwwei5 <- tremwei5[, 2]


#-------- Compute control limits n=10--------------
alpha <- 0.005

cswei5 <-  as.vector(quantile(emswei5, c(alpha, 1-alpha)))
cwwei5 <- as.vector(quantile(emwwei5, c(alpha, 1- alpha)))

#--------Assign control limits n=10-----------

l1 <- cswei5[1]
u1 <- cswei5[2]
l2 <- cwwei5[1]
u2 <- cwwei5[2]


#-------------- Run length distribution n=10 --------------------------------
set.seed(123)
nsim1<-50000
alp1<-1
bet1 <-c(1.6,1.75,2,2.25,4)
n1<-10


arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1wei5<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputswei5<-mapply(FUN = sgnl, inputs1wei5$n1,inputs1wei5$alp1,inputs1wei5$bet1,inputs1wei5$u1,inputs1wei5$l1,inputs1wei5$u2,inputs1wei5$l2)

resultswei5<-na.omit(cbind(inputs1wei5,t(outputswei5)))


#-------------------ARL, SDRL for n=10----------------------------
arls <- by(resultswei5[,9], INDICES=resultswei5[,4], FUN=mean,  simplify=TRUE)
arlw <- by(resultswei5[,10], INDICES=resultswei5[,4], FUN=mean,  simplify=TRUE)
sdrls <- by(resultswei5[,9], INDICES=resultswei5[,4], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultswei5[,10], INDICES=resultswei5[,4], FUN=sd,  simplify=TRUE)

arlwei5<-cbind(arls,sdrls,arlw,sdrlw)
arlwei5<-round(arlwei5,2)
saveRDS(arlwei5,file="arlwei5.RDs")
readRDS("arlwei5.RDs")



#-------------------------n=20 ------------------
#------------Obtain the Empirical Distribution n=20---------------------
set.seed(123)
nsim <-1e6
n <- 20
alp <- 1
bet <- 1.6

inputswei <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
emwei6 <- mapply(FUN = cntrlstat, inputswei$n, inputswei$alp, inputswei$bet)

tremwei6 <- t(emwei6)
emswei6 <- tremwei6[, 1]
emwwei6 <- tremwei6[, 2]


#-------- Compute control limits n=20--------------
alpha <- 0.005
cswei6<-  as.vector(quantile(emswei6, c(alpha, 1-alpha)))
cwwei6 <- as.vector(quantile(emwwei6, c(alpha, 1- alpha)))

#--------Assign control limits n=20-----------
l1 <- cswei6[1]
u1 <- cswei6[2]
l2 <- cwwei6[1]
u2 <- cwwei6[2]



#-------------- Run length distribution--------------------------------
set.seed(123)
nsim1<-50000
alp1<-1
bet1 <-c(1.6,1.75,2,2.25,4)
n1<-20

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1wei6<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputswei6<-mapply(FUN = sgnl, inputs1wei6$n1,inputs1wei6$alp1,inputs1wei6$bet1,inputs1wei6$u1,inputs1wei6$l1,inputs1wei6$u2,inputs1wei6$l2)

resultswei6<-na.omit(cbind(inputs1wei6,t(outputswei6)))

#-------------------ARL, SDRL for n=20----------------------------

arls <- by(resultswei6[,9], INDICES=resultswei6[,4], FUN=mean,  simplify=TRUE)
arlw <- by(resultswei6[,10], INDICES=resultswei6[,4], FUN=mean,  simplify=TRUE)
sdrls <- by(resultswei6[,9], INDICES=resultswei6[,4], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultswei6[,10], INDICES=resultswei6[,4], FUN=sd,  simplify=TRUE)

arlwei6<-cbind(arls,sdrls,arlw,sdrlw)
arlwei6<-round(arlwei6,2)
saveRDS(arlwei6,file="arlwei6.RDs")
readRDS("arlwei6.RDs")


