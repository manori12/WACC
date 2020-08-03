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
emwei7 <- mapply(FUN = cntrlstat, inputswei$n, inputswei$alp, inputswei$bet)

tremwei7 <- t(emwei7)
emswei7 <- tremwei7[, 1]
emwwei7 <- tremwei7[, 2]


#-------- Compute control limits n=5--------------
alpha <- 0.005
cswei7 <-  as.vector(quantile(emswei7, c(alpha, 1-alpha)))
cwwei7 <- as.vector(quantile(emwwei7, c(alpha, 1- alpha)))


#--------Assign control limits n=5-----------

l1 <- cswei7[1]
u1 <- cswei7[2]
l2 <- cwwei7[1]
u2 <- cwwei7[2]

#-------------- Run length distribution n=5--------------------------------
set.seed(123)
nsim1<-50000
alp1<-c(0.36,0.43,0.48,0.55,0.7,0.8,0.9,1,1.05,1.1,1.15)
bet1 <-c(1.6,1.75,2,2.25,4)
n1<-5

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1wei7<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputswei7<-mapply(FUN = sgnl, inputs1wei7$n1,inputs1wei7$alp1,inputs1wei7$bet1,inputs1wei7$u1,inputs1wei7$l1,inputs1wei7$u2,inputs1wei7$l2)
resultswei7<-na.omit(cbind(inputs1wei7,t(outputswei7)))

#-------------------ARL, SDRL for n=5----------------------------
arls<- by(resultswei7[,9], INDICES=resultswei7[,3:4], FUN=mean,  simplify=TRUE)
arlw<- by(resultswei7[,10], INDICES=resultswei7[,3:4], FUN=mean,  simplify=TRUE)
sdrls<- by(resultswei7[,9], INDICES=resultswei7[,3:4], FUN=sd,  simplify=TRUE)
sdrlw<- by(resultswei7[,10], INDICES=resultswei7[,3:4], FUN=sd,  simplify=TRUE)

arlwei7<-cbind(arls,sdrls,arlw,sdrlw)
arlwei7<-round(arlwei7,2)
saveRDS(arlwei7,file="arlwei7.RDs")
readRDS("arlwei7.RDs")


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
emwei8 <- mapply(FUN = cntrlstat, inputswei$n, inputswei$alp, inputswei$bet)

tremwei8 <- t(emwei8)
emswei8 <- tremwei8[, 1]
emwwei8 <- tremwei8[, 2]


#-------- Compute control limits n=10--------------
alpha <- 0.005

cswei8 <-  as.vector(quantile(emswei8, c(alpha, 1-alpha)))
cwwei8 <- as.vector(quantile(emwwei8, c(alpha, 1- alpha)))

#--------Assign control limits n=10-----------

l1 <- cswei8[1]
u1 <- cswei8[2]
l2 <- cwwei8[1]
u2 <- cwwei8[2]


#-------------- Run length distribution n=10 --------------------------------
set.seed(123)
nsim1<-50000
alp1<-c(0.36,0.43,0.48,0.55,0.7,1,1.05,1.1,1.15)
bet1 <-c(1.6,1.75,2,2.25,4)
n1<-10


arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1wei8<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputswei8<-mapply(FUN = sgnl, inputs1wei8$n1,inputs1wei8$alp1,inputs1wei8$bet1,inputs1wei8$u1,inputs1wei8$l1,inputs1wei8$u2,inputs1wei8$l2)

resultswei8<-na.omit(cbind(inputs1wei8,t(outputswei8)))


#-------------------ARL, SDRL for n=10----------------------------
arls <- by(resultswei8[,9], INDICES=resultswei8[,3:4], FUN=mean,  simplify=TRUE)
arlw <- by(resultswei8[,10], INDICES=resultswei8[,3:4], FUN=mean,  simplify=TRUE)
sdrls <- by(resultswei8[,9], INDICES=resultswei8[,3:4], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultswei8[,10], INDICES=resultswei8[,3:4], FUN=sd,  simplify=TRUE)

arlwei8<-cbind(arls,sdrls,arlw,sdrlw)
arlwei8<-round(arlwei8,2)
saveRDS(arlwei8,file="arlwei8.RDs")
readRDS("arlwei8.RDs")



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
emwei9 <- mapply(FUN = cntrlstat, inputswei$n, inputswei$alp, inputswei$bet)

tremwei9 <- t(emwei9)
emswei9 <- tremwei9[, 1]
emwwei9 <- tremwei9[, 2]


#-------- Compute control limits n=20--------------
alpha <- 0.005
cswei9<-  as.vector(quantile(emswei9, c(alpha, 1-alpha)))
cwwei9 <- as.vector(quantile(emwwei9, c(alpha, 1- alpha)))

#--------Assign control limits n=20-----------
l1 <- cswei9[1]
u1 <- cswei9[2]
l2 <- cwwei9[1]
u2 <- cwwei9[2]



#-------------- Run length distribution--------------------------------
set.seed(123)
nsim1<-50000
alp1<-c(0.36,0.43,0.48,0.55,0.7,1,1.05,1.1,1.15)
bet1 <-c(1.6,1.75,2,2.25,4)
n1<-20

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1wei9<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputswei9<-mapply(FUN = sgnl, inputs1wei9$n1,inputs1wei9$alp1,inputs1wei9$bet1,inputs1wei9$u1,inputs1wei9$l1,inputs1wei9$u2,inputs1wei9$l2)

resultswei9<-na.omit(cbind(inputs1wei9,t(outputswei9)))

#-------------------ARL, SDRL for n=20----------------------------

arls <- by(resultswei9[,9], INDICES=resultswei9[,3:4], FUN=mean,  simplify=TRUE)
arlw <- by(resultswei9[,10], INDICES=resultswei9[,3:4], FUN=mean,  simplify=TRUE)
sdrls <- by(resultswei9[,9], INDICES=resultswei9[,3:4], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultswei9[,10], INDICES=resultswei9[,3:4], FUN=sd,  simplify=TRUE)

arlwei9<-cbind(arls,sdrls,arlw,sdrlw)
arlwei9<-round(arlwei9,2)
saveRDS(arlwei9,file="arlwei9.RDs")
readRDS("arlwei9.RDs")


