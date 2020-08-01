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
emg7 <- mapply(FUN = cntrlstat, inputsg$n, inputsg$alp, inputsg$bet)

tremg7 <- t(emg7)
emsg7 <- tremg7[, 1]
emwg7 <- tremg7[, 2]


#-------- Compute control limits n=5--------------
alpha <- 0.005
csg7 <-  as.vector(quantile(emsg7, c(alpha, 1-alpha)))
cwg7 <- as.vector(quantile(emwg7, c(alpha, 1- alpha)))


#--------Assign control limits n=5-----------

l1 <- csg7[1]
u1 <- csg7[2]
l2 <- cwg7[1]
u2 <- cwg7[2]

#-------------- Run length distribution n=5--------------------------------
set.seed(123)
nsim1<-50000
alp1<-1.1

bet1<-c(1.5,1.75,2,2.25,2.5,3,3.5)
n1<-5

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1g7<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputsg7<-mapply(FUN = sgnl, inputs1g7$n1,inputs1g7$alp1,inputs1g7$bet1,inputs1g7$u1,inputs1g7$l1,inputs1g7$u2,inputs1g7$l2)
resultsg7<-na.omit(cbind(inputs1g7,t(outputsg7)))

#-------------------ARL, SDRL for n=5----------------------------
arls<- by(resultsg7[,9], INDICES=resultsg7[,4], FUN=mean,  simplify=TRUE)
arlw<- by(resultsg7[,10], INDICES=resultsg7[,4], FUN=mean,  simplify=TRUE)
sdrls<- by(resultsg7[,9], INDICES=resultsg7[,4], FUN=sd,  simplify=TRUE)
sdrlw<- by(resultsg7[,10], INDICES=resultsg7[,4], FUN=sd,  simplify=TRUE)

arlg7<-cbind(arls,sdrls,arlw,sdrlw)
arlg7<-round(arlg7,2)
saveRDS(arlg7,file="arlg7.RDs")
readRDS("arlg7.RDs")


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
emg8 <- mapply(FUN = cntrlstat, inputsg$n, inputsg$alp, inputsg$bet)

tremg8 <- t(emg8)
emsg8 <- tremg8[, 1]
emwg8 <- tremg8[, 2]


#-------- Compute control limits n=10--------------
alpha <- 0.005

csg8 <-  as.vector(quantile(emsg8, c(alpha, 1-alpha)))
cwg8 <- as.vector(quantile(emwg8, c(alpha, 1- alpha)))

#--------Assign control limits n=10-----------

l1 <- csg8[1]
u1 <- csg8[2]
l2 <- cwg8[1]
u2 <- cwg8[2]


#-------------- Run length distribution n=10 --------------------------------
set.seed(123)
nsim1<-50000
alp1<-1.1
bet1<-c(1.5,1.75,2,2.25,2.5,3,3.5)
n1<-10


arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1g8<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputsg8<-mapply(FUN = sgnl, inputs1g8$n1,inputs1g8$alp1,inputs1g8$bet1,inputs1g8$u1,inputs1g8$l1,inputs1g8$u2,inputs1g8$l2)

resultsg8<-na.omit(cbind(inputs1g8,t(outputsg8)))


#-------------------ARL, SDRL for n=10----------------------------
arls <- by(resultsg8[,9], INDICES=resultsg8[,4], FUN=mean,  simplify=TRUE)
arlw <- by(resultsg8[,10], INDICES=resultsg8[,4], FUN=mean,  simplify=TRUE)
sdrls <- by(resultsg8[,9], INDICES=resultsg8[,4], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultsg8[,10], INDICES=resultsg8[,4], FUN=sd,  simplify=TRUE)

arlg8<-cbind(arls,sdrls,arlw,sdrlw)
arlg8<-round(arlg8,2)
saveRDS(arlg8,file="arlg8.RDs")
readRDS("arlg8.RDs")



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
emg9 <- mapply(FUN = cntrlstat, inputsg$n, inputsg$alp, inputsg$bet)

tremg9 <- t(emg9)
emsg9 <- tremg9[, 1]
emwg9 <- tremg9[, 2]


#-------- Compute control limits n=20--------------
alpha <- 0.005
csg9<-  as.vector(quantile(emsg9, c(alpha, 1-alpha)))
cwg9 <- as.vector(quantile(emwg9, c(alpha, 1- alpha)))

#--------Assign control limits n=20-----------
l1 <- csg9[1]
u1 <- csg9[2]
l2 <- cwg9[1]
u2 <- cwg9[2]



#-------------- Run length distribution--------------------------------
set.seed(123)
nsim1<-50000
alp1<-1.1
bet1<-c(1.5,1.75,2,2.25,2.5,3,3.5)
n1<-20

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1g9<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputsg9<-mapply(FUN = sgnl, inputs1g9$n1,inputs1g9$alp1,inputs1g9$bet1,inputs1g9$u1,inputs1g9$l1,inputs1g9$u2,inputs1g9$l2)

resultsg9<-na.omit(cbind(inputs1g9,t(outputsg9)))

#-------------------ARL, SDRL for n=20----------------------------

arls <- by(resultsg9[,9], INDICES=resultsg9[,4], FUN=mean,  simplify=TRUE)
arlw <- by(resultsg9[,10], INDICES=resultsg9[,4], FUN=mean,  simplify=TRUE)
sdrls <- by(resultsg9[,9], INDICES=resultsg9[,4], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultsg9[,10], INDICES=resultsg9[,4], FUN=sd,  simplify=TRUE)

arlg9<-cbind(arls,sdrls,arlw,sdrlw)
arlg9<-round(arlg9,2)
saveRDS(arlg9,file="arlg9.RDs")
readRDS("arlg9.RDs")


