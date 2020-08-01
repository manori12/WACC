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
alp <- 0.5
bet <- 1

inputsg <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
emg13 <- mapply(FUN = cntrlstat, inputsg$n, inputsg$alp, inputsg$bet)

tremg13 <- t(emg13)
emsg13 <- tremg13[, 1]
emwg13 <- tremg13[, 2]


#-------- Compute control limits n=5--------------
alpha <- 0.005
csg13 <-  as.vector(quantile(emsg13, c(alpha, 1-alpha)))
cwg13 <- as.vector(quantile(emwg13, c(alpha, 1- alpha)))


#--------Assign control limits n=5-----------

l1 <- csg13[1]
u1 <- csg13[2]
l2 <- cwg13[1]
u2 <- cwg13[2]

#-------------- Run length distribution n=5--------------------------------
set.seed(123)
nsim1<-50000
alp1<-0.5
bet1 <-c(1,1.25,1.5,1.75,2,2.5)
n1<-5

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1g13<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputsg13<-mapply(FUN = sgnl, inputs1g13$n1,inputs1g13$alp1,inputs1g13$bet1,inputs1g13$u1,inputs1g13$l1,inputs1g13$u2,inputs1g13$l2)
resultsg13<-na.omit(cbind(inputs1g13,t(outputsg13)))

#-------------------ARL, SDRL for n=5----------------------------
arls<- by(resultsg13[,9], INDICES=resultsg13[,4], FUN=mean,  simplify=TRUE)
arlw<- by(resultsg13[,10], INDICES=resultsg13[,4], FUN=mean,  simplify=TRUE)
sdrls<- by(resultsg13[,9], INDICES=resultsg13[,4], FUN=sd,  simplify=TRUE)
sdrlw<- by(resultsg13[,10], INDICES=resultsg13[,4], FUN=sd,  simplify=TRUE)

arlg13<-cbind(arls,sdrls,arlw,sdrlw)
arlg13<-round(arlg13,2)
saveRDS(arlg13,file="arlg13.RDs")
readRDS("arlg13.RDs")


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
emg14 <- mapply(FUN = cntrlstat, inputsg$n, inputsg$alp, inputsg$bet)

tremg14 <- t(emg14)
emsg14 <- tremg14[, 1]
emwg14 <- tremg14[, 2]


#-------- Compute control limits n=10--------------
alpha <- 0.005

csg14 <-  as.vector(quantile(emsg14, c(alpha, 1-alpha)))
cwg14 <- as.vector(quantile(emwg14, c(alpha, 1- alpha)))

#--------Assign control limits n=10-----------

l1 <- csg14[1]
u1 <- csg14[2]
l2 <- cwg14[1]
u2 <- cwg14[2]


#-------------- Run length distribution n=10 --------------------------------
set.seed(123)
nsim1<-50000
alp1<-0.5
bet1 <-c(1,1.25,1.5,1.75,2,2.5)
n1<-10


arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1g14<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputsg14<-mapply(FUN = sgnl, inputs1g14$n1,inputs1g14$alp1,inputs1g14$bet1,inputs1g14$u1,inputs1g14$l1,inputs1g14$u2,inputs1g14$l2)

resultsg14<-na.omit(cbind(inputs1g14,t(outputsg14)))


#-------------------ARL, SDRL for n=10----------------------------
arls <- by(resultsg14[,9], INDICES=resultsg14[,4], FUN=mean,  simplify=TRUE)
arlw <- by(resultsg14[,10], INDICES=resultsg14[,4], FUN=mean,  simplify=TRUE)
sdrls <- by(resultsg14[,9], INDICES=resultsg14[,4], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultsg14[,10], INDICES=resultsg14[,4], FUN=sd,  simplify=TRUE)

arlg14<-cbind(arls,sdrls,arlw,sdrlw)
arlg14<-round(arlg14,2)
saveRDS(arlg14,file="arlg14.RDs")
readRDS("arlg14.RDs")



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
emg15 <- mapply(FUN = cntrlstat, inputsg$n, inputsg$alp, inputsg$bet)

tremg15 <- t(emg15)
emsg15 <- tremg15[, 1]
emwg15 <- tremg15[, 2]


#-------- Compute control limits n=20--------------
alpha <- 0.005
csg15<-  as.vector(quantile(emsg15, c(alpha, 1-alpha)))
cwg15 <- as.vector(quantile(emwg15, c(alpha, 1- alpha)))

#--------Assign control limits n=20-----------
l1 <- csg15[1]
u1 <- csg15[2]
l2 <- cwg15[1]
u2 <- cwg15[2]



#-------------- Run length distribution--------------------------------
set.seed(123)
nsim1<-50000
alp1<-0.5
bet1 <-c(1,1.25,1.5,1.75,2,2.5)
n1<-20

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1g15<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputsg15<-mapply(FUN = sgnl, inputs1g15$n1,inputs1g15$alp1,inputs1g15$bet1,inputs1g15$u1,inputs1g15$l1,inputs1g15$u2,inputs1g15$l2)

resultsg15<-na.omit(cbind(inputs1g15,t(outputsg15)))

#-------------------ARL, SDRL for n=20----------------------------

arls <- by(resultsg15[,9], INDICES=resultsg15[,4], FUN=mean,  simplify=TRUE)
arlw <- by(resultsg15[,10], INDICES=resultsg15[,4], FUN=mean,  simplify=TRUE)
sdrls <- by(resultsg15[,9], INDICES=resultsg15[,4], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultsg15[,10], INDICES=resultsg15[,4], FUN=sd,  simplify=TRUE)

arlg15<-cbind(arls,sdrls,arlw,sdrlw)
arlg15<-round(arlg15,2)
saveRDS(arlg15,file="arlg15.RDs")
readRDS("arlg15.RDs")


