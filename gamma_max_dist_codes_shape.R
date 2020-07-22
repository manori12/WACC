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

#------------Obtain the Empirical Distribution---------------------


nsim <-1e6
n <- 5
#n<-10
#n<-20
alp <- 2
bet <- 1

inputs <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
em <- mapply(FUN = cntrlstat, inputs$n, inputs$alp, inputs$bet)

trem <- t(em)
ems <- trem[, 1]
emw <- trem[, 2]

#-------- Compute control limits--------------
alpha <- 0.005
csg5 <-  as.vector(quantile(ems, c(alpha, 1-alpha)))
cwg5 <- as.vector(quantile(emw, c(alpha, 1- alpha)))

#csg10 <-  as.vector(quantile(ems, c(alpha, 1-alpha)))
#cwg10 <- as.vector(quantile(emw, c(alpha, 1- alpha)))

#csg20<-  as.vector(quantile(ems, c(alpha, 1-alpha)))
#cwg20 <- as.vector(quantile(emw, c(alpha, 1- alpha)))

#--------Assign control limits n=5-----------

l1 <- csg5[1]
u1 <- csg5[2]
l2 <- cwg5[1]
u2 <- cwg5[2]

#--------Assign control limits n=10-----------

#l1 <- csg10[1]
#u1 <- csg10[2]
#l2 <- cwg10[1]
#u2 <- cwg10[2]

#--------Assign control limits n=20-----------
#l1 <- csg20[1]
#u1 <- csg20[2]
#l2 <- cwg20[1]
#u2 <- cwg20[2]

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

#-------------- Run length distribution--------------------------------
nsim1<-50000
alp1<-c(2,2.25,2.5,2.75,3,3.5,4)
bet1<-1
n1<-5
#n1<-10
#n1<-20

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputs<-mapply(FUN = sgnl, inputs1$n1,inputs1$alp1,inputs1$bet1,inputs1$u1,inputs1$l1,inputs1$u2,inputs1$l2)

resultsg5sh<-na.omit(cbind(inputs1,t(outputs)))
#resultsg10sh<-na.omit(cbind(inputs1,t(outputs)))
#resultsg20sh<-na.omit(cbind(inputs1,t(outputs)))

#-------------------ARL, SDRL for n=5----------------------------
arls<- by(resultsg5sh[,9], INDICES=resultsg5sh[,3], FUN=mean,  simplify=TRUE)
arlw<- by(resultsg5sh[,10], INDICES=resultsg5sh[,3], FUN=mean,  simplify=TRUE)
sdrls<- by(resultsg5sh[,9], INDICES=resultsg5sh[,3], FUN=sd,  simplify=TRUE)
sdrlw<- by(resultsg5sh[,10], INDICES=resultsg5sh[,3], FUN=sd,  simplify=TRUE)

#arlg5sh<-cbind(arls,sdrls,arlw,sdrlw)
#arlg5sh<-round(arlg5sh,2)
#saveRDS(arlg5sh,file="arlg5sh.RDs")
#readRDS("arlg5sh.RDs")


#-------------------ARL, SDRL for n=10----------------------------
arls <- by(resultsg10sh[,9], INDICES=resultsg10sh[,3], FUN=mean,  simplify=TRUE)
arlw <- by(resultsg10sh[,10], INDICES=resultsg10sh[,3], FUN=mean,  simplify=TRUE)
sdrls <- by(resultsg10sh[,9], INDICES=resultsg10sh[,3], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultsg10sh[,10], INDICES=resultsg10sh[,3], FUN=sd,  simplify=TRUE)

arlg10sh<-cbind(arls,sdrls,arlw,sdrlw)
arlg10sh<-round(arlg10sh,2)
saveRDS(arlgsh10,file="arlg10sh.RDs")
readRDS("arlg10sh.RDs")


#-------------------ARL, SDRL for n=20----------------------------

arls <- by(resultsg20sh[,9], INDICES=resultsg20sh[,3], FUN=mean,  simplify=TRUE)
arlw <- by(resultsg20sh[,10], INDICES=resultsg20sh[,3], FUN=mean,  simplify=TRUE)
sdrls <- by(resultsg20sh[,9], INDICES=resultsg20sh[,3], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultsg20sh[,10], INDICES=resultsg20sh[,3], FUN=sd,  simplify=TRUE)

arlg20sh<-cbind(arls,sdrls,arlw,sdrlw)
arlg20sh<-round(arlg20sh,2)
saveRDS(arlg20sh,file="arlg20sh.RDs")
readRDS("arlg20sh.RDs")


