# -------- Obtain the Control Statistic-------------------
set.seed(123)
cntrlstat <- function(n, mu, sig)
{
  x <- rlnorm(n, mu, sig)
  refr <- max(x) # Set minimum, maximum,  mean, median etc as the reference point
  w <- abs(x - refr) / sum(abs(x - refr))
  y <- w * x
  ybar <- mean(y)
  xbar <- mean(x)
  return(cbind(xbar, ybar))
}

#------------Obtain the Empirical Distribution---------------------
nsim <-1e6
n<-c(5,10,20)
mu <- 0
sig <- 1

inputs <- expand.grid(
  nsim = 1:nsim,
  n = n,
  mu = mu,
  sig = sig
)
em<- mapply(FUN = cntrlstat, inputs$n, inputs$mu, inputs$sig)
emdis<-na.omit(cbind(inputs,t(em)))

emdis1<-split.data.frame(emdis,emdis[,2])
emdis5<-data.frame(emdis1[1])
emdis10<-data.frame(emdis1[2])
emdis20<-data.frame(emdis1[3])

ems5 <- emdis5[, 5]
emw5 <- emdis5[, 6]
ems10 <- emdis10[, 5]
emw10 <- emdis10[, 6]
ems20 <- emdis20[, 5]
emw20 <- emdis20[, 6]



#-------- Compute control limits--------------
alpha <- 0.005
cs5 <-  as.vector(quantile(ems5, c(alpha, 1-alpha)))
cw5 <- as.vector(quantile(emw5, c(alpha, 1- alpha)))
cs10 <-  as.vector(quantile(ems10, c(alpha, 1-alpha)))
cw10 <- as.vector(quantile(emw10, c(alpha, 1- alpha)))
cs20 <-  as.vector(quantile(ems20, c(alpha, 1-alpha)))
cw20 <- as.vector(quantile(emw20, c(alpha, 1- alpha)))


#-------------Function Signal------------------------
sgnl<-function(n,mu,sig,u1,l1,u2,l2)
{
  x.RL<-0
  y.RL<-0
  mn<-cntrlstat(n=n,mu=mu,sig=sig)
  while( l1 < mn[1]  && mn[1] < u1 )
  {
    x.RL<-x.RL+1
    mn<-cntrlstat(n=n,mu=mu,sig=sig)
    
  }
  mn<-cntrlstat(n=n,mu=mu,sig=sig)
  while( l2 < mn[2]  && mn[2] < u2 )
  {
    y.RL<-y.RL+1
    mn<-cntrlstat(n=n,mu=mu,sig=sig)
    
  }
  return(cbind(x.RL,y.RL))
}  


#--------Assign control limits n=5-----------
l1 <- cs5[1]
u1 <- cs5[2]
l2 <- cw5[1]
u2 <- cw5[2]

#-------------- Run length distribution--------------------------------
set.seed(123)
mu1<-c(-1.5,-1.0,-0.75,-0.5,-0.25,0,0.2,0.5,0.75,1.0,1.5)
sig1<-c(1,1.1,1.2,1.5,1.75,2,2.5,3.0)
nsim1<-50000
n1<-5

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0

inputs1<-expand.grid(nsim=1:nsim1,n=n1,mu=mu1,sig=sig1,u1=u1,l1=l1,u2=u2,l2=l2)
outputs<-mapply(FUN = sgnl, inputs1$n,inputs1$mu,inputs1$sig,inputs1$u1,inputs1$l1,inputs1$u2,inputs1$l2)
results5bo<-na.omit(cbind(inputs1,t(outputs)))


#-------------------ARL, SDRL for n=5----------------------------
arls <- by(results5bo[,9], INDICES=results5bo[,3:4], FUN =mean,  simplify=TRUE)
arlw <- by(results5bo[,10], INDICES=results5bo[,3:4], FUN=mean,  simplify=TRUE)
sdrls <- by(results5bo[,9], INDICES=results5bo[,3:4], FUN=sd,  simplify=TRUE)
sdrlw <- by(results5bo[,10], INDICES=results5bo[,3:4], FUN=sd,  simplify=TRUE)


arl5bo<-cbind(arls,sdrls,arlw,sdrlw)
arl5bo<-round(arl5bo,2)

saveRDS(arl5bo,file="arlln5bo.RDs")
readRDS("arlln5bo.RDs")


#--------Assign control limits n=10-----------
l1 <- cs10[1]
u1 <- cs10[2]
l2 <- cw10[1]
u2 <- cw10[2]

#-------------- Run length distribution--------------------------------
set.seed(123)
mu1<-c(-1.5,-1.0,-0.75,-0.5,-0.25,0,0.2,0.5,0.75,1.0,1.5)
sig1<-c(1,1.1,1.2,1.5,1.75,2)
nsim1<-50000

n1<-10


arls<-0
arlw<-0
sdrls<-0
sdrlw<-0

inputs1<-expand.grid(nsim=1:nsim1,n=n1,mu=mu1,sig=sig1,u1=u1,l1=l1,u2=u2,l2=l2)
outputs<-mapply(FUN = sgnl, inputs1$n,inputs1$mu,inputs1$sig,inputs1$u1,inputs1$l1,inputs1$u2,inputs1$l2)

results10<-na.omit(cbind(inputs1,t(outputs)))

#-------------------ARL, SDRL for n=10----------------------------
arls <- by(results10[,9], INDICES=results10[,3:4], FUN=mean,  simplify=TRUE)
arlw <- by(results10[,10], INDICES=results10[,3:4], FUN=mean,  simplify=TRUE)
sdrls <- by(results10[,9], INDICES=results10[,3:4], FUN=sd,  simplify=TRUE)
sdrlw <- by(results10[,10], INDICES=results10[,3:4], FUN=sd,  simplify=TRUE)

arl10bo<-cbind(arls,sdrls,arlw,sdrlw)
arl10bo<-round(arl10bo,2)
saveRDS(arl10bo,file="arlln10bo.RDs")
readRDS("arlln10bo.RDs")


#--------Assign control limits n=20-----------
l1 <- cs20[1]
u1 <- cs20[2]
l2 <- cw20[1]
u2 <- cw20[2]



#-------------- Run length distribution--------------------------------
set.seed(123)
mu1<-c(-1.5,-1.0,-0.75,-0.5,-0.25,0,0.2,0.5,0.75,1.0,1.5)
sig1<-c(1,1.1,1.2,1.5,1.75,2)
nsim1<-50000
n1<-20

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0

inputs1<-expand.grid(nsim=1:nsim1,n=n1,mu=mu1,sig=sig1,u1=u1,l1=l1,u2=u2,l2=l2)
outputs<-mapply(FUN = sgnl, inputs1$n,inputs1$mu,inputs1$sig,inputs1$u1,inputs1$l1,inputs1$u2,inputs1$l2)

results20<-na.omit(cbind(inputs1,t(outputs)))



#-------------------ARL, SDRL for n=20----------------------------

arls <- by(results20[,9], INDICES=results20[,3:4], FUN=mean,  simplify=TRUE)
arlw <- by(results20[,10], INDICES=results20[,3:4], FUN=mean,  simplify=TRUE)
sdrls <- by(results20[,9], INDICES=results20[,3:4], FUN=sd,  simplify=TRUE)
sdrlw <- by(results20[,10], INDICES=results20[,3:4], FUN=sd,  simplify=TRUE)

arl20<-cbind(arls,sdrls,arlw,sdrlw)
arl20<-round(arl20,2)
saveRDS(arl20,file="arlln20lo.RDs")
readRDS("arlln20lo.RDs")



