# -------- Obtain the Control Statistic-------------------
set.seed(123)
cntrlstat <- function(n, alp, bet)
{
  x <- rllogis(n, shape = alp,rate= 1/bet ,  scale = bet)
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
alp <- 1.7
bet <- 1

inputllog <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)

emllog7 <- mapply(FUN = cntrlstat, inputllog$n, inputllog$alp, inputllog$bet)
emllog7<-na.omit(cbind(inputllog,t(emllog7)))


emsllog7 <- emllog7[, 5]
emwllog7 <- emllog7[, 6]


#-------- Compute control limits n=5--------------

alpha <- 0.005
csllog7 <-  as.vector(quantile(emsllog7, c(alpha, 1-alpha)))
cwllog7 <- as.vector(quantile(emwllog7, c(alpha, 1- alpha)))


#--------Assign control limits n=5-----------

l1 <- csllog7[1]
u1 <- csllog7[2]
l2 <- cwllog7[1]
u2 <- cwllog7[2]

#-------------- Run length distribution n=5--------------------------------
set.seed(123)
nsim1<-50000
alp1<-c(1.1,1.2,1.3,1.4,1.5,1.6,1.7)
bet1 <-c(1,1.25,1.5,1.75,2,2.5,3)
n1<-5

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1llog7<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputsllog7<-mapply(FUN = sgnl, inputs1llog7$n1,inputs1llog7$alp1,inputs1llog7$bet1,inputs1llog7$u1,inputs1llog7$l1,inputs1llog7$u2,inputs1llog7$l2)
resultsllog7<-na.omit(cbind(inputs1llog7,t(outputsllog7)))

#-------------------ARL, SDRL for n=5----------------------------
arls<- by(resultsllog7[,9], INDICES=resultsllog7[,3:4], FUN=mean,  simplify=TRUE)
arlw<- by(resultsllog7[,10], INDICES=resultsllog7[,3:4], FUN=mean,  simplify=TRUE)
sdrls<- by(resultsllog7[,9], INDICES=resultsllog7[,3:4], FUN=sd,  simplify=TRUE)
sdrlw<- by(resultsllog7[,10], INDICES=resultsllog7[,3:4], FUN=sd,  simplify=TRUE)

arlllog7<-cbind(arls,sdrls,arlw,sdrlw)
arlllog7<-round(arlllog7,2)
saveRDS(arlllog7,file="arlllog7.RDs")
readRDS("arlllog7.RDs")


#---------------- n=10 -------------------------------
#------------Obtain the Empirical Distribution n=10---------------------
set.seed(123)
nsim <-1e6
n <- 10
alp <- 1.7
bet <- 1

inputsllog <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
emllog8<- mapply(FUN = cntrlstat, inputsllog$n, inputsllog$alp, inputsllog$bet)

tremllog8<- t(emllog8)
emsllog8<- tremllog8[, 1]
emwllog8<- tremllog8[, 2]


#-------- Compute control limits n=10--------------
alpha <- 0.005

csllog8<-  as.vector(quantile(emsllog8, c(alpha, 1-alpha)))
cwllog8<- as.vector(quantile(emwllog8, c(alpha, 1- alpha)))

#--------Assign control limits n=10-----------

l1 <- csllog8[1]
u1 <- csllog8[2]
l2 <- cwllog8[1]
u2 <- cwllog8[2]


#-------------- Run length distribution n=10 --------------------------------
set.seed(123)
nsim1<-50000
alp1<-c(1.1,1.2,1.3,1.4,1.5,1.6,1.7)
bet1 <-c(1,1.25,1.5,2,2.5,3)
n1<-10


arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1llog8<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputsllog8<-mapply(FUN = sgnl, inputs1llog8$n1,inputs1llog8$alp1,inputs1llog8$bet1,inputs1llog8$u1,inputs1llog8$l1,inputs1llog8$u2,inputs1llog8$l2)

resultsllog8<-na.omit(cbind(inputs1llog8,t(outputsllog8)))


#-------------------ARL, SDRL for n=10----------------------------
arls <- by(resultsllog8[,9], INDICES=resultsllog8[,3:4], FUN=mean,  simplify=TRUE)
arlw <- by(resultsllog8[,10], INDICES=resultsllog8[,3:4], FUN=mean,  simplify=TRUE)
sdrls <- by(resultsllog8[,9], INDICES=resultsllog8[,3:4], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultsllog8[,10], INDICES=resultsllog8[,3:4], FUN=sd,  simplify=TRUE)

arlllog8<-cbind(arls,sdrls,arlw,sdrlw)
arlllog8<-round(arlllog8,2)
saveRDS(arlllog8,file="arlllog8.RDs")
readRDS("arlllog8.RDs")



#-------------------------n=20 ------------------
#------------Obtain the Empirical Distribution n=20---------------------
set.seed(123)
nsim <-1e6
n <- 20
alp <- 1.7
bet <- 1

inputsllog <- expand.grid(
  nsim = 1:nsim,
  n = n,
  alp = alp,
  bet = bet
)
emllog9 <- mapply(FUN = cntrlstat, inputsllog$n, inputsllog$alp, inputsllog$bet)

tremllog9 <- t(emllog9)
emsllog9 <- tremllog9[, 1]
emwllog9 <- tremllog9[, 2]


#-------- Compute control limits n=20--------------
alpha <- 0.005
csllog9<-  as.vector(quantile(emsllog9, c(alpha, 1-alpha)))
cwllog9 <- as.vector(quantile(emwllog9, c(alpha, 1- alpha)))

#--------Assign control limits n=20-----------
l1 <- csllog9[1]
u1 <- csllog9[2]
l2 <- cwllog9[1]
u2 <- cwllog9[2]



#-------------- Run length distribution--------------------------------
set.seed(123)
nsim1<-50000
bet1 <-c(1,1.25,1.5,2,2.5,3)
alp1<-c(1.1,1.2,1.3,1.4,1.5,1.6,1.7)
n1<-20

arls<-0
arlw<-0
sdrls<-0
sdrlw<-0


inputs1llog9<-expand.grid(nsim=1:nsim1,n1=n1,alp1=alp1,bet1=bet1,u1=u1,l1=l1,u2=u2,l2=l2)
outputsllog9<-mapply(FUN = sgnl, inputs1llog9$n1,inputs1llog9$alp1,inputs1llog9$bet1,inputs1llog9$u1,inputs1llog9$l1,inputs1llog9$u2,inputs1llog9$l2)

resultsllog9<-na.omit(cbind(inputs1llog9,t(outputsllog9)))

#-------------------ARL, SDRL for n=20----------------------------

arls <- by(resultsllog9[,9], INDICES=resultsllog9[,3:4], FUN=mean,  simplify=TRUE)
arlw <- by(resultsllog9[,10], INDICES=resultsllog9[,3:4], FUN=mean,  simplify=TRUE)
sdrls <- by(resultsllog9[,9], INDICES=resultsllog9[,3:4], FUN=sd,  simplify=TRUE)
sdrlw <- by(resultsllog9[,10], INDICES=resultsllog9[,3:4], FUN=sd,  simplify=TRUE)

arlllog9<-cbind(arls,sdrls,arlw,sdrlw)
arlllog9<-round(arlllog9,2)
saveRDS(arlllog9,file="arlllog9.RDs")
readRDS("arlllog9.RDs")


