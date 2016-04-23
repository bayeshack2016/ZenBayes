# TO DO: Fix for loop! Different Fmat is needed at every time step! 
# currently all use the first one!
# load needed libraries
library(dlm)
library(mvtnorm)
rm(list=ls())
data = read.csv("yX_simulated.csv", row.names=NULL)

y = unlist(data[1,])
X = data[2:nrow(data),]
N = nrow(X)
TT = length(y)

if (sum(which(X==0)) == 0){
  print("No zeros in X. Taking the log is safe!")
}

# a function to generate a sparse symmetric matrix, 
# where the diagonal and a few off diagonals are 1
fitll.off.diagonal <- function(N, k){
  # n is the dimension of the matrix
  # k is the number of off diagonals to fill with 1
  rho = diag(rep(1,N))
  for (j in 1:k){
    rho[row(rho) == (col(rho) - j)] = 1
    rho[row(rho) == (col(rho) + j)] = 1
  }
  return(rho)
}
# construct R as diagonal matrix with 3 off diagonals = 1
rho = fitll.off.diagonal(N,3)

par(mfrow=c(2,1))
plot(1:TT, X[1,], type='l', xlab = 'year', ylim = c(0, .02),
     main='prop of open positions (sum to 1 every year)', ylab='X')
apply(X, 1, function(w) lines(1:TT, w))
plot.ts(y, main ='# of available position (m)')


### setup
xt_1  <-  X[, 1]
yt  <-  y[1]
yt_1  <-  yt * .97
Fmat  <-  t(cbind(matrix(1, N, 1), rho %*% matrix(xt_1, N, 1), matrix(yt, N, 1), 
                matrix(yt_1, N, 1)))
# number of paramters in theta
q <- nrow(Fmat)
# means of theta(t-1) and theta(t)
a <- m <- array(NA,c(q,TT))
# covariances of theta(t-1) and theta(t)
R <- C <- array(NA,c(q,q,TT))
# mean and residuals of y(t) and covariances
ft <- et <- Cdiag <- matrix(0,nrow=N,ncol=TT)
# matrix for C
Qt=array(NA,c(N,N,TT))
s=numeric(TT)
n=numeric(TT)
d=numeric(TT)


# Specification of initial states
m0 = rep(.01,q)
C0 = diag(.01,q,q)
n0 = 1
s0 = 1e-5
d0 = n0*s0
# discount factors
delta=.99

####################################################
################# Filtering ########################
####################################################
Sigma <- diag(N)
G <- diag(q)
a[,1] <- G%*%m0
R[,,1] <- G%*%C0%*%t(G)/delta
ft[,1] <- t(Fmat)%*%a[,1]
Qt[,,1] <- t(Fmat)%*%R[,,1]%*%Fmat+s0*Sigma
et[,1] <- X[,1]-ft[,1]
At <- R[,,1]%*%Fmat%*%solve(Qt[,,1])
n[1] <- n0+N
d[1] <- d0+s0*t(et[,1])%*%(Qt[,,1])%*%et[,1]
s[1] <- d[1]/n[1]
m[,1] <- a[,1]+At%*%et[,1]
C[,,1] <- s[1]*(R[,,1]-At%*%Qt[,,1]%*%t(At))/s0
Cdiag[,1] <- diag(C[,,1])

for(t in 2:TT){
  xt_1  <-  X[,t-1]
  yt  <-  y[t]
  yt_1  <-  y[t-1] 
  Fmat  <-  t(cbind(matrix(1, N, 1), rho %*% matrix(xt_1, N, 1), matrix(yt, N, 1), 
                    matrix(yt_1, N, 1)))
  a[,t] <- G%*%m[,t-1]
  R[,,t] <- G%*%C[,,t-1]%*%t(G)/delta
  ft[,t] <- t(Fmat)%*%a[,t]
  Qt[,,t] <- t(Fmat)%*%R[,,t]%*%Fmat+s[t-1]*Sigma
  Qt[,,t] <- (Qt[,,t]+t(Qt[,,t]))/2
  et[,t] <- X[,t]-ft[,t]
  At <- R[,,t]%*%Fmat%*%solve(Qt[,,t])
  n[t] <- n[t-1]+N
  d[t] <- d[t-1]+s[t-1]*t(et[,t])%*%(Qt[,,t])%*%et[,t]
  s[t] <- d[t]/n[t]
  m[,t] <- a[,t]+At%*%et[,t]
  C[,,t] <- s[t]*(R[,,t]-At%*%Qt[,,t]%*%t(At))/s[t-1]
  Cdiag[,t] <- diag(C[,,t])
  print(t)
}


par(mfrow=c(1,1))
plot(1:TT, X[1,], type='l', xlab = 'year', ylim = c(0, .02),
     main='prop of open positions (sum to 1 every year)', ylab='X')
apply(X, 1, function(w) lines(1:TT, w))

apply(ft[,2:TT], 1, function(w) lines(2:TT, w,col='blue'))

plot.ts(c(X[1,]), ylim=c(-.1,.1))
lines(c(m[1,]), col='blue')
lines(c(m[1,]) + c(Cdiag[1,])^(.5), col='blue')
lines(c(m[1,]) - c(Cdiag[1,])^(.5), col='blue')


### Forecasting

k=2
ak=matrix(0,q,k)
Rk=array(NA, c(q,q,k))
fk=matrix(0,N,k)
Qk=array(NA, c(N,N,k))

ak[,1]=m[,TT]
Rk[,,1]=C[,,TT]
fk[,1]=t(Fmat)%*%ak[,1]
Qk[,,1]=t(Fmat)%*%Rk[,,1]%*%Fmat+s[TT]

for(i in 2:k){
  ak[,i]=G%*%ak[,i-1]
  Rk[,,i]=G%*%Rk[,,i-1]%*%t(G)/delta
  fk[,i]=t(Fmat)%*%ak[,i]
  Qk[,,i]=t(Fmat)%*%Rk[,,i]%*%Fmat+s[TT]
}


