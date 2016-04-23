# this is an example that generates x(t) values conditional on:
# a known time series y(t), t=t_0,..., T.
# parameters a(t), b(t), c(t)
# relation matrix R

# x(t) = a(t)*R * X(t-1) + b(t)*y(t) + c(t)*y(t-1) + error_xt

# x(t) = [R * X(t-1) , b*y(t) ,  c(t)* y(t-1)] [a(t)] +  [error_xt]
#                                              [b(t)]    
#                                              [c(t)]

# let theta(t) = (a(t), b(t), c(t))'

# theta(t) = theta(t-1) + error_theta

rm(list=ls())
set.seed(330)
# number of occupations
n = 200
# number of years
TT = 15

# a function to generate a sparse symmetric matrix, 
# where the diagonal and a few off diagonals are 1
fitll.off.diagonal <- function(n, k){
  # n is the dimension of the matrix
  # k is the number of off diagonals to fill with 1
  R = diag(rep(1,n))
  for (j in 1:k){
    R[row(R) == (col(R) - j)] = 1
    R[row(R) == (col(R) + j)] = 1
  }
  return(R)
}
# construct R as diagonal matrix with 3 off diagonals = 1
R = fitll.off.diagonal(n,3)

a = b = c = y = c()
# parameter values
a[1] = .01
b[1] = 0.01
c[1] = -0.01
# y is a vector of positive numbers representing total number of open positions

y[1] = 5
for (t in 2:TT){
  a[t] = rnorm(1, a[t-1], .01)
  b[t] = rnorm(1, b[t-1], .01)
  c[t] = rnorm(1, c[t-1], .01)
  y[t] = rnorm(1, y[t-1],.01)
} 

theta = matrix(c(a,b,c), 3, TT, byrow=T)

# standard deviation of (iid) observational errors
sigma = .01

# x is a vector probabilities that sums to 1
x = matrix(rnorm(n, 100, 1), n, 1)
x = x/sum(x)

Xmat = matrix(0, n, TT)

Xmat[, 1] = x

for (t in 2:TT){
  xt_1 = Xmat[, t-1]
  yt = y[t]
  yt_1 = y[t-1]
  Fmat = cbind(R %*% matrix(xt_1, n, 1), matrix(yt, n, 1), matrix(yt_1, n, 1))
  meanX = .2 + Fmat %*% theta[, t] + rnorm(n, 0, sigma)
  Xmat[, t] = meanX
}

par(mfrow=c(3,1))
# raw x values (don't sum to 1)
plot(1:TT, Xmat[1,], type='l', xlab = 'year', ylim = c(-.1, .5),
     main='raw proportion of open positions')
apply(Xmat, 1, function(x) lines(1:TT, x))
a
# normalize the x's to sum to 1 at each time step
Xmat_normalize = apply(Xmat, 2, function(x) x/sum(x))

plot(1:TT, Xmat_normalize[1,], type='l', xlab = 'year', ylim = c(0, .01),
     main='normalized proportion of open positions')
apply(Xmat_normalize, 1, function(x) lines(1:TT, x))

yX = data.frame(rbind(y,Xmat_normalize), row.names = NULL)
names(yX) = gsub("X", "t", names(yX))

plot.ts(y, main='number in millions of open positions')

write.csv(yX, "yX_simulated.csv", row.names=FALSE)

