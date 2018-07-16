
runif(10,0,1) #ten pseudo random numbers between 0 and 1
runif(10) #same as above
runif(10,2,5) # between 2 and 5

## Estimate theta = int_0^1 e^x dx
## Note: theta = E(e^U), U~unif(0,1)
mean(exp(runif(10000)))

set.seed(2)
n=10000
u=runif(n)
#head(u)
x=exp(u)
theta.hat=mean(x)
theta.hat

## Estimate theta = int_0^1 e^(x^2) dx
## Note: theta = E(e^(U^2)), U~unif(0,1)
n=10000
u=runif(n)
x=exp(u^2)
theta.hat=mean(x)
theta.hat

## Estimate theta = int_-2^3 e^(x^2) dx
## Note: theta = 5*E(e^(U^2)), U~unif(-2,3)
n=10000
u=runif(n, -2,3)
x=exp(u^2)
theta.hat=5*mean(x)
theta.hat

## Estimate theta = int_0^1 1/y^2 e^-(1/y-1)^2 dy
## Note: theta = E(e^(U^2)), U~unif(0,1)
n=10000
u=runif(n)
x=exp(-(1/u-1)^2)/u^2
theta.hat=mean(x)
theta.hat

## Estimate theta = P(X<Y), where X ~ exp(rate=2), Y ~ exp(rate=3), independent

set.seed(53)
n = 5
x = rexp(n,2)
y = rexp(n,3)

ind = rep(0,n)
for (i in 1:n){
  if (x[i] <y[i])
    ind[i]=1
}
ind
# ind without the for loop
ind = x < y
sum(ind)

set.seed(5)
n = 10000
x = rexp(n,2)
y = rexp(n,3)
ind = x < y
theta.hat=sum(ind)/n
theta.hat

### Generate Discrete Random Variable

# generate one x
set.seed(3)
u=runif(1)
if (u <.2){
   x = 1
} else if (u<0.35){
  x = 2
} else if (u<.6){
  x = 3
} else {
  x = 4
}
# generate many x's

n=10000
x=rep(0,n)
for (i in 1:n){
  u=runif(1)
  if (u <.2){
    x[i] = 1
  } else if (u<0.35){
    x[i] = 2
  } else if (u<.6){
    x[i] = 3
  } else {
    x[i] = 4
  }
}
x[1:20]
table(x)/n

# a bit more efficient

n=10000
x=rep(0,n)
for (i in 1:n){
  u=runif(1)
  if (u <.4){
    x[i] = 4
  } else if (u<0.65){
    x[i] = 3
  } else if (u<.85){
    x[i] = 1
  } else {
    x[i] = 2
  }
}
table(x)/n

# generating discrete uniform (1,2,...,m)

m=5
n=10000
#floor(4.6)
x=rep(0,n)
for (i in 1:n){
  u=runif(1)
  x[i]=floor(m*u)+1
}
x[1:20]
table(x)/n

#w/o loop

u=runif(n)
x=floor(m*u)+1
table(x)/n

## Generating geometric random variable with p = .3

n=10000
p=.3

u=runif(n)
x=floor(log(u)/log(1-p))+1

table(x)/n

y=rgeom(n,p)
table(y)/n

# checking
mean(x) # compare with 1/p
1/p

# or
k=10
prob=rep(0,k)
for (i in 1:k){
  prob[i]=(1-p)^(i-1)*p
}
prob

## Example: Rolling a die 50 times
n=50
k=6
u=runif(n)
x=floor(u*k)+1
x[1:20]
table(x)/n

## Estimate the number of rolls needed until all faces shown

# one N

k=6
counter = 0
record = rep(0,k)

while (sum(record) < 6){
  roll = floor(k*runif(1)) + 1
  record[roll] = 1
  counter = counter + 1
}

N = counter

# 10,000 Ns

n=10000
N = rep(0,n)
k=6

for (i in 1:n){
  counter = 0
  record = rep(0,k)
  
  while (sum(record) < 6){
    roll = floor(k*runif(1)) + 1
    record[roll] = 1
    counter = counter + 1
  }
  
  N[i]= counter
}
mean(N)

## Generate Poisson RV
## one x
j=0
lam=2
F1=exp(-lam)
p=exp(-lam)
U=runif(1)

while (U >= F1){
  p=lam/(j+1)*p
  F1=F1+p
  j=j+1
}

x = j

# many xs

n=10000
x=rep(0,n)
lam=2

for (i in 1:n){
  j=0
  F1=exp(-lam)
  p=exp(-lam)
  U=runif(1)
  
  while (U >= F1){
    p=lam/(j+1)*p
    F1=F1+p
    j=j+1
  }
  x[i] = j
}
table(x)/n
mean(x)
hist(x)
var(x)


## binomial

n=10000
x=rep(0,n)
p=.3
m=10

for (i in 1:n){
  j=0
  F1=pr=(1-p)^m
  U=runif(1)
  
  while (U >= F1){
    pr=(p/(1-p))*((m-j)/(j+1))*pr
    F1=F1+pr
    j=j+1
  }
  x[i] = j
}
hist(x)
mean(x)
var(x)


## AR-method

## Example 

k = 10
p = c(.11,.12,.09,.08,.12,.1,.09,.09,.1,.1)
q = rep(1/k,k)
c = max(p/q)

n=100000
x=rep(0,n)

for (i in 1:n){
  reject = T
  while(reject){
    u=runif(1)
    y=floor(k*runif(1)) + 1
    if (u <= p[y]/(c*q[y]))
      reject = F
  }
  x[i]=y
}
table(x)/n

## Generate gamma(1.5,1) random variable

r_c = function(x){
  f.x=sqrt(2*exp(1)/3)*x^0.5*exp(-x/3)
  f.x
}

n=100000
x=rep(0,n)

for (i in 1:n){
  reject = T
  while(reject){
    u=runif(1)
    y = -3*log(runif(1))/2
    r_c1 = r_c(y)
    if (u <= r_c1)  
      reject = F
  }
  x[i]=y
}
hist(x, freq = F)

w = seq(from = 0, to = 6, by = .01)
f.w = 2*sqrt(w)*exp(-w)/sqrt(pi)
lines(w,f.w, type = 'l', col = "red")

y=rgamma(n,3/2,1)
hist(y, freq = F)

## Generate normal random variable

r_c = function(x){
  f.x=exp(-.5*(x-1)^2)
  f.x
}

n=100000
x=rep(0,n)

for (i in 1:n){
  reject = T
  while(reject){
    u=runif(1)
    y = -log(runif(1))
    r_c1 = r_c(y)
    if (u <= r_c1)  
      reject = F
  }
  w = runif(1)
  if (w < 0.5){
    x[i]=y
  } else {
    x[i]=-y
  }
}
hist(x, freq = F)

w = seq(from = -4, to = 4, by = .01)
f.w = exp(-.5*w^2)/sqrt(2*pi)
lines(w,f.w, type = 'l', col = "green")

z = rnorm(n,0,1)
hist(z, freq = F)

## estimate theta = \int_0^1 e^x dx
## theta = E(e^U^2) where U ~ unif(0,1)
# 20 theta.hat
k = 20
n = 10000
theta.hat = numeric(k)
for (i in 1:k){
  u = runif(n)
  x = exp(u)
  theta.hat[i] = mean(x)
}
theta.hat
mean(theta.hat)
var(theta.hat)

## negative corr. method
# one theta.hat

n = 10000
u = runif(1)
x = exp(u)
y = exp(1-u)
w = (x+y)/2
theta.hat.neg = mean(w)
theta.hat.neg

# 20 theta.hat

k = 20
n = 10000
theta.hat.neg = rep(0,k)
for (i in 1:k){
  u = runif(n)
  x = exp(u)
  y = exp(1-u)
  w = (x+y)/2
  theta.hat.neg[i] = mean(w)
}
theta.hat.neg
mean(theta.hat.neg)
var(theta.hat.neg) # less than half of before