source("colorPalette.R")
require(rmutil)


##### Hjorth Distribution
### parameter
delta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)	# m (delta)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)		# s (beta)
theta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)	# f (theta)

### input varialbe
x = seq(0.1, 1, length.out = 1000)


### 수명 분포
dhjorth(x, m = 1, s = 1, f = 1)


### 분위수 함수
qhjorth(x, m = 1, s = 1, f = 1)


### 난수 함수
rhjorth(x, m = 1, s = 1, f = 1)


### 누적분포함수
phjorth(x, m = 1, s = 1, f = 1)


### 생존함수
shjorth = function (x, m = 1, s = 1, f=1)
{
    fx = 1 - phjorth(x, m = m, s = m, f = f)
    return(fx)
}


### 위험함수
hhjorth = function (x, m = 1, s = 1, f=1)
{
    fx = dhjorth(x, m = m, s = m, f = f) / shjorth(x, m = m, s = m, f = f)
    return(fx)
}