source("colorPalette.R")
require(rmutil)


##### laplace Distribution
### parameter
m = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
s = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dlaplace(x, m = 0, s = 1)


### 분위수 함수
qlaplace(x, m = 0, s = 1)


### 난수 함수
rlaplace(x, m = 0, s = 1)


### 누적분포함수
plaplace(x, m = 0, s = 1)


### 생존함수
slaplace = function(x, m = 0, s = 1)
{
    fx = 1 - plaplace(x, m = m, s = s)
    return(fx)
}


### 위험함수
hlaplace = function(x, m = 0, s = 1)
{
    fx = dlaplace(x, m = m, s = s) / slaplace(x, m = m, s = s)
    return(fx)
}