
source("colorPalette.R")
require(extraDistr)


##### Birnbaum-Saunders Distribution
### parameter
alpha = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 1, length.out = 1000)


### 수명 분포
dfatigue(x, alpha, beta = 1, mu = 0)


### 분위수 함수
qfatigue(x, alpha, beta, mu = 0)


### 난수 함수
rfatigue(x, alpha, beta, mu = 0)


### 누적분포함수
pfatigue(x, alpha, beta, mu = 0)


### 생존함수
sfatigue = function (x, alpha = 1, beta = 0) 
{
    fx = 1 - pfatigue(x, alpha, beta)
    return(fx)
}


### 위험함수
hfatigue = function (x, alpha = 1, beta = 0)
{
    fx = dfatigue(x, alpha, beta) / sfatigue(x, alpha, beta)
    return(fx)
}