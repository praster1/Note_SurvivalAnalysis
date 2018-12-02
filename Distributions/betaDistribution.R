source("colorPalette.R")


##### Beta Distribution
### parameter
alpha = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 1, length.out = 1000)


### 수명 분포
dbeta(x, alpha, beta)


### 분위수 함수
qbeta(x, alpha, beta)


### 난수 함수
rbeta(x, alpha, beta)


### 누적분포함수
pbeta(x, alpha, beta)


### 생존함수
sbeta = function (x, shape1 = 1, shape2 = 0) 
{
    fx = 1 - pbeta(x, shape1=shape1, shape2=shape2)
    return(fx)
}


### 위험함수
hbeta = function (x, shape1 = 1, shape2 = 0)
{
    fx = dbeta(x, shape1=shape1, shape2=shape2) / sbeta(x, shape1=shape1, shape2=shape2)
    return(fx)
}
