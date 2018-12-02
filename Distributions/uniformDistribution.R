source("colorPalette.R")


##### unif2 Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dunif2 = function(x, alpha = 0, beta = 1)
{
    fx = rep(1/beta, length(x))
    return(fx)
}


### 누적분포함수
punif2 = function(x, alpha = 0, beta = 1)
{
    fx = (x - alpha)/beta
    return(fx)
}


### 생존함수
sunif2 = function (x, alpha = 0, beta = 1) 
{
    fx = 1 - (x - alpha)/beta
    return(fx)
}


### 위험함수
hunif2 = function (x, alpha = 0, beta = 1)
{
    fx = dunif2(x, alpha, beta) / sunif2(x, alpha, beta)
    return(fx)
}