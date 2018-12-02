source("colorPalette.R")


##### parabolicUshaped Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dparabolicUshaped = function(x, alpha = 0, beta = 1)
{
    fx = (3/(2*beta)) * ((x - alpha)/beta)^2
    return(fx)
}


### 누적분포함수
pparabolicUshaped = function(x, alpha = 0, beta = 1)
{
    fx = -(sparabolicUshaped(x, alpha = alpha, beta = beta) - 1)
    return(fx)
}


### 생존함수
sparabolicUshaped = function (x, alpha = 0, beta = 1) 
{
    fx = (1/2) * (1 - ((x-alpha)/beta)^3)
    return(fx)
}


### 위험함수
hparabolicUshaped = function (x, alpha = 0, beta = 1)
{
    fx = dparabolicUshaped(x, alpha, beta) / sparabolicUshaped(x, alpha, beta)
    return(fx)
}