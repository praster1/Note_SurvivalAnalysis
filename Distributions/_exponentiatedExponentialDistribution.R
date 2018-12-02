##### Not Completed 


source("colorPalette.R")


##### exponentiatedExponential Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
gamma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dexponentiatedExponential = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (gamma/beta) * exp(-((x - alpha) / beta)) * (1 - exp(((x - alpha) / beta)))^(gamma-1)
    return(fx)
}


### 누적분포함수
pexponentiatedExponential = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = -(sexponentiatedExponential(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
sexponentiatedExponential = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = 1 - (1 - exp(((x - alpha) / beta)))^gamma
    return(fx)
}


### 위험함수
hexponentiatedExponential = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = dexponentiatedExponential(x, alpha, beta, gamma) / sexponentiatedExponential(x, alpha, beta, gamma)
    return(fx)
}