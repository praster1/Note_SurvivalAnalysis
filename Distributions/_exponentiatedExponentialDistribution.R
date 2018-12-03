##### Not Completed 
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