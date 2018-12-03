# https://rdrr.io/cran/shotGroups/man/maxwell.html
### 수명 분포
dmaxwellboltzmann = function(x, alpha = 1, beta = 1)
{
    fx = (1/beta) * sqrt(2/pi) * (((x - alpha)/beta)^2) * exp(-(1/2)*(((x - alpha)/beta)^2))
    return(fx)
}


### 누적분포함수
pmaxwellboltzmann = function(x, alpha = 1, beta = 1)
{
    fx = -(smaxwellboltzmann(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
smaxwellboltzmann = function (x, alpha = 1, beta = 0) 
{
    #fx = 1 - (2/pi) * atan(exp((x - alpha)/beta))
    return(fx)
}


### 위험함수
hmaxwellboltzmann = function (x, alpha = 1, beta = 0)
{
    fx = dmaxwellboltzmann(x, alpha, beta) / smaxwellboltzmann(x, alpha, beta)
    return(fx)
}
