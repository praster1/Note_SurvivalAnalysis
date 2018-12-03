### 수명 분포
# dlnorm(x, meanlog = meanlog, sdlog = sdlog)


### 분위수 함수
# qlnorm(x, meanlog = meanlog, sdlog = sdlog)


### 난수 함수
# rlnorm(x, meanlog = meanlog, sdlog = sdlog)


### 누적분포함수
# plnorm(x, meanlog = meanlog, sdlog = sdlog)


### 생존함수
slnorm = function (x, meanlog = 0, sdlog = 1)
{
    fx = 1 - plnorm(x, meanlog = meanlog, sdlog = sdlog)
    return(fx)
}


### 위험함수
hlnorm = function (x, meanlog = 0, sdlog = 1)
{
    fx = dlnorm(x, meanlog = meanlog, sdlog = sdlog) / slnorm(x, meanlog = meanlog, sdlog = sdlog)
    return(fx)
}